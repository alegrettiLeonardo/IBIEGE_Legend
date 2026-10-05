from __future__ import annotations
from .numerical_audit import excitation_orders, harmonic_label

from dataclasses import dataclass, field
from datetime import datetime
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess
import struct
import sys
import tempfile
from typing import Iterable

from .domain import Project, ShaftElementKind, ShaftSegment
from .solver_disks import build_solver_disks
from .solver_runtime import solver_process_creationflags


class SolverError(RuntimeError):
    pass


MAX_RESPONSE_SPEED_POINTS = 1000


@dataclass(slots=True)
class SolverSection:
    end_m: float
    diameter_m: float
    inner_diameter_m: float
    final_diameter_m: float
    final_inner_diameter_m: float
    element_type: ShaftElementKind
    young_pa: float
    poisson: float
    density_kg_m3: float
    divisions: int = 0
    # Persisted compatibility field name. The physical quantity is distributed
    # electromagnetic stiffness k' [N/m²], not a line force/stiffness in N/m.
    ump_n_m: float = 0.0
    yield_strength_pa: float = 0.0

    @property
    def ump_stiffness_n_m2(self) -> float:
        """Distributed UMP electromagnetic stiffness k' [N/m²]."""
        return self.ump_n_m

    @ump_stiffness_n_m2.setter
    def ump_stiffness_n_m2(self, value: float) -> None:
        self.ump_n_m = float(value)
    # RotorDin/GYCOF: corrige o termo giroscópico quando a seção da UI
    # precisa ser representada por um diâmetro circular equivalente.
    gyroscopic_factor: float = 1.0


@dataclass(slots=True)
class BearingParameterPayload:
    bearing_index: int
    path: Path
    kind: int  # 1 = equation coefficients (COEF), 2 = speed table (TABLE)
    content: str


@dataclass(slots=True)
class CriticalPoint:
    rpm: float
    hz: float
    harmonic: str = "1.0x"
    kind: str = "campbell_crossing"


@dataclass(slots=True)
class SolverResult:
    command: list[str]
    returncode: int
    stdout: str
    stderr: str
    input_text: str
    run_dir: Path
    sections: dict[str, str] = field(default_factory=dict)
    critical_speeds: list[CriticalPoint] = field(default_factory=list)
    commands: list[list[str]] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return self.returncode == 0

    @property
    def display_critical_speeds(self) -> list[CriticalPoint]:
        """Pontos para a grade legada rpm/Hz.

        A grade histórica não possui coluna de harmônico. Prioriza os
        cruzamentos 1x; se o solver não reportar 1x na faixa, mantém os demais
        cruzamentos para não esconder um resultado válido.
        """
        one_x = [point for point in self.critical_speeds if point.harmonic == "1.0x"]
        return one_x or self.critical_speeds


_SECTION_START = re.compile(r"^#BEGIN\s+(.+?)\s*$")
_SECTION_END = re.compile(r"^#END\s+(.+?)\s*$")


def _timeout_output_text(data: str | bytes | None) -> str:
    """Normalize TimeoutExpired captured output to text.

    Python may expose TimeoutExpired.stdout/stderr as bytes even when the
    subprocess was launched with text=True. Keep timeout evidence writable and
    preserve undecodable bytes with replacement characters instead of masking
    the real timeout with a TypeError.
    """
    if data is None:
        return ""
    if isinstance(data, bytes):
        return data.decode("utf-8", errors="replace")
    return data


def parse_marked_sections(text: str) -> dict[str, str]:
    """Extrai os blocos #BEGIN/#END emitidos pelo RotorDin em -std."""
    result: dict[str, list[str]] = {}
    current: str | None = None
    for raw in text.splitlines():
        if raw.strip().startswith('RD_AUDIT_V1 '):
            result.setdefault('rdaudit.out', []).append(raw)
            continue
        m = _SECTION_START.match(raw.strip())
        if m:
            current = m.group(1)
            result.setdefault(current, [])
            continue
        m = _SECTION_END.match(raw.strip())
        if m:
            current = None
            continue
        if current is not None:
            result[current].append(raw)
    return {name: "\n".join(lines).rstrip() + "\n" for name, lines in result.items()}


def parse_critical_speeds(campbell_text: str) -> list[CriticalPoint]:
    """Extrai cruzamentos 1.0x/2.0x/0.5x do relatório Campbell.

    Algumas configurações simétricas não apresentam cruzamento 1.0x dentro da
    faixa, mas podem apresentar 2.0x/0.5x. O frontend antigo exibe rpm/Hz sem
    impor uma física própria; por isso preservamos todos os cruzamentos não
    nulos reportados pelo solver e carregamos o harmônico como metadado.
    """
    lines = campbell_text.splitlines()
    native_metadata = any(line.strip().startswith('EXCITATION_ORDERS ') for line in lines)
    harmonics = tuple(harmonic_label(order) for order in excitation_orders(lines))

    def collect_matrix(title: str) -> list[list[float]]:
        for i, line in enumerate(lines):
            if title.lower() not in line.lower():
                continue
            rows: list[list[float]] = []
            for row in lines[i + 2:]:
                text = row.strip()
                if not text:
                    break
                values: list[float] = []
                tokens = text.split()
                if native_metadata and len(tokens) != len(harmonics):
                    raise ValueError('Tabela de cruzamentos com colunas inconsistentes')
                for token in tokens[:len(harmonics)]:
                    try:
                        values.append(float(token.replace("D", "E").replace("d", "e")))
                    except ValueError:
                        values = []
                        break
                if not values:
                    break
                while len(values) < len(harmonics):
                    values.append(0.0)
                rows.append(values)
            return rows
        return []

    rpm_rows = collect_matrix("Critical Speeds (rpm)")
    hz_rows = collect_matrix("Critical Frequencies (Hz)")
    points: list[CriticalPoint] = []
    for row_idx in range(min(len(rpm_rows), len(hz_rows))):
        for col, harmonic in enumerate(harmonics):
            rpm = rpm_rows[row_idx][col]
            hz = hz_rows[row_idx][col]
            if rpm > 0.0 and hz > 0.0:
                points.append(CriticalPoint(rpm=rpm, hz=hz, harmonic=harmonic))
    points.sort(key=lambda point: (point.rpm, point.harmonic))
    return points


def _fmt(v: float) -> str:
    if abs(v) >= 1e5 or (0 < abs(v) < 1e-4):
        return f"{v:.10E}"
    return f"{v:.10g}"


def _f10(v: float) -> str:
    """Campo real de 10 colunas, compatível com FORMAT(F10.0)."""
    v = float(v)
    s = f"{v:10.4E}" if v >= 0 else f"{v:10.3E}"
    if len(s) > 10:
        s = f"{v:10.3E}"
    if len(s) > 10:
        raise SolverError(f"Valor não cabe em F10.0: {v}")
    return s


def _axial_f10_m(value: float) -> str:
    """Serialize an axial SI position with micrometre-level fixed precision.

    RotorDin reads axial coordinates with F10.0.  The generic scientific formatter
    can move mandatory stations by tens of micrometres for values such as
    1.504875 m.  Prefer F10.6 when it fits and fall back to the generic formatter
    only for unusually large coordinates.
    """
    value = float(value)
    fixed = f"{value:10.6f}"
    return fixed if len(fixed) <= 10 else _f10(value)


def _real32(value: float) -> float:
    """Round exactly as the native default REAL used by RotorDin."""
    try:
        return struct.unpack("=f", struct.pack("=f", float(value)))[0]
    except (OverflowError, struct.error) as exc:
        raise SolverError(f"Valor não representável em REAL nativo: {value}") from exc


def _native_response_point_count(
    initial_field: str,
    final_field: str,
    step_field: str,
) -> int:
    """Emulate the native REAL32 rpm=rpm+ndr response loop."""
    rpm = _real32(float(initial_field))
    final = _real32(float(final_field))
    step = _real32(float(step_field))
    if step <= 0.0:
        return 0
    limit = _real32(final + _real32(1.0e-6))
    count = 0
    while rpm <= limit:
        count += 1
        if count > MAX_RESPONSE_SPEED_POINTS + 1:
            return count
        rpm = _real32(rpm + step)
    return count


def _legacy_response_step_field(
    project: Project,
    initial_rpm: float,
    final_rpm: float,
    step_rpm: float,
) -> str:
    """Preserve imported legacy response-grid cardinality at native precision."""
    default = _f10(step_rpm)
    provenance = project.settings.get("legacy_response_grid")
    if not isinstance(provenance, dict):
        return default
    if provenance.get("source_semantics") != "legacy_unbfd_division_parameter":
        return default

    try:
        effective_i = float(provenance["effective_initial_rpm"])
        effective_f = float(provenance["effective_final_rpm"])
        converted = float(provenance["converted_step_rpm"])
        target_points = int(provenance["number_of_points"])
    except (KeyError, TypeError, ValueError):
        return default

    if not (
        math.isclose(initial_rpm, effective_i, rel_tol=1.0e-12, abs_tol=1.0e-9)
        and math.isclose(final_rpm, effective_f, rel_tol=1.0e-12, abs_tol=1.0e-9)
        and math.isclose(step_rpm, converted, rel_tol=1.0e-12, abs_tol=1.0e-9)
    ):
        return default
    if target_points <= 0 or target_points > MAX_RESPONSE_SPEED_POINTS:
        return default

    initial_field = _f10(initial_rpm)
    final_field = _f10(final_rpm)
    if _native_response_point_count(initial_field, final_field, default) == target_points:
        return default

    represented = float(default)
    if represented <= 0.0:
        return default
    exponent = math.floor(math.log10(abs(represented)))
    quantum = 10.0 ** (exponent - 4)

    matches: set[str] = set()
    for offset in range(-64, 65):
        candidate = represented + offset * quantum
        if candidate <= 0.0:
            continue
        token = _f10(candidate)
        if _native_response_point_count(initial_field, final_field, token) == target_points:
            matches.add(token)

    if not matches:
        raise SolverError(
            "Resposta legada: a precisão fixa F10.0/REAL do RotorDin não consegue "
            f"preservar os {target_points} pontos da grade importada "
            f"({initial_rpm:g}–{final_rpm:g} rpm, UNBFD={step_rpm:.15g})."
        )

    return min(
        matches,
        key=lambda token: (
            abs(float(token) - step_rpm),
            0 if float(token) <= step_rpm else 1,
            float(token),
        ),
    )

def _i10(v: int) -> str:
    s = f"{int(v):10d}"
    if len(s) > 10:
        raise SolverError(f"Inteiro não cabe em I10: {v}")
    return s


def _fw(*fields: str) -> str:
    return "".join(fields)


@dataclass(frozen=True, slots=True)
class RibbedEquivalentProperties:
    equivalent_outer_diameter_mm: float
    inner_diameter_mm: float
    real_area_m2: float
    equivalent_area_m2: float
    real_second_moment_m4: float
    equivalent_second_moment_m4: float
    original_density_kg_m3: float
    equivalent_density_kg_m3: float
    gyroscopic_factor: float

    @property
    def real_area_mm2(self) -> float:
        return self.real_area_m2 * 1.0e6

    @property
    def equivalent_area_mm2(self) -> float:
        return self.equivalent_area_m2 * 1.0e6

    @property
    def real_second_moment_mm4(self) -> float:
        return self.real_second_moment_m4 * 1.0e12


def ribbed_equivalent_properties(seg: ShaftSegment, density: float) -> RibbedEquivalentProperties:
    """Single engineering authority for ribbed-section solver equivalence.

    D_eq preserves lateral second moment with the original D_INT, rho_eq preserves
    mass per unit length, and RotorDin GYCOF=A_eq/A_real preserves the current
    native gyroscopic rho*I contract. No torsional equivalence is asserted here.
    """
    if not seg.is_ribbed:
        d = seg.diameter_mm / 1000.0
        di = seg.inner_diameter_mm / 1000.0 if seg.is_hollow else 0.0
        area = math.pi * (d**2 - di**2) / 4.0
        inertia = math.pi * (d**4 - di**4) / 64.0
        return RibbedEquivalentProperties(seg.diameter_mm, di * 1000.0, area, area, inertia, inertia, density, density, 1.0)

    d = seg.diameter_mm / 1000.0
    di = seg.inner_diameter_mm / 1000.0
    dp = seg.package_diameter_mm / 1000.0
    ba = seg.a_mm / 1000.0
    hf = seg.b_mm / 1000.0
    bf = seg.c_mm / 1000.0
    zz = seg.rib_count
    ha = max((dp - d) / 2.0 - hf, 0.0)
    ra = (ha + d) / 2.0
    rf = ra + (ha + hf) / 2.0
    i_ribs = zz / 2.0 * (
        ba * ha**3 / 12.0 + ha * ba**3 / 12.0 + ra**2 * ha * ba
        + bf * hf**3 / 12.0 + hf * bf**3 / 12.0 + rf**2 * hf * bf
    )
    i_shaft = math.pi * (d**4 - di**4) / 64.0
    i_actual = i_shaft + i_ribs
    deq = (di**4 + 64.0 * i_actual / math.pi) ** 0.25
    area_real = math.pi * (d**2 - di**2) / 4.0 + zz * (ba * ha + bf * hf)
    area_eq = math.pi * (deq**2 - di**2) / 4.0
    if area_real <= 0.0 or area_eq <= 0.0:
        raise SolverError("Geometria costelada inválida para cálculo do diâmetro equivalente.")
    rho_eq = density * area_real / area_eq
    gycof = area_eq / area_real
    if not (0.0 < gycof <= 5.0):
        raise SolverError(
            f"GYCOF={gycof:.6g} da seção costelada está fora do intervalo aceito pelo RotorDin (0, 5]."
        )
    i_eq = math.pi * (deq**4 - di**4) / 64.0
    return RibbedEquivalentProperties(
        deq * 1000.0, di * 1000.0, area_real, area_eq, i_actual, i_eq,
        density, rho_eq, gycof,
    )


def _ribbed_equivalent(seg: ShaftSegment, density: float) -> tuple[float, float, float]:
    """Backward-compatible tuple facade over ribbed_equivalent_properties."""
    props = ribbed_equivalent_properties(seg, density)
    return props.equivalent_outer_diameter_mm, props.equivalent_density_kg_m3, props.gyroscopic_factor


def _segment_index_and_ratio(project: Project, x_mm: float) -> tuple[int, ShaftSegment, float, float]:
    x0 = 0.0
    for idx, seg in enumerate(project.segments):
        x1 = x0 + seg.length_mm
        if x_mm <= x1 + 1e-9:
            ratio = 0.0 if seg.length_mm == 0 else max(0.0, min(1.0, (x_mm - x0) / seg.length_mm))
            return idx, seg, ratio, x0
        x0 = x1
    idx = len(project.segments) - 1
    return idx, project.segments[-1], 1.0, x0 - project.segments[-1].length_mm


def _element_type_code(kind: ShaftElementKind) -> int:
    return {
        ShaftElementKind.SOLID_CYLINDRICAL: 1,
        ShaftElementKind.SOLID_CONICAL: 2,
        ShaftElementKind.HOLLOW_CYLINDRICAL: 3,
        ShaftElementKind.HOLLOW_CONICAL: 4,
    }[kind]


def build_solver_sections(project: Project) -> list[SolverSection]:
    """Converte os segmentos da UI em ``SECTIONS`` nativas do RotorDin (SI).

    Trechos cônicos não são mais discretizados pelo Python em vários degraus. O
    frontend envia diâmetros inicial/final e o tipo do elemento; o Fortran faz a
    interpolação linear das geometrias de cada elemento finito. ``conical_subdivisions``
    continua controlando a discretização, agora como ``DIV`` nativo do trecho.
    """
    cfg = project.configuration
    cuts: set[float] = {0.0, project.shaft_length_mm}
    x = 0.0
    for seg in project.segments:
        cuts.add(x)
        x += seg.length_mm
        cuts.add(x)

    # UMP precisa coincidir com fronteiras de seção porque é constante por seção.
    for mass in project.masses:
        if mass.ump:
            cuts.add(mass.xi_mm)
            cuts.add(mass.xi_mm + mass.length_mm)

    pts = sorted(p for p in cuts if 0.0 <= p <= project.shaft_length_mm)
    sections: list[SolverSection] = []
    for xa, xb in zip(pts, pts[1:]):
        if xb - xa <= 1e-9:
            continue
        xm = (xa + xb) / 2.0
        _, seg, _, seg_x0 = _segment_index_and_ratio(project, xm)
        r0 = max(0.0, min(1.0, (xa - seg_x0) / seg.length_mm))
        r1 = max(0.0, min(1.0, (xb - seg_x0) / seg.length_mm))

        kind = seg.resolved_element_type
        outer0 = seg.diameter_mm + (seg.effective_final_diameter_mm - seg.diameter_mm) * r0
        outer1 = seg.diameter_mm + (seg.effective_final_diameter_mm - seg.diameter_mm) * r1
        inner_start = seg.inner_diameter_mm if seg.is_hollow else 0.0
        inner_end = seg.effective_final_inner_diameter_mm if seg.is_hollow else 0.0
        inner0 = inner_start + (inner_end - inner_start) * r0
        inner1 = inner_start + (inner_end - inner_start) * r1

        if seg.is_ribbed:
            props = ribbed_equivalent_properties(seg, cfg.density_kg_m3)
            outer0 = outer1 = props.equivalent_outer_diameter_mm
            rho_eq = props.equivalent_density_kg_m3
            gycof = props.gyroscopic_factor
            if seg.is_hollow:
                inner0 = inner1 = seg.inner_diameter_mm
                kind = ShaftElementKind.HOLLOW_CYLINDRICAL
            else:
                inner0 = inner1 = 0.0
                kind = ShaftElementKind.SOLID_CYLINDRICAL
        else:
            rho_eq = cfg.density_kg_m3
            gycof = 1.0

        # Para uma seção cônica dividida apenas por UMP, distribui o total de
        # subdivisões proporcionalmente ao comprimento físico do subtrecho.
        divisions = 0
        if kind in (ShaftElementKind.SOLID_CONICAL, ShaftElementKind.HOLLOW_CONICAL):
            fraction = (xb - xa) / seg.length_mm
            divisions = max(1, int(round(cfg.conical_subdivisions * fraction)))

        ump = 0.0
        for mass in project.masses:
            if mass.ump and mass.xi_mm - 1e-9 <= xm <= mass.xi_mm + mass.length_mm + 1e-9:
                ump += mass.ump_stiffness_n_m2

        sections.append(
            SolverSection(
                end_m=xb / 1000.0,
                diameter_m=outer0 / 1000.0,
                inner_diameter_m=inner0 / 1000.0,
                final_diameter_m=outer1 / 1000.0,
                final_inner_diameter_m=inner1 / 1000.0,
                element_type=kind,
                young_pa=cfg.young_pa,
                poisson=cfg.poisson,
                density_kg_m3=rho_eq,
                divisions=divisions,
                ump_n_m=ump,
                yield_strength_pa=cfg.yield_strength_pa,
                gyroscopic_factor=gycof,
            )
        )
    if len(sections) > 99:
        raise SolverError(f"A conversão geométrica gerou {len(sections)} seções; o RotorDin aceita no máximo 99.")
    return sections


def _bearing_parameter_payloads(project: Project) -> list[BearingParameterPayload]:
    """Carrega os arquivos ``Arq`` dos mancais para o protocolo ``-std``.

    O RotorDin possui um único ``KIND`` por execução: 1 para arquivos COEF e
    2 para arquivos TABLE. Em ``-std`` os conteúdos dos arquivos são enviados
    inline, na mesma ordem dos mancais listados em BEAFILES. Portanto o
    frontend apenas lê/valida o contrato textual e não interpola nem calcula
    coeficientes de mancal.
    """
    payloads: list[BearingParameterPayload] = []
    kinds: set[int] = set()
    for index, bearing in enumerate(project.bearings, start=1):
        name = bearing.file.strip()
        if not name:
            continue
        path = Path(name).expanduser()
        if not path.is_file():
            raise SolverError(f"Arquivo de parâmetros do mancal #{index} não encontrado: {path}")
        try:
            raw = path.read_text(encoding="utf-8-sig")
        except UnicodeDecodeError:
            raw = path.read_text(encoding="latin-1")
        lines = raw.replace("\r\n", "\n").replace("\r", "\n").splitlines()
        first = next((line.strip().upper() for line in lines if line.strip()), "")
        if first.startswith("COEF"):
            kind = 1
        elif first.startswith("TABLE"):
            kind = 2
        else:
            raise SolverError(
                f"Arquivo de mancal #{index} inválido: a primeira linha útil deve iniciar por COEF ou TABLE ({path})."
            )
        # Em stdin o leitor usa a linha EOF para encerrar cada bloco antes de
        # prosseguir para o próximo mancal/SUPPORT. Acrescentamos somente se o
        # arquivo físico não a possui.
        while lines and not lines[-1].strip():
            lines.pop()
        if not lines or lines[-1].strip().upper() != "EOF":
            lines.append("EOF")
        content = "\n".join(lines) + "\n"
        payloads.append(BearingParameterPayload(index, path, kind, content))
        kinds.add(kind)
    if len(kinds) > 1:
        raise SolverError(
            "O RotorDin aceita um único KIND de parâmetros de mancais por execução; não misture arquivos COEF e TABLE."
        )
    return payloads


class SolverInputWriter:
    """Serializador do contrato textual do RotorDin.

    A UI usa mm; o arquivo é emitido integralmente em SI e respeita os
    FORMATs fixos de ``entrada.f``.  Não há física rotodinâmica aqui: este
    módulo só traduz o modelo da interface para o contrato do solver.
    """

    def render(self, project: Project) -> str:
        project.validate()
        cfg = project.configuration
        sections = build_solver_sections(project)
        bearing_payloads = _bearing_parameter_payloads(project)
        ident = project.identification
        length_m = project.shaft_length_mm / 1000.0
        rpm_i = ident.rpm_initial or 0.0
        rpm_f = ident.rpm_final or max(ident.rpm_nominal, 6000.0)
        if rpm_f <= rpm_i:
            if project.analyses.campbell or project.analyses.log_decrement:
                raise SolverError(
                    f"Campbell: faixa inválida {rpm_i:g}–{rpm_f:g} rpm; "
                    "esperado fim > início."
                )
            rpm_f = rpm_i + max(cfg.campbell_step_rpm, 1.0)
        # SMSPD é a rotação em que o mapa -a avalia coeficientes de mancal.
        # Quando a UI usa Arq/TABLE/COEF, 0 rpm não é válido e o valor deve
        # cair dentro da faixa de análise. Sem campo SMSPD visível no iRdin
        # legado, usamos rpm n quando ele já está na faixa; caso contrário rpm i.
        if cfg.map_speed_rpm > 0:
            map_speed_rpm = cfg.map_speed_rpm
        elif bearing_payloads:
            map_speed_rpm = max(ident.rpm_nominal, ident.rpm_initial)
        else:
            map_speed_rpm = ident.rpm_nominal
        # Response sweep is an analysis-specific contract.  Do not alias it
        # to the Campbell range stored historically in identification.rpm_*.
        resp_i = cfg.response_initial_rpm
        resp_f = cfg.response_final_rpm
        if resp_f <= resp_i:
            if project.analyses.dynamic_response:
                raise SolverError(
                    f"Resposta em frequência: faixa inválida "
                    f"{resp_i:g}–{resp_f:g} rpm; esperado fim > início."
                )
            resp_f = resp_i + cfg.response_step_rpm
        if project.analyses.dynamic_response:
            response_start = max(resp_i, 1.0e-9)
            response_points = (
                math.floor(
                    (resp_f - response_start) / cfg.response_step_rpm + 1.0e-12
                )
                + 1
            )
            if response_points > MAX_RESPONSE_SPEED_POINTS:
                raise SolverError(
                    "Resposta em frequência: "
                    f"faixa {resp_i:g}–{resp_f:g} rpm com passo "
                    f"{cfg.response_step_rpm:g} rpm gera {response_points} pontos; "
                    f"o RotorDin aceita no máximo {MAX_RESPONSE_SPEED_POINTS}. "
                    "Aumente o passo de resposta ou reduza a faixa de rotação."
                )

        user = (ident.user or ident.reference or "ROTORDIN")[:12]
        description = (ident.description or self._default_description(project))[:50]

        lines: list[str] = []
        emit = lines.append
        emit("NAME")
        emit(user)
        emit("")
        emit("DESCRIPTION")
        emit(description)
        emit("")
        emit("SECTIONS  LENGTH    DIV       MXDIV     LDR       HANGLE    ACCG      HANUN     SHMFWD")
        emit(
            _fw(
                _i10(len(sections)), _f10(length_m), _i10(cfg.min_divisions),
                _i10(cfg.max_divisions), _f10(cfg.load_distribution),
                _f10(cfg.horizontal_angle_deg), _f10(cfg.gravity_m_s2),
            )
            + f"{(cfg.horizontal_angle_unit[:1] or 'D'):>10}"
            + f" {cfg.section_filter_forward},{cfg.section_filter_backward}"
        )
        emit("")
        emit("POSITION  EXTDIAM   INTDIAM   YOUNG     POISSON   DENSITY   DIV       UMP       YSTR      ETYPE     EXTDIAM2  INTDIAM2")
        for sec in sections:
            # Os primeiros 80 caracteres preservam integralmente o contrato legado.
            # O tail opcional começa por YSTR (campo já existente) e acrescenta
            # ETYPE/diâmetros finais; solvers antigos simplesmente ignoram o excedente.
            emit(
                _fw(
                    _f10(sec.end_m), _f10(sec.diameter_m), _f10(sec.inner_diameter_m),
                    _f10(sec.young_pa), _f10(sec.poisson), _f10(sec.density_kg_m3),
                    _i10(sec.divisions), _f10(sec.ump_stiffness_n_m2),
                    _f10(sec.yield_strength_pa), _i10(_element_type_code(sec.element_type)),
                    _f10(sec.final_diameter_m), _f10(sec.final_inner_diameter_m),
                )
            )

        emit("")
        emit("DISK      NRAT      GYCOF")
        # entrada.f: FORMAT(//,I10,A) seguido de FORMAT(F10.0,A). GYCOF usa
        # pares ``section-factor`` separados por vírgula e aceita até 5 pares.
        gycof_pairs = [
            (idx, sec.gyroscopic_factor)
            for idx, sec in enumerate(sections, start=1)
            if abs(sec.gyroscopic_factor - 1.0) > 1e-10
        ]
        if len(gycof_pairs) > 5:
            raise SolverError(
                f"A conversão de seções equivalentes requer {len(gycof_pairs)} fatores GYCOF; "
                "o RotorDin aceita no máximo 5."
            )
        gycof_text = ",".join(f"{idx}-{factor:.6g}" for idx, factor in gycof_pairs)
        if ident.rpm_nominal or gycof_text:
            disk_tail = _f10(ident.rpm_nominal or 0.0) + gycof_text
        else:
            disk_tail = ""

        # One canonical adapter resolves physical bores, offsets and package
        # expansion before any DISK record reaches the fixed-width protocol.
        disks = build_solver_disks(project)
        emit(_i10(len(disks)) + disk_tail)
        emit("")
        emit("POSITION  DIAMETER  LENGTH    DENSITY   INTDIAM   IX        IY        WEIGHT    OFFSET")
        for disk in disks:
            emit(
                _fw(
                    _axial_f10_m(disk.position_mm / 1000.0),
                    _f10(disk.outer_diameter_mm / 1000.0),
                    _f10(disk.length_mm / 1000.0),
                    # Explicit-WEIGHT mode: keep DENSITY zero so WEIGHT=0 is
                    # never interpreted as an implicit geometry/density mode.
                    _f10(0.0),
                    _f10(disk.inner_diameter_mm / 1000.0),
                    # Keep predad.f authoritative for disk inertia. The Python
                    # contract computes the same Ix/Iy for UI/parity evidence,
                    # but zero here deliberately requests the native calculation.
                    _f10(0.0),
                    _f10(0.0),
                    _f10(disk.weight_kg),
                    _f10(disk.offset_mm / 1000.0),
                )
            )

        emit("")
        emit("BEARING   SCALE     RKS       FXSTF")
        # Esta linha é lida como texto e depois via list-directed READ.
        emit(
            f"{len(project.bearings)} {_fmt(cfg.bearing_scale)} "
            f"{_fmt(cfg.static_bearing_stiffness_ratio)} {_fmt(cfg.fixed_static_stiffness_n_m)}"
        )
        emit("")
        emit("POSITION  KXX       KXZ       KZZ       KZX       CXX       CXZ       CZZ       CZX       WEIGTH    KPH       KTH")
        for bearing in project.bearings:
            emit(
                _fw(
                    _f10(bearing.position_mm / 1000.0), _f10(bearing.kxx), _f10(bearing.kxz),
                    _f10(bearing.kzz), _f10(bearing.kzx), _f10(bearing.cxx),
                    _f10(bearing.cxz), _f10(bearing.czz), _f10(bearing.czx),
                    _f10(bearing.mass_kg),
                )
                + (_fw(_f10(bearing.kphi), _f10(bearing.ktheta)) if bearing.kphi or bearing.ktheta else "")
            )

        emit("")
        emit("CPBIS     CPBFS     CPBSPD    NCRSPD    INTSPD    SMIST     SMNDC     SMSPD     IMDTRK")
        emit(
            _fw(
                _f10(rpm_i), _f10(rpm_f), _f10(cfg.campbell_step_rpm),
                _i10(cfg.critical_speed_count), _i10(cfg.interpolation_points),
                _f10(cfg.map_stiffness_initial), _i10(cfg.map_number_curves),
                _f10(map_speed_rpm),
            )
            + (_i10(int(cfg.mode_tracking)) if cfg.mode_tracking else "")
        )

        emit("")
        emit("EXCT      UNBIF     UNBFF     UNBFD     UNBMD     MINAMP    MDRPM     UANUN")
        emit(
            _fw(
                _i10(len(project.forces)), _f10(resp_i), _f10(resp_f),
                (
                    _legacy_response_step_field(
                        project, resp_i, resp_f, cfg.response_step_rpm
                    )
                    if project.analyses.dynamic_response
                    else _f10(cfg.response_step_rpm)
                ),
                _i10(cfg.response_modes),
            )
            + _fw(_f10(cfg.minimum_amplification), _f10(cfg.modal_rpm))
            # HarmonicForce.phase_deg is always stored in degrees. Emit the
            # native Fortran angle flag accordingly, independent of stale project
            # metadata inherited from older frontend revisions.
            + f"{'D':>10}"
        )
        emit("")
        emit("POSITION  VALUE     PHASE     KIND      THFR/OFFSET")
        for force in project.forces:
            # KIND=5 recebe índice da seção; demais tipos recebem posição SI.
            position = force.position_mm if int(force.kind) == 5 else force.position_mm / 1000.0
            emit(
                _fw(_f10(position), _f10(force.value), _f10(force.phase_deg), _i10(int(force.kind)))
                + (_f10(force.extra) if force.extra else "")
            )

        emit("")
        emit("FRESPPOS  NBRMOD    TIMOEP    TIMSPD    TANGLE    RANUN     TAUN")
        emit(
            _fw(
                _i10(len(project.probes)), _i10(cfg.output_modes),
                _f10(cfg.orbit_position_mm / 1000.0), _f10(cfg.orbit_speed_rpm),
                _f10(cfg.orbit_angle_deg),
            )
            # ResponseProbe.orientation_deg and orbit_angle_deg are degrees.
            # RANUN therefore must advertise degrees at the solver boundary.
            + f"{'D':>10}"
            + f"{(cfg.time_angle_unit[:1] or 'D'):>10}"
        )
        emit("")
        emit("POSITION  DISPL     ORIENT")
        for probe in project.probes:
            position = probe.position_mm / 1000.0 if probe.position_mm >= 0 else probe.position_mm
            emit(_fw(_f10(position), _f10(float(probe.coordinate)), _f10(probe.orientation_deg)))

        # Sem Arq de mancal, OPTIONS permanece vazio e as análises da UI são
        # selecionadas pelas opções CLI. Com Arq, ``opt`` informa ao leitor
        # standard-input que o bloco KIND/BEAFILES será fornecido inline.
        emit("")
        emit("OPTIONS")
        emit("opt" if bearing_payloads else "")

        emit("")
        emit("CONCENT")
        emit(_i10(len(project.concentrated_masses)))
        emit("")
        emit("CMASSPOS  CMASS     IX        IY        IZ")
        for mass in project.concentrated_masses:
            emit(
                _fw(
                    _f10(mass.xi_mm / 1000.0), _f10(mass.kg), _f10(mass.ix_kg_m2),
                    _f10(mass.iy_kg_m2), _f10(mass.iz_kg_m2),
                )
            )

        if bearing_payloads:
            kind = bearing_payloads[0].kind
            emit("KIND      MODES")
            emit(_i10(kind))
            emit("")
            emit("BEAFILES  MINAMP    MDRPM     CPBCRX")
            emit(_fw(_i10(len(bearing_payloads)), _f10(cfg.minimum_amplification), _f10(cfg.modal_rpm)))
            emit("")
            emit("BEANBR    BEAFILES")
            for payload in bearing_payloads:
                # O nome é apenas um identificador no protocolo -std; o conteúdo
                # real segue inline e não é reaberto pelo solver.
                emit(_f10(float(payload.bearing_index)) + f" bearing_{payload.bearing_index}")
            emit("")
            emit("OPTIONS")
            emit("")
            for payload in bearing_payloads:
                lines.extend(payload.content.rstrip("\n").split("\n"))

        if project.supports:
            # Em -std, beasupf inicia a leitura logo após CMASSPOS/última massa.
            # Não inserir linha vazia aqui: o FORMAT(/,A) do solver usa o cabeçalho
            # SUPPORT como registro de avanço e lê a quantidade na linha seguinte.
            emit("SUPPORT   SCALE")
            # beasupf lê esta linha como texto/list-directed.
            emit(f"{len(project.supports)} 1.0")
            emit("")
            emit("BEANBR    KXX       KXZ       KZZ       KZX       CXX       CXZ       CZZ       CZX       WEIGTH    DESC")
            for support in project.supports:
                # entrada.f/beasupf: FORMAT(I10,9F10.0,A)
                desc = (support.type or "PYTHON")[:10]
                emit(
                    _fw(
                        _i10(support.bearing_index + 1), _f10(support.kxx), _f10(support.kxz),
                        _f10(support.kzz), _f10(support.kzx), _f10(support.cxx),
                        _f10(support.cxz), _f10(support.czz), _f10(support.czx), _f10(support.kg),
                    )
                    + desc
                )

        # Não escrevemos FOUNDATION/TRANSIENT/FLEXDISK: são funcionalidades do
        # solver posteriores à UI legada e ficam deliberadamente fora desta fase.
        emit("")
        return "\n".join(lines) + "\n"

    @staticmethod
    def _default_description(project: Project) -> str:
        i = project.identification
        parts = [x for x in [i.component, i.line, i.frame, f"{i.poles}P" if i.poles else "", f"{i.frequency_hz:g}Hz"] if x]
        return " ".join(parts) or i.reference or "RotorDin calculation"


class RotordinRunner:
    def __init__(self, executable: str | Path | None = None, run_root: str | Path | None = None):
        self.executable = str(executable) if executable else ""
        self.run_root = Path(run_root) if run_root else Path.home() / ".rotordin" / "runs"
        self.writer = SolverInputWriter()

    def resolve_executable(self, project: Project | None = None) -> Path:
        candidates: list[str] = []
        if project and project.configuration.solver_executable:
            candidates.append(project.configuration.solver_executable)
        if self.executable:
            candidates.append(self.executable)
        if os.environ.get("ROTORDIN_SOLVER"):
            candidates.append(os.environ["ROTORDIN_SOLVER"])
        # Distribuição instalada: aceita o solver ao lado do executável da UI
        # ou em uma subpasta ``solver``. Isso mantém a aplicação portátil sem
        # codificar caminhos de desenvolvimento.
        app_dir = Path(sys.executable).resolve().parent
        module_root = Path(__file__).resolve().parents[2]
        for bundled in (
            app_dir / "rotordin",
            app_dir / "rotordin.exe",
            app_dir / "solver" / "rotordin",
            app_dir / "solver" / "rotordin.exe",
            module_root / "solver" / "rotordin",
            module_root / "solver" / "rotordin.exe",
        ):
            candidates.append(str(bundled))
        which = shutil.which("rotordin")
        if which:
            candidates.append(which)
        for c in candidates:
            p = Path(c).expanduser()
            if p.is_file():
                return p.resolve()
        raise SolverError(
            "Executável RotorDin não encontrado. Defina-o em Configurações ou na variável ROTORDIN_SOLVER."
        )

    @staticmethod
    def _support_modifier(project: Project) -> list[str]:
        return ["-b"] if project.supports or any(b.support_enabled for b in project.bearings) else []

    @classmethod
    def analysis_jobs(cls, project: Project) -> list[tuple[str, list[str]]]:
        """Return isolated RotorDin jobs for the analyses selected in the UI.

        RotorDin accepts several command-line flags, but some combinations share
        standard-output units and can fail when executed simultaneously in ``-std``
        mode (for example both critical-speed maps). The legacy UI exposes the
        analyses as independent checkboxes. Therefore one UI execution is
        orchestrated as one solver invocation per selected analysis, then the
        report blocks are merged. This does not recalculate any physics.
        """
        a = project.analyses
        requested: list[tuple[str, str]] = []
        if a.campbell or a.log_decrement:
            requested.append(("campbell", "-c"))
        if a.dynamic_response:
            requested.append(("response", "-f"))
        if a.modes:
            requested.append(("modes", "-m"))
        if a.line:
            requested.append(("line", "-s"))
        if a.map:
            requested.append(("map", "-k"))
        if a.orbit:
            requested.append(("orbit", "-t"))
        if not requested:
            requested.append(("base", ""))
        modifier = cls._support_modifier(project)
        return [(name, ["-std", *([flag] if flag else []), *modifier]) for name, flag in requested]

    @classmethod
    def cli_flags(cls, project: Project) -> list[str]:
        """Compatibility helper for a project with one selected analysis.

        When several analyses are selected, returns the first isolated job flags;
        callers needing the full plan should use :meth:`analysis_jobs`.
        """
        return cls.analysis_jobs(project)[0][1]

    def _execute_job(
        self,
        executable: Path,
        flags: list[str],
        input_text: str,
        job_dir: Path,
        timeout_s: float,
    ) -> tuple[list[str], subprocess.CompletedProcess[str], dict[str, str]]:
        job_dir.mkdir(parents=True, exist_ok=False)
        (job_dir / "input.txt").write_text(input_text, encoding="utf-8")
        command = [str(executable), *flags]
        try:
            proc = subprocess.run(
                command,
                input=input_text,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                cwd=job_dir,
                timeout=timeout_s,
                check=False,
                creationflags=solver_process_creationflags(),
            )
        except subprocess.TimeoutExpired as exc:
            (job_dir / "stdout.out").write_text(
                _timeout_output_text(exc.stdout), encoding="utf-8"
            )
            (job_dir / "stderr.log").write_text(
                _timeout_output_text(exc.stderr), encoding="utf-8"
            )
            raise SolverError(f"RotorDin excedeu o timeout de {timeout_s:g} s.") from exc
        (job_dir / "stdout.out").write_text(proc.stdout, encoding="utf-8")
        (job_dir / "stderr.log").write_text(proc.stderr, encoding="utf-8")
        sections = parse_marked_sections(proc.stdout)
        if sections.get('rdaudit.out'):
            (job_dir / 'rdaudit.out').write_text(sections['rdaudit.out'], encoding='utf-8')
        if proc.returncode != 0:
            msg = proc.stderr.strip() or self._last_meaningful_line(proc.stdout) or f"return code {proc.returncode}"
            raise SolverError(f"RotorDin falhou: {msg}\nExecução: {job_dir}")
        return command, proc, sections

    def run(self, project: Project) -> SolverResult:
        project.validate()
        executable = self.resolve_executable(project)
        input_text = self.writer.render(project)
        self.run_root.mkdir(parents=True, exist_ok=True)
        stamp = datetime.now().strftime("%Y%m%d-%H%M%S-%f")
        safe_ref = re.sub(r"[^A-Za-z0-9_.-]+", "_", project.identification.reference.strip() or "calculo")[:80]
        run_dir = self.run_root / f"{stamp}_{safe_ref}"
        run_dir.mkdir(parents=True, exist_ok=False)
        (run_dir / "input.txt").write_text(input_text, encoding="utf-8")

        merged_sections: dict[str, str] = {}
        stdout_parts: list[str] = []
        stderr_parts: list[str] = []
        commands: list[list[str]] = []
        jobs_meta: list[dict[str, object]] = []

        for ordinal, (name, flags) in enumerate(self.analysis_jobs(project), start=1):
            job_dir = run_dir / f"{ordinal:02d}_{name}"
            command, proc, sections = self._execute_job(
                executable, flags, input_text, job_dir, project.configuration.timeout_s,
            )
            commands.append(command)
            stdout_parts.append(f"===== {name} : {' '.join(flags)} =====\n{proc.stdout.rstrip()}\n")
            if proc.stderr.strip():
                stderr_parts.append(f"===== {name} =====\n{proc.stderr.rstrip()}\n")
            for key, value in sections.items():
                # Analysis-specific reports are unique. For generic output/exectime,
                # retain the first one while every raw stdout stays auditably saved.
                if key == 'rdaudit.out':
                    merged_sections[key] = merged_sections.get(key, '') + value
                else:
                    merged_sections.setdefault(key, value)
            jobs_meta.append({
                "name": name,
                "command": command,
                "returncode": proc.returncode,
                "sections": sorted(sections),
                "directory": job_dir.name,
            })

        stdout = "\n".join(stdout_parts)
        stderr = "\n".join(stderr_parts)
        (run_dir / "stdout.out").write_text(stdout, encoding="utf-8")
        (run_dir / "stderr.log").write_text(stderr, encoding="utf-8")
        if merged_sections.get('rdaudit.out'):
            (run_dir / 'rdaudit.out').write_text(merged_sections['rdaudit.out'], encoding='utf-8')
        critical = parse_critical_speeds(merged_sections.get("campbell.out", ""))
        metadata = {
            "commands": commands,
            "jobs": jobs_meta,
            "returncode": 0,
            "sections": sorted(merged_sections),
            "critical_speeds": [{"rpm": x.rpm, "hz": x.hz, "harmonic": x.harmonic} for x in critical],
            "speed_dependent_bearings": [i + 1 for i, b in enumerate(project.bearings) if b.file.strip()],
        }
        (run_dir / "run.json").write_text(json.dumps(metadata, ensure_ascii=False, indent=2), encoding="utf-8")
        first_command = commands[0] if commands else [str(executable), "-std"]
        return SolverResult(first_command, 0, stdout, stderr, input_text, run_dir, merged_sections, critical, commands)

    @staticmethod
    def _last_meaningful_line(text: str) -> str:
        for line in reversed(text.splitlines()):
            if line.strip():
                return line.strip()
        return ""
