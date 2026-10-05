from __future__ import annotations

from dataclasses import asdict, dataclass, field
from datetime import datetime
import math
from enum import IntEnum, StrEnum
from typing import Any


class DomainError(ValueError):
    """Erro de regra/consistência do modelo da interface."""


class SectionKind(StrEnum):
    CIRCULAR = "Circular"
    RIBBED = "Costelada"


class ShaftElementKind(StrEnum):
    """Geometria nativa do elemento de eixo enviada ao solver RotorDin.

    ``AUTO`` existe somente para compatibilidade com projetos antigos. A interface
    sempre grava uma das quatro opções explícitas solicitadas pelo usuário.
    """

    AUTO = "Auto"
    SOLID_CYLINDRICAL = "Solid cylindrical shaft element"
    SOLID_CONICAL = "Solid conical shaft element"
    HOLLOW_CYLINDRICAL = "Hollow cylindrical shaft element"
    HOLLOW_CONICAL = "Hollow conical shaft element"


class ForceKind(IntEnum):
    TRANSIENT_TORQUE = -1
    UNBALANCE = 0
    CONCENTRATED = 1
    HARMONIC_TORQUE = 2
    STATIC_TORQUE = 3
    FREQUENCY_RESPONSE = 4
    DISTRIBUTED = 5


FORCE_KIND_LABELS: dict[int, str] = {
    -1: "-1 — Torque transiente",
    0: "0 — Desbalanceamento",
    1: "1 — Força concentrada",
    2: "2 — Torque harmônico",
    3: "3 — Torque estático",
    4: "4 — Força p/ resposta em frequência",
    5: "5 — Força distribuída",
}


@dataclass(slots=True)
class Identification:
    reference: str = ""
    component: str = ""
    description: str = ""
    line: str = ""
    poles: int = 2
    frame: str = ""
    frequency_hz: float = 60.0
    user: str = ""
    date_iso: str = field(default_factory=lambda: datetime.now().isoformat(timespec="seconds"))
    rpm_nominal: float = 0.0
    rpm_initial: float = 0.0
    rpm_final: float = 0.0


@dataclass(slots=True)
class AnalysisSelection:
    campbell: bool = True
    dynamic_response: bool = False
    modes: bool = False
    line: bool = False
    map: bool = False
    orbit: bool = False
    log_decrement: bool = False
    # Compatibility-only shim for old Python callers/tests that still pass
    # AnalysisSelection(auto=...) or assign analyses.auto. AUTO is no longer a
    # lateral analysis: the UI does not expose it, analysis_jobs ignores it and
    # Project.to_dict() deliberately does not persist it.
    auto: bool = False


@dataclass(slots=True)
class SolverConfiguration:
    # Cabeçalho SECTIONS
    min_divisions: int = 1
    max_divisions: int = 3
    load_distribution: float = 0.0
    horizontal_angle_deg: float = 0.0
    gravity_m_s2: float = 9.81
    horizontal_angle_unit: str = "D"
    section_filter_forward: int = 1
    section_filter_backward: int = 1

    # Material padrão das seções do eixo
    young_pa: float = 2.07e11
    poisson: float = 0.3
    density_kg_m3: float = 7850.0
    yield_strength_pa: float = 0.0

    # Dados legados de discos/pacote. O projeto mantém uma massa lógica para o
    # pacote; a tradução para DISK expande essa massa conforme package_divisions.
    disk_density_kg_m3: float = 7850.0
    package_divisions: int = 4

    # Mancais
    bearing_scale: float = 1.0
    static_bearing_stiffness_ratio: float = 0.0
    fixed_static_stiffness_n_m: float = 5.0e20

    # Campbell / mapa
    # Legacy attribute name kept for project compatibility. Native CPBSPD
    # is a dimensionless number of speed points, not an rpm increment.
    campbell_step_rpm: float = 25.0
    critical_speed_count: int = 4
    interpolation_points: int = 50
    map_stiffness_initial: float = 1.0e7
    map_number_curves: int = 4
    map_speed_rpm: float = 0.0
    # Track physical modal families across speed sweeps using complex MAC.
    # Disable only for strict reproduction of the historical rank-sorted output.
    mode_tracking: bool = True

    # Resposta / forças
    response_initial_rpm: float = 500.0
    response_final_rpm: float = 12000.0
    response_step_rpm: float = 400.0
    response_modes: int = 10
    minimum_amplification: float = 0.0
    modal_rpm: float = 0.0
    excitation_angle_unit: str = "D"

    # Saídas / órbita histórica
    output_modes: int = 10
    orbit_position_mm: float = 0.0
    orbit_speed_rpm: float = 0.0
    orbit_angle_deg: float = 0.0
    response_angle_unit: str = "D"
    time_angle_unit: str = "D"

    # Discretização FEM de trechos cônicos executada nativamente pelo solver Fortran.
    conical_subdivisions: int = 12

    # Execução
    solver_executable: str = ""
    timeout_s: float = 300.0

    def validate(self) -> None:
        if self.min_divisions < 1 or self.max_divisions < 1:
            raise DomainError("DIV e MXDIV devem ser maiores que zero.")
        if self.gravity_m_s2 < 0:
            raise DomainError("ACCG não pode ser negativo.")
        if self.young_pa <= 0 or self.density_kg_m3 <= 0:
            raise DomainError("Módulo de Young e densidade devem ser positivos.")
        if self.disk_density_kg_m3 <= 0:
            raise DomainError("Densidade dos discos deve ser maior que zero.")
        if self.package_divisions < 1 or self.package_divisions > 99:
            raise DomainError("DIV pacote deve estar entre 1 e 99.")
        if self.poisson <= -1 or self.poisson >= 0.5:
            raise DomainError("Coeficiente de Poisson fora da faixa física (-1, 0,5).")
        # Native CPBSPD is the number of Campbell speed points;
        # campbl.f divides the range by (CPBSPD-1) and stores at most mtg=500.
        cpbspd = float(self.campbell_step_rpm)
        if (
            cpbspd < 2.0
            or cpbspd > 500.0
            or abs(cpbspd - round(cpbspd)) > 1.0e-9
        ):
            raise DomainError(
                "CPBSPD deve ser um número inteiro de pontos entre 2 e 500."
            )
        if self.response_step_rpm <= 0:
            raise DomainError("Passo da resposta deve ser maior que zero.")
        if self.response_modes < 1 or self.output_modes < 1:
            raise DomainError("Número de modos deve ser maior que zero.")
        if self.critical_speed_count > 19 or self.output_modes > 19:
            raise DomainError("NCRSPD/NBRMOD excede o limite de 19 modos do RotorDin.")
        if self.conical_subdivisions < 2:
            raise DomainError("Discretização de trecho cônico deve ter ao menos 2 divisões.")
        if self.timeout_s <= 0:
            raise DomainError("Timeout do solver deve ser maior que zero.")


@dataclass(slots=True)
class ShaftSegment:
    length_mm: float
    diameter_mm: float
    package_diameter_mm: float = 0.0
    a_mm: float = 0.0
    b_mm: float = 0.0
    c_mm: float = 0.0
    rib_count: int = 0
    inner_diameter_mm: float = 0.0
    final_diameter_mm: float = 0.0
    section: SectionKind = SectionKind.CIRCULAR
    final_inner_diameter_mm: float = 0.0
    element_type: ShaftElementKind = ShaftElementKind.AUTO

    @property
    def resolved_element_type(self) -> ShaftElementKind:
        if self.element_type != ShaftElementKind.AUTO:
            return self.element_type
        hollow = self.inner_diameter_mm > 0.0 or self.final_inner_diameter_mm > 0.0
        final_outer = self.final_diameter_mm if self.final_diameter_mm > 0.0 else self.diameter_mm
        final_inner = (
            self.final_inner_diameter_mm
            if self.final_inner_diameter_mm > 0.0
            else self.inner_diameter_mm
        )
        conical = (
            abs(final_outer - self.diameter_mm) > 1e-12
            or abs(final_inner - self.inner_diameter_mm) > 1e-12
        )
        if hollow and conical:
            return ShaftElementKind.HOLLOW_CONICAL
        if hollow:
            return ShaftElementKind.HOLLOW_CYLINDRICAL
        if conical:
            return ShaftElementKind.SOLID_CONICAL
        return ShaftElementKind.SOLID_CYLINDRICAL

    @property
    def effective_final_diameter_mm(self) -> float:
        return self.final_diameter_mm if self.final_diameter_mm > 0.0 else self.diameter_mm

    @property
    def effective_final_inner_diameter_mm(self) -> float:
        if self.resolved_element_type in (
            ShaftElementKind.HOLLOW_CYLINDRICAL, ShaftElementKind.HOLLOW_CONICAL
        ):
            return (
                self.final_inner_diameter_mm
                if self.final_inner_diameter_mm > 0.0
                else self.inner_diameter_mm
            )
        return 0.0

    @property
    def is_hollow(self) -> bool:
        return self.resolved_element_type in (
            ShaftElementKind.HOLLOW_CYLINDRICAL, ShaftElementKind.HOLLOW_CONICAL
        )

    @property
    def is_conical(self) -> bool:
        return self.resolved_element_type in (
            ShaftElementKind.SOLID_CONICAL, ShaftElementKind.HOLLOW_CONICAL
        )

    @property
    def is_ribbed(self) -> bool:
        return self.section == SectionKind.RIBBED or self.rib_count > 0 or self.package_diameter_mm > 0

    def validate(self) -> None:
        if self.length_mm <= 0:
            raise DomainError("O comprimento L deve ser maior que zero.")
        if self.diameter_mm <= 0:
            raise DomainError("O diâmetro D deve ser maior que zero.")
        if self.inner_diameter_mm < 0 or self.inner_diameter_mm >= self.diameter_mm:
            if self.inner_diameter_mm != 0:
                raise DomainError("D_INT deve ser não-negativo e menor que D.")
        if self.final_diameter_mm < 0:
            raise DomainError("D_F não pode ser negativo.")
        if self.final_inner_diameter_mm < 0:
            raise DomainError("D_INT_F não pode ser negativo.")

        etype = self.resolved_element_type
        final_outer = self.effective_final_diameter_mm
        final_inner = self.effective_final_inner_diameter_mm
        if final_outer <= 0.0:
            raise DomainError("O diâmetro externo final deve ser maior que zero.")
        if final_inner < 0.0 or final_inner >= final_outer:
            if final_inner != 0.0:
                raise DomainError("D_INT_F deve ser não-negativo e menor que D_F.")
        if etype in (ShaftElementKind.SOLID_CYLINDRICAL, ShaftElementKind.SOLID_CONICAL):
            if self.inner_diameter_mm > 0.0 or self.final_inner_diameter_mm > 0.0:
                raise DomainError("Elementos sólidos não podem possuir diâmetro interno.")
        if etype in (ShaftElementKind.HOLLOW_CYLINDRICAL, ShaftElementKind.HOLLOW_CONICAL):
            if self.inner_diameter_mm <= 0.0:
                raise DomainError("Elementos ocos exigem D_INT maior que zero.")
        if etype in (ShaftElementKind.SOLID_CYLINDRICAL, ShaftElementKind.HOLLOW_CYLINDRICAL):
            if abs(final_outer - self.diameter_mm) > 1e-12 or abs(final_inner - self.inner_diameter_mm) > 1e-12:
                raise DomainError("Elementos cilíndricos devem ter diâmetros inicial e final iguais.")
        if etype in (ShaftElementKind.SOLID_CONICAL, ShaftElementKind.HOLLOW_CONICAL):
            if (
                abs(final_outer - self.diameter_mm) <= 1e-12
                and abs(final_inner - self.inner_diameter_mm) <= 1e-12
            ):
                raise DomainError("Elementos cônicos exigem variação de diâmetro ao longo do trecho.")
        if self.is_ribbed and etype in (ShaftElementKind.SOLID_CONICAL, ShaftElementKind.HOLLOW_CONICAL):
            raise DomainError(
                "Seção costelada cônica ainda não é qualificada. "
                "Use elemento cilíndrico ou remova a geometria costelada."
            )
        if self.rib_count < 0:
            raise DomainError("NC não pode ser negativo.")
        if self.is_ribbed:
            if self.package_diameter_mm <= self.diameter_mm:
                raise DomainError("DPCT deve ser maior que D em seção costelada.")
            if self.rib_count <= 0:
                raise DomainError("NC deve ser maior que zero em seção costelada.")
            if min(self.a_mm, self.b_mm, self.c_mm) < 0:
                raise DomainError("A, B e C não podem ser negativos.")


@dataclass(slots=True)
class Bearing:
    position_mm: float
    kxx: float = 0.0
    kzz: float = 0.0
    kxz: float = 0.0
    kzx: float = 0.0
    cxx: float = 0.0
    czz: float = 0.0
    cxz: float = 0.0
    czx: float = 0.0
    type: str = ""
    file: str = ""
    support_enabled: bool = False
    mass_kg: float = 0.0
    kphi: float = 0.0
    ktheta: float = 0.0

    def validate(self, shaft_length_mm: float) -> None:
        if not 0 <= self.position_mm <= shaft_length_mm:
            raise DomainError("Posição do mancal fora do comprimento do eixo.")
        if self.mass_kg < 0:
            raise DomainError("Massa do mancal não pode ser negativa.")


@dataclass(slots=True)
class DistributedMass:
    xi_mm: float
    length_mm: float
    kg: float
    outer_diameter_mm: float = 0.0
    inner_diameter_mm: float = 0.0
    package: bool = False
    ump: bool = False
    # Persisted compatibility name. Physical meaning: distributed
    # electromagnetic negative stiffness k' [N/m²].
    ump_value: float = 0.0
    # Axial offset between the disk geometric plane and its center of gravity.
    # Positive values follow the shaft axial coordinate; persisted in millimetres.
    offset_mm: float = 0.0

    @property
    def ump_stiffness_n_m2(self) -> float:
        """Distributed UMP electromagnetic stiffness k' [N/m²]."""
        return self.ump_value

    @ump_stiffness_n_m2.setter
    def ump_stiffness_n_m2(self, value: float) -> None:
        self.ump_value = float(value)

    @property
    def center_mm(self) -> float:
        return self.xi_mm + self.length_mm / 2.0

    def validate(self, shaft_length_mm: float) -> None:
        if self.xi_mm < 0 or self.length_mm <= 0 or self.xi_mm + self.length_mm > shaft_length_mm + 1e-9:
            raise DomainError("Massa distribuída fora do comprimento do eixo ou com LC inválido.")
        if self.kg < 0:
            raise DomainError("Massa deve ser não-negativa.")
        if self.outer_diameter_mm < 0 or self.inner_diameter_mm < 0:
            raise DomainError("Diâmetros da massa devem ser não-negativos.")
        if self.outer_diameter_mm and self.inner_diameter_mm >= self.outer_diameter_mm:
            raise DomainError("D.Int da massa deve ser menor que D.Ext.")
        if not math.isfinite(self.offset_mm):
            raise DomainError("Offset da massa deve ser finito [mm].")
        if self.ump and (
            not math.isfinite(self.ump_stiffness_n_m2)
            or self.ump_stiffness_n_m2 < 0.0
        ):
            raise DomainError(
                "Rigidez UMP k' deve ser finita e não-negativa [N/m²]."
            )


@dataclass(slots=True)
class HarmonicForce:
    position_mm: float
    phase_deg: float
    value: float
    kind: int = 0
    extra: float = 0.0

    def validate(self, shaft_length_mm: float, section_count: int) -> None:
        if self.kind < -1 or self.kind > 5:
            raise DomainError("Tipo de força deve estar entre -1 e 5.")
        if self.kind == int(ForceKind.DISTRIBUTED):
            idx = round(self.position_mm)
            if idx < 1 or idx > section_count:
                raise DomainError("Força distribuída usa número de seção válido em Pos.")
        elif not 0 <= self.position_mm <= shaft_length_mm:
            raise DomainError("Posição da força fora do comprimento do eixo.")


@dataclass(slots=True)
class ResponseProbe:
    position_mm: float
    coordinate: int = 1
    orientation_deg: float = 0.0

    def validate(self, shaft_length_mm: float) -> None:
        # Posições negativas são reservadas pelo solver para respostas em suporte.
        if self.position_mm >= 0 and self.position_mm > shaft_length_mm:
            raise DomainError("Posição de resposta fora do comprimento do eixo.")
        if self.coordinate not in (1, 2):
            raise DomainError("Coord. da resposta deve ser 1 ou 2.")


@dataclass(slots=True)
class ConcentratedMass:
    xi_mm: float
    kg: float
    ix_kg_m2: float = 0.0
    iy_kg_m2: float = 0.0
    iz_kg_m2: float = 0.0

    def validate(self, shaft_length_mm: float) -> None:
        if not 0 <= self.xi_mm <= shaft_length_mm:
            raise DomainError("Massa concentrada fora do comprimento do eixo.")
        if min(self.kg, self.ix_kg_m2, self.iy_kg_m2, self.iz_kg_m2) < 0:
            raise DomainError("Massa e inércias concentradas devem ser não-negativas.")


@dataclass(slots=True)
class Support:
    bearing_index: int
    kxx: float = 0.0
    kzz: float = 0.0
    kxz: float = 0.0
    kzx: float = 0.0
    cxx: float = 0.0
    czz: float = 0.0
    cxz: float = 0.0
    czx: float = 0.0
    kg: float = 0.0
    type: str = ""

    def validate(self, bearing_count: int) -> None:
        if self.bearing_index < 0 or self.bearing_index >= bearing_count:
            raise DomainError("Índice de mancal do suporte é inválido.")
        if self.kg <= 0:
            raise DomainError("Suporte deve possuir massa maior que zero.")


@dataclass(slots=True)
class Project:
    identification: Identification = field(default_factory=Identification)
    analyses: AnalysisSelection = field(default_factory=AnalysisSelection)
    configuration: SolverConfiguration = field(default_factory=SolverConfiguration)
    segments: list[ShaftSegment] = field(default_factory=list)
    bearings: list[Bearing] = field(default_factory=list)
    masses: list[DistributedMass] = field(default_factory=list)
    forces: list[HarmonicForce] = field(default_factory=list)
    probes: list[ResponseProbe] = field(default_factory=list)
    concentrated_masses: list[ConcentratedMass] = field(default_factory=list)
    supports: list[Support] = field(default_factory=list)
    settings: dict[str, Any] = field(default_factory=dict)

    @property
    def shaft_length_mm(self) -> float:
        return sum(s.length_mm for s in self.segments)

    def validate(self) -> None:
        self.configuration.validate()

        # maprigs/mapriga store at most mxk=50 stiffness points. This is an
        # analysis-specific constraint: stale/legacy map settings must not block
        # unrelated jobs such as Orbit, Campbell, Modes or mesh qualification
        # when the map analysis is not selected.
        if self.analyses.map:
            nrdc = int(self.configuration.map_number_curves)
            if nrdc < 1 or nrdc > 5:
                points = 1 + 9 * nrdc
                raise DomainError(
                    "SMNDC (dc) deve estar entre 1 e 5. "
                    f"O valor {nrdc} solicita {points} pontos de rigidez, "
                    "mas o mapa nativo do RotorDin suporta no máximo 50 (mxk=50)."
                )

        if not self.segments:
            raise DomainError("Defina ao menos um segmento de eixo.")
        for s in self.segments:
            s.validate()
        length = self.shaft_length_mm
        if len(self.bearings) > 9:
            raise DomainError("O RotorDin aceita no máximo 9 mancais laterais.")
        if len(self.masses) > 99:
            raise DomainError("O RotorDin aceita no máximo 99 massas/discos.")
        expanded_disks = sum(
            self.configuration.package_divisions if mass.package else 1
            for mass in self.masses
        )
        if expanded_disks > 99:
            raise DomainError(
                f"A expansão do pacote gera {expanded_disks} discos; o RotorDin aceita no máximo 99."
            )
        if len(self.forces) > 99:
            raise DomainError("O RotorDin aceita no máximo 99 excitações.")
        if len(self.probes) > 9:
            raise DomainError("O RotorDin aceita no máximo 9 posições de resposta.")
        if len(self.concentrated_masses) > 15:
            raise DomainError("O RotorDin aceita no máximo 15 massas concentradas.")
        if len(self.supports) > 9:
            raise DomainError("O RotorDin aceita no máximo 9 suportes de mancal.")
        for b in self.bearings:
            b.validate(length)
        # Speed-dependent bearing files are validated against the speed
        # actually used by each selected analysis during solver serialization.
        # Do not bind them globally to identification.rpm_i/rpm_f: those fields
        # are the historical Campbell mirror and response/orbit/modal speeds are
        # independent first-class analysis inputs.
        for m in self.masses:
            m.validate(length)

        # Regra histórica da grade Xi/LC: uma massa distribuída deve estar
        # inteiramente contida em um único segmento do eixo e intervalos de
        # massa não podem se sobrepor. O VB6 fazia essas verificações na UI;
        # na migração elas pertencem ao domínio para valer também em importação.
        segment_limits: list[tuple[float, float]] = []
        x0 = 0.0
        for seg in self.segments:
            x1 = x0 + seg.length_mm
            segment_limits.append((x0, x1))
            x0 = x1
        for idx, mass in enumerate(self.masses, start=1):
            a, b = mass.xi_mm, mass.xi_mm + mass.length_mm
            if not any(a >= s0 - 1e-9 and b <= s1 + 1e-9 for s0, s1 in segment_limits):
                raise DomainError(
                    f"Massa #{idx} (Xi/LC) ultrapassa um escalonamento do eixo."
                )
        ordered = sorted(enumerate(self.masses, start=1), key=lambda item: item[1].xi_mm)
        for (ia, ma), (ib, mb) in zip(ordered, ordered[1:]):
            if mb.xi_mm < ma.xi_mm + ma.length_mm - 1e-9:
                raise DomainError(f"Massas #{ia} e #{ib} possuem regiões Xi/LC sobrepostas.")

        if sum(1 for m in self.masses if m.package) > 1:
            raise DomainError("Somente uma massa pode ser definida como Pacote por cálculo.")
        for f in self.forces:
            f.validate(length, len(self.segments))
        for p in self.probes:
            p.validate(length)
        for c in self.concentrated_masses:
            c.validate(length)
        seen_supports: set[int] = set()
        for s in self.supports:
            s.validate(len(self.bearings))
            if s.bearing_index in seen_supports:
                raise DomainError("Há mais de um suporte associado ao mesmo mancal.")
            seen_supports.add(s.bearing_index)
        for idx, bearing in enumerate(self.bearings):
            if bearing.support_enabled and idx not in seen_supports:
                raise DomainError(
                    f"Mancal #{idx + 1} está marcado como suporte, mas não possui linha correspondente na aba Suporte."
                )

        # Analysis ranges are validated by the installed speed contract using
        # their authoritative Campbell/response/orbit fields.  The historical
        # Identification rpm_i/rpm_f mirror must not constrain unrelated jobs.

    def to_dict(self) -> dict[str, Any]:
        d = asdict(self)
        # AUTO is accepted only as an in-memory compatibility shim. Never write
        # it into new project files after the lateral analysis was removed.
        d.get("analyses", {}).pop("auto", None)
        for s in d["segments"]:
            s["section"] = str(s["section"])
            s["element_type"] = str(s["element_type"])
        return d

    @classmethod
    def from_dict(cls, d: dict[str, Any]) -> "Project":
        # Legacy projects may still contain the removed lateral "auto" analysis.
        # Discard only that obsolete key so historical files remain loadable.
        analyses_data = dict(d.get("analyses", {}))
        analyses_data.pop("auto", None)
        return cls(
            identification=Identification(**d.get("identification", {})),
            analyses=AnalysisSelection(**analyses_data),
            configuration=SolverConfiguration(**d.get("configuration", {})),
            segments=[
                ShaftSegment(**{
                    **x,
                    "section": SectionKind(x.get("section", "Circular")),
                    "element_type": ShaftElementKind(x.get("element_type", "Auto")),
                })
                for x in d.get("segments", [])
            ],
            bearings=[Bearing(**x) for x in d.get("bearings", [])],
            masses=[DistributedMass(**x) for x in d.get("masses", [])],
            forces=[HarmonicForce(**x) for x in d.get("forces", [])],
            probes=[ResponseProbe(**x) for x in d.get("probes", [])],
            concentrated_masses=[ConcentratedMass(**x) for x in d.get("concentrated_masses", [])],
            supports=[Support(**x) for x in d.get("supports", [])],
            settings=dict(d.get("settings", {})),
        )