from __future__ import annotations

from collections import defaultdict
from hashlib import sha256
from pathlib import Path
import re

from .engineering_units import legacy_ump_provenance
from .domain import (
    AnalysisSelection,
    Bearing,
    ConcentratedMass,
    DistributedMass,
    DomainError,
    HarmonicForce,
    Identification,
    Project,
    ResponseProbe,
    SectionKind,
    ShaftSegment,
    SolverConfiguration,
    Support,
)


_GRID_KEY = re.compile(r"^(\d+)\s*,\s*(\d+)$")
_SAFE_NAME = re.compile(r"[^A-Za-z0-9_.-]+")


class LegacyImportError(DomainError):
    """Arquivo de cálculo legado inválido ou incompatível."""


def _read_text(path: str | Path) -> str:
    source = Path(path)
    if not source.is_file():
        raise LegacyImportError(f"Arquivo de cálculo não encontrado: {source}")
    for encoding in ("utf-8-sig", "cp1252", "latin-1"):
        try:
            return source.read_text(encoding=encoding)
        except UnicodeDecodeError:
            continue
    raise LegacyImportError(f"Não foi possível decodificar o arquivo: {source}")


def _parse_document(text: str) -> dict[str, dict[str, str]]:
    sections: dict[str, dict[str, str]] = {}
    current: dict[str, str] | None = None
    for line_no, raw in enumerate(text.replace("\r\n", "\n").replace("\r", "\n").splitlines(), start=1):
        line = raw.strip()
        if not line or line.startswith((";", "#")):
            continue
        if line.startswith("[") and line.endswith("]"):
            name = line[1:-1].strip().casefold()
            current = sections.setdefault(name, {})
            continue
        if "=" not in line:
            # O formato VB6/INI contém somente pares chave=valor fora dos cabeçalhos.
            # Ignorar texto solto torna o import tolerante a comentários antigos.
            continue
        if current is None:
            raise LegacyImportError(f"Linha {line_no}: valor fora de uma seção INI.")
        key, value = line.split("=", 1)
        current[key.strip().casefold()] = value.strip()
    if "dados" not in sections:
        raise LegacyImportError("Arquivo legado sem a seção obrigatória [Dados].")
    if "secoes" not in sections:
        raise LegacyImportError("Arquivo legado sem a seção obrigatória [Secoes].")
    return sections


def _number(value: str | float | int | None, default: float = 0.0) -> float:
    if value is None:
        return default
    text = str(value).strip()
    if not text:
        return default
    text = text.replace("D", "E").replace("d", "e")
    if "," in text and "." not in text:
        text = text.replace(",", ".")
    try:
        return float(text)
    except ValueError as exc:
        raise LegacyImportError(f"Valor numérico inválido no arquivo legado: {value!r}") from exc


def _integer(value: str | float | int | None, default: int = 0) -> int:
    if value is None or str(value).strip() == "":
        return default
    number = _number(value)
    rounded = int(round(number))
    if abs(number - rounded) > 1e-9:
        raise LegacyImportError(f"Valor inteiro inválido no arquivo legado: {value!r}")
    return rounded


def _bool(value: str | float | int | None) -> bool:
    if value is None:
        return False
    text = str(value).strip().casefold()
    if text in {"true", "yes", "sim", "on"}:
        return True
    return bool(_integer(value, 0))


def _grid_rows(section: dict[str, str] | None) -> list[dict[int, str]]:
    grouped: dict[int, dict[int, str]] = defaultdict(dict)
    for key, value in (section or {}).items():
        match = _GRID_KEY.match(key)
        if not match:
            continue
        row = int(match.group(1))
        col = int(match.group(2))
        if row < 1 or col < 0:
            raise LegacyImportError(f"Índice de grade legado inválido: {key}")
        grouped[row][col] = value
    return [grouped[row] for row in sorted(grouped)]


def _cell(row: dict[int, str], col: int, default: str = "") -> str:
    return row.get(col, default)


def _inline_table_content(raw: str, bearing_index: int) -> tuple[str, float | None]:
    chunks = [chunk.strip() for chunk in raw.split("§") if chunk.strip()]
    if not chunks:
        raise LegacyImportError(f"Arq do mancal #{bearing_index} está vazio.")
    kind = chunks[0].upper()
    rows = [chunk.replace("|", " ").strip() for chunk in chunks[1:]]
    if kind.startswith("TABLE"):
        if not rows:
            raise LegacyImportError(f"TABLE do mancal #{bearing_index} não possui pontos.")
        minimum_speed: float | None = None
        for row_no, row in enumerate(rows, start=1):
            fields = row.split()
            if len(fields) != 9:
                raise LegacyImportError(
                    f"TABLE do mancal #{bearing_index}, ponto {row_no}: esperadas 9 colunas "
                    "(rpm + 8 coeficientes K/C)."
                )
            values = [_number(value) for value in fields]
            speed = values[0]
            if speed < 0:
                raise LegacyImportError(f"TABLE do mancal #{bearing_index}: rotação negativa.")
            minimum_speed = speed if minimum_speed is None else min(minimum_speed, speed)
        content = (
            f"TABLE:LEGACY_IMPORT_{bearing_index}\n"
            f"{len(rows)} 1.0\n"
            + "\n".join(rows)
            + "\nEOF\n"
        )
        return content, minimum_speed
    if kind.startswith("COEF"):
        if not rows:
            raise LegacyImportError(f"COEF do mancal #{bearing_index} não possui dados.")
        return f"COEF:LEGACY_IMPORT_{bearing_index}\n" + "\n".join(rows) + "\nEOF\n", None
    raise LegacyImportError(
        f"Arq do mancal #{bearing_index} deve iniciar por TABLE ou COEF; recebido {chunks[0]!r}."
    )


def _bearing_asset(
    raw: str,
    *,
    source: Path,
    bearing_index: int,
    asset_root: str | Path | None,
) -> tuple[str, float | None]:
    value = raw.strip()
    if not value:
        return "", None
    if not value.upper().startswith(("TABLE§", "COEF§", "TABLE\u00a7", "COEF\u00a7")):
        candidate = Path(value).expanduser()
        if not candidate.is_absolute():
            relative = source.parent / candidate
            if relative.exists():
                candidate = relative
        return str(candidate), None

    content, minimum_speed = _inline_table_content(value, bearing_index)
    digest = sha256((str(source.resolve()) + "\n" + value).encode("utf-8")).hexdigest()[:16]
    if asset_root is None:
        root = Path.home() / ".rotordin" / "imports" / digest
    else:
        root = Path(asset_root)
    root.mkdir(parents=True, exist_ok=True)
    suffix = ".tbl" if value.upper().startswith("TABLE") else ".coef"
    target = root / f"bearing_{bearing_index}{suffix}"
    target.write_text(content, encoding="utf-8", newline="\n")
    return str(target), minimum_speed


def load_legacy_project(path: str | Path, *, asset_root: str | Path | None = None) -> Project:
    """Importa o formato textual/INI produzido pelo iRdin VB6.

    O arquivo legado usa grades esparsas com chaves ``linha,coluna``. O importador
    preserva a numeração/ordem dessas linhas, converte apenas tipos e materializa
    ``Arq=TABLE§...``/``COEF§...`` em arquivos nativos aceitos pelo RotorDin atual.
    Nenhuma análise física é executada durante o import.
    """

    source = Path(path)
    text = _read_text(source)
    document = _parse_document(text)
    header = document.get("irdin", {})
    data = document["dados"]

    ident = Identification(
        reference=data.get("ref", ""),
        component=data.get("comp", ""),
        description=data.get("obs", data.get("descricao", "")),
        line=data.get("linha", ""),
        poles=max(1, _integer(data.get("polos"), 2)),
        frame=data.get("carc", ""),
        frequency_hz=_number(data.get("freq"), 60.0),
        user=header.get("usuario", data.get("usuario", "")),
        date_iso=header.get("data", data.get("data", "")),
        rpm_nominal=_number(data.get("nnom"), 0.0),
        rpm_initial=_number(data.get("c_rpmi"), 0.0),
        rpm_final=_number(data.get("c_rpmf"), 0.0),
    )

    cfg = SolverConfiguration()
    cfg.young_pa = _number(data.get("s_melast"), cfg.young_pa)
    cfg.density_kg_m3 = _number(data.get("s_masesp"), cfg.density_kg_m3)
    cfg.poisson = _number(data.get("s_poisson"), cfg.poisson)
    cfg.min_divisions = max(1, _integer(data.get("s_div"), cfg.min_divisions))
    cfg.max_divisions = max(1, _integer(data.get("s_mxdiv"), cfg.max_divisions))
    cfg.campbell_step_rpm = _number(data.get("c_div"), cfg.campbell_step_rpm)
    cfg.critical_speed_count = _integer(data.get("c_nrrot"), cfg.critical_speed_count)
    cfg.interpolation_points = max(1, _integer(data.get("c_interp"), cfg.interpolation_points))
    cfg.response_initial_rpm = _number(data.get("d_rpmi"), cfg.response_initial_rpm)
    cfg.response_final_rpm = _number(data.get("d_rpmf"), cfg.response_final_rpm)
    cfg.response_step_rpm = _number(data.get("d_div"), cfg.response_step_rpm)
    cfg.response_modes = max(1, _integer(data.get("d_nrmodos"), cfg.response_modes))
    cfg.output_modes = max(1, _integer(data.get("m_nrmodos"), cfg.output_modes))
    cfg.orbit_position_mm = _number(data.get("t_pos"), cfg.orbit_position_mm)
    cfg.orbit_speed_rpm = _number(data.get("t_rpm"), cfg.orbit_speed_rpm)
    cfg.gravity_m_s2 = _number(data.get("le_g"), cfg.gravity_m_s2)
    cfg.map_stiffness_initial = _number(data.get("mr_rini"), cfg.map_stiffness_initial)
    cfg.map_number_curves = max(1, _integer(data.get("mr_nrdc"), cfg.map_number_curves))

    project = Project(
        identification=ident,
        analyses=AnalysisSelection(),
        configuration=cfg,
    )

    for row in _grid_rows(document.get("secoes")):
        package_diameter = _number(_cell(row, 2), 0.0)
        rib_count = _integer(_cell(row, 6), 0)
        project.segments.append(
            ShaftSegment(
                length_mm=_number(_cell(row, 0)),
                diameter_mm=_number(_cell(row, 1)),
                package_diameter_mm=package_diameter,
                a_mm=_number(_cell(row, 3), 0.0),
                b_mm=_number(_cell(row, 4), 0.0),
                c_mm=_number(_cell(row, 5), 0.0),
                rib_count=rib_count,
                inner_diameter_mm=_number(_cell(row, 7), 0.0),
                final_diameter_mm=_number(_cell(row, 8), 0.0),
                section=SectionKind.RIBBED if package_diameter or rib_count else SectionKind.CIRCULAR,
            )
        )

    global_ump = _number(data.get("ump_crg"), 0.0)
    for row in _grid_rows(document.get("massas")):
        ump = _bool(_cell(row, 5))
        project.masses.append(
            DistributedMass(
                xi_mm=_number(_cell(row, 0)),
                length_mm=_number(_cell(row, 1)),
                kg=_number(_cell(row, 2)),
                outer_diameter_mm=_number(_cell(row, 3), 0.0),
                inner_diameter_mm=_number(_cell(row, 6), 0.0),
                package=_bool(_cell(row, 4)),
                ump=ump,
                ump_value=global_ump if ump else 0.0,
            )
        )

    minimum_table_speed: float | None = None
    bearing_rows = _grid_rows(document.get("mancais"))
    for index, row in enumerate(bearing_rows, start=1):
        bearing_file, table_min = _bearing_asset(
            _cell(row, 11),
            source=source,
            bearing_index=index,
            asset_root=asset_root,
        )
        if table_min is not None:
            minimum_table_speed = table_min if minimum_table_speed is None else min(minimum_table_speed, table_min)
        project.bearings.append(
            Bearing(
                position_mm=_number(_cell(row, 0)),
                kxx=_number(_cell(row, 1), 0.0),
                kzz=_number(_cell(row, 2), 0.0),
                kxz=_number(_cell(row, 3), 0.0),
                kzx=_number(_cell(row, 4), 0.0),
                cxx=_number(_cell(row, 5), 0.0),
                czz=_number(_cell(row, 6), 0.0),
                cxz=_number(_cell(row, 7), 0.0),
                czx=_number(_cell(row, 8), 0.0),
                type=_cell(row, 10),
                file=bearing_file,
            )
        )

    for row in _grid_rows(document.get("desbal")):
        project.forces.append(
            HarmonicForce(
                position_mm=_number(_cell(row, 0)),
                phase_deg=_number(_cell(row, 1), 0.0),
                value=_number(_cell(row, 2), 0.0),
                kind=0,
            )
        )

    # Algumas versões exportaram a grade geral como [Forca] ou [Forcas].
    force_section = document.get("forca") or document.get("forcas")
    for row in _grid_rows(force_section):
        project.forces.append(
            HarmonicForce(
                position_mm=_number(_cell(row, 0)),
                phase_deg=_number(_cell(row, 1), 0.0),
                value=_number(_cell(row, 2), 0.0),
                kind=_integer(_cell(row, 3), 0),
            )
        )

    for row in _grid_rows(document.get("respo")):
        project.probes.append(
            ResponseProbe(
                position_mm=_number(_cell(row, 0)),
                coordinate=_integer(_cell(row, 1), 1),
                orientation_deg=_number(_cell(row, 2), 0.0),
            )
        )

    for row in _grid_rows(document.get("concent")):
        project.concentrated_masses.append(
            ConcentratedMass(
                xi_mm=_number(_cell(row, 0)),
                kg=_number(_cell(row, 1)),
                ix_kg_m2=_number(_cell(row, 2), 0.0),
                iy_kg_m2=_number(_cell(row, 3), 0.0),
                iz_kg_m2=_number(_cell(row, 4), 0.0),
            )
        )

    support_indices: set[int] = set()
    for row in _grid_rows(document.get("suporte")):
        bearing_number = _integer(_cell(row, 0))
        support_indices.add(bearing_number)
        project.supports.append(
            Support(
                bearing_index=bearing_number - 1,
                kxx=_number(_cell(row, 1), 0.0),
                kzz=_number(_cell(row, 2), 0.0),
                kxz=_number(_cell(row, 3), 0.0),
                kzx=_number(_cell(row, 4), 0.0),
                cxx=_number(_cell(row, 5), 0.0),
                czz=_number(_cell(row, 6), 0.0),
                cxz=_number(_cell(row, 7), 0.0),
                czx=_number(_cell(row, 8), 0.0),
                kg=_number(_cell(row, 9), 0.0),
                type=_cell(row, 10),
            )
        )
    for index, bearing in enumerate(project.bearings, start=1):
        bearing.support_enabled = index in support_indices

    known_data = {
        "ref", "linha", "carc", "comp", "polos", "freq", "nnom",
        "s_melast", "s_masesp", "s_poisson", "s_div", "s_mxdiv",
        "c_rpmi", "c_rpmf", "c_div", "c_nrrot", "c_interp",
        "d_rpmi", "d_rpmf", "d_div", "d_nrmodos", "m_nrmodos",
        "t_pos", "t_rpm", "le_g", "mr_rini", "mr_nrdc", "ump_crg",
        "obs", "descricao", "usuario", "data",
    }
    warnings: list[str] = []
    if minimum_table_speed is not None and ident.rpm_initial < minimum_table_speed:
        warnings.append(
            f"rpm i legado ({ident.rpm_initial:g}) é menor que o primeiro ponto das tabelas de mancal "
            f"({minimum_table_speed:g} rpm). Ajuste rpm i antes de salvar/executar, se necessário."
        )
    legacy_metadata = {
        "format": "irDin/VB6 INI",
        "source_name": source.name,
        "source_path": str(source),
        # Preserve raw response-speed semantics at the import boundary. d_div is
        # a historical division parameter and must never be reconstructed later
        # from the modern UNBFD rpm increment.
        "raw_speed_fields": {
            key: data[key]
            for key in ("d_rpmi", "d_rpmf", "d_div")
            if key in data
        },
        "unmapped_dados": {key: value for key, value in data.items() if key not in known_data},
        "warnings": warnings,
    }

    # UMP source provenance belongs to the engineering import boundary, not to
    # the desktop window. Preserve the raw legacy number and explicitly record
    # that no producer-side scale factor is proven.
    if any(mass.ump for mass in project.masses):
        legacy_metadata["ump_unit_audit"] = legacy_ump_provenance(global_ump)
        warning = (
            "UMP legado: a unidade de origem de ump_crg não está comprovada. "
            "O valor foi preservado sem reescala automática; no RotorDin atual "
            "o campo é rigidez distribuída k' [N/m²]."
        )
        if warning not in warnings:
            warnings.append(warning)

    project.settings["legacy_import"] = legacy_metadata
    return project


__all__ = ["LegacyImportError", "load_legacy_project"]
