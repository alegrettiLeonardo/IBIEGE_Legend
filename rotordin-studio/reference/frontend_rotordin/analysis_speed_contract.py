from __future__ import annotations

"""Operating-speed and analysis-speed contract for RotorDin Desktop.

The legacy iRdin format mixes several different notions of speed in the same
screen even though the native RotorDin input already has independent fields:

* ``nnom`` / ``NRAT``: nominal machine speed;
* ``nini`` / ``nfin``: machine operating range when present in legacy files;
* ``c_rpmi`` / ``c_rpmf`` -> ``CPBIS`` / ``CPBFS``: Campbell analysis range;
* ``d_rpmi`` / ``d_rpmf`` -> ``UNBIF`` / ``UNBFF``: response analysis range;
* ``t_rpm`` -> ``TIMSPD``: single-speed harmonic time/orbit evaluation.

The historical Python model stores ``c_rpmi/c_rpmf`` in
``Identification.rpm_initial/rpm_final``.  Removing those fields would break
saved projects, fixtures and the fixed-width solver contract.  This module
therefore keeps that storage as a compatibility detail while exposing the
machine operating range separately in ``Project.settings``.  Solver input is
then rewritten explicitly from the authoritative analysis values so one range
can no longer silently overwrite another.

No RotorDin Fortran equation is changed by this module.
"""

from dataclasses import dataclass
import math
from pathlib import Path
from typing import Any
import warnings


_CONTRACT_KEY = "analysis_speed_contract"
_MACHINE_KEY = "machine_operating_speed"
_VERSION = 1
ORBIT_SPEED_SINGLE = "single"
ORBIT_SPEED_RESPONSE_SWEEP = "response_sweep"
_ORBIT_SPEED_MODES = {ORBIT_SPEED_SINGLE, ORBIT_SPEED_RESPONSE_SWEEP}
ORBIT_SECTION_ALL = "all_sections"
ORBIT_SECTION_POSITION = "position"
_ORBIT_SECTION_MODES = {ORBIT_SECTION_ALL, ORBIT_SECTION_POSITION}
_installed = False


@dataclass(frozen=True, slots=True)
class LegacyResponseGrid:
    """Resolved modern response grid for one legacy iRdin d_div contract."""

    effective_initial_rpm: float
    effective_final_rpm: float
    response_step_rpm: float
    number_of_points: int
    clipped_by_bearing_table: bool
    raw_initial_rpm: float
    raw_final_rpm: float
    raw_d_div: float


def resolve_legacy_response_grid(
    raw_initial_rpm: float,
    raw_final_rpm: float,
    raw_d_div: float,
    bearing_table_range: tuple[float, float] | None = None,
) -> LegacyResponseGrid:
    """Translate the historical division parameter into the modern UNBFD step.

    The VB6/iRdin response contract used d_div as a division parameter, not as
    an rpm increment. The historical grid interval is
    (rpm_final-rpm_initial)/(d_div+1). When speed-dependent TABLE bearings
    restrict the usable range, the same historical division parameter is applied to
    the effective intersection so the endpoint convention remains auditable.
    """

    raw_i = float(raw_initial_rpm)
    raw_f = float(raw_final_rpm)
    raw_div = float(raw_d_div)
    if not all(math.isfinite(value) for value in (raw_i, raw_f, raw_div)):
        raise ValueError("A grade legada de resposta contém valor não finito.")
    if raw_i < 0.0 or raw_f <= raw_i:
        raise ValueError(
            f"Faixa legada de resposta inválida: {raw_i:g}–{raw_f:g} rpm."
        )
    if raw_div < 0.0:
        raise ValueError(f"d_div legado deve ser não negativo; recebido {raw_div:g}.")

    effective_i, effective_f = raw_i, raw_f
    clipped = False
    if bearing_table_range is not None:
        table_i, table_f = map(float, bearing_table_range)
        if not all(math.isfinite(value) for value in (table_i, table_f)) or table_f < table_i:
            raise ValueError("Faixa comum TABLE inválida para a resposta legada.")
        effective_i = max(raw_i, table_i)
        effective_f = min(raw_f, table_f)
        if effective_f <= effective_i:
            raise ValueError(
                "A faixa legada de resposta não possui interseção positiva com a faixa comum TABLE."
            )
        clipped = abs(effective_i - raw_i) > 1.0e-9 or abs(effective_f - raw_f) > 1.0e-9

    step = (effective_f - effective_i) / (raw_div + 1.0)
    if not math.isfinite(step) or step <= 0.0:
        raise ValueError("A conversão de d_div produziu passo de resposta inválido.")
    points = int(math.floor((effective_f - effective_i) / step + 1.0e-9)) + 1

    return LegacyResponseGrid(
        effective_initial_rpm=effective_i,
        effective_final_rpm=effective_f,
        response_step_rpm=step,
        number_of_points=points,
        clipped_by_bearing_table=clipped,
        raw_initial_rpm=raw_i,
        raw_final_rpm=raw_f,
        raw_d_div=raw_div,
    )


def _number(value: Any, default: float = 0.0) -> float:
    try:
        text = str(value if value is not None else "").strip()
        if not text:
            return float(default)
        text = text.replace("D", "E").replace("d", "e")
        if "," in text and "." not in text:
            text = text.replace(",", ".")
        return float(text)
    except (TypeError, ValueError):
        return float(default)


def _analysis_settings(project) -> dict[str, Any]:
    data = project.settings.setdefault(_CONTRACT_KEY, {})
    data.setdefault("version", _VERSION)
    return data


def has_explicit_analysis_speed_contract(project) -> bool:
    data = project.settings.get(_CONTRACT_KEY)
    return isinstance(data, dict) and (
        "campbell_initial_rpm" in data or "campbell_final_rpm" in data
    )


def set_campbell_speed_range(project, initial_rpm: float, final_rpm: float) -> None:
    """Set the Campbell range and keep the historical Identification mirror aligned.

    ``Identification.rpm_initial/rpm_final`` were the public Python storage for
    ``c_rpmi/c_rpmf`` before operating and analysis speeds were separated.  Keeping
    that mirror synchronized preserves direct API/tests and saved-project
    compatibility while ``Project.settings`` remains the explicit new contract.
    """

    initial = float(initial_rpm)
    final = float(final_rpm)
    data = _analysis_settings(project)
    data["campbell_initial_rpm"] = initial
    data["campbell_final_rpm"] = final
    project.identification.rpm_initial = initial
    project.identification.rpm_final = final


def campbell_speed_range(project) -> tuple[float, float]:
    data = project.settings.get(_CONTRACT_KEY, {})
    if isinstance(data, dict) and (
        "campbell_initial_rpm" in data or "campbell_final_rpm" in data
    ):
        return (
            _number(data.get("campbell_initial_rpm"), project.identification.rpm_initial),
            _number(data.get("campbell_final_rpm"), project.identification.rpm_final),
        )
    # Compatibility with projects saved before this contract existed.
    return (
        float(project.identification.rpm_initial),
        float(project.identification.rpm_final),
    )


def set_machine_operating_range(project, initial_rpm: float, final_rpm: float) -> None:
    project.settings[_MACHINE_KEY] = {
        "version": _VERSION,
        "initial_rpm": float(initial_rpm),
        "final_rpm": float(final_rpm),
    }


def has_explicit_machine_operating_range(project) -> bool:
    return isinstance(project.settings.get(_MACHINE_KEY), dict)


def machine_operating_range(project) -> tuple[float, float]:
    data = project.settings.get(_MACHINE_KEY)
    if isinstance(data, dict):
        return _number(data.get("initial_rpm")), _number(data.get("final_rpm"))

    # Old imported projects preserved nini/nfin in unmapped_dados.  Read them
    # non-destructively so opening an existing SQLite project immediately gains
    # the separated operating range without losing its audit trail.
    legacy = project.settings.get("legacy_import", {})
    unmapped = legacy.get("unmapped_dados", {}) if isinstance(legacy, dict) else {}
    if isinstance(unmapped, dict) and ("nini" in unmapped or "nfin" in unmapped):
        return _number(unmapped.get("nini")), _number(unmapped.get("nfin"))

    # Identification.rpm_initial/rpm_final are a persistence mirror of Campbell,
    # never authority for the machine operating envelope.  Without nini/nfin or
    # an explicit machine contract the operating range is genuinely unspecified.
    return 0.0, 0.0


def orbit_speed_mode(project) -> str:
    """Return the explicit orbit speed source; old projects remain single-speed."""

    data = project.settings.get(_CONTRACT_KEY, {})
    value = (
        str(data.get("orbit_speed_mode", ORBIT_SPEED_SINGLE)).strip().casefold()
        if isinstance(data, dict)
        else ORBIT_SPEED_SINGLE
    )
    return value if value in _ORBIT_SPEED_MODES else ORBIT_SPEED_SINGLE


def set_orbit_speed_mode(project, mode: str) -> None:
    value = str(mode).strip().casefold()
    if value not in _ORBIT_SPEED_MODES:
        raise ValueError(f"Modo de rotação da órbita inválido: {mode}")
    _analysis_settings(project)["orbit_speed_mode"] = value


def resolved_orbit_speed_rpm(project) -> float:
    if orbit_speed_mode(project) == ORBIT_SPEED_RESPONSE_SWEEP:
        return 0.0
    speed = float(project.configuration.orbit_speed_rpm)
    if speed > 0.0:
        return speed
    nominal = float(project.identification.rpm_nominal)
    return nominal if nominal > 0.0 else 0.0


def orbit_section_mode(project) -> str:
    """Return the explicit orbit section source with legacy-safe inference.

    Historical projects used TIMOEP=0 as the native sentinel for speed-shape
    output at every section. Preserve that meaning when no explicit modern
    section-mode setting exists.
    """

    data = project.settings.get(_CONTRACT_KEY, {})
    if isinstance(data, dict):
        value = str(data.get("orbit_section_mode", "")).strip().casefold()
        if value in _ORBIT_SECTION_MODES:
            return value
    return (
        ORBIT_SECTION_ALL
        if abs(float(project.configuration.orbit_position_mm)) <= 1.0e-12
        else ORBIT_SECTION_POSITION
    )


def set_orbit_section_mode(project, mode: str) -> None:
    value = str(mode).strip().casefold()
    if value not in _ORBIT_SECTION_MODES:
        raise ValueError(f"Modo de seção da órbita inválido: {mode}")
    _analysis_settings(project)["orbit_section_mode"] = value


def resolved_orbit_position_mm(project) -> float:
    if orbit_section_mode(project) == ORBIT_SECTION_ALL:
        return 0.0
    return float(project.configuration.orbit_position_mm)


def _bearing_file_kind(path: str | Path) -> str | None:
    """Return the native bearing-file kind without interpreting coefficients."""

    source = Path(path).expanduser()
    if not source.is_file():
        return None
    try:
        raw = source.read_text(encoding="utf-8-sig")
    except UnicodeDecodeError:
        raw = source.read_text(encoding="latin-1")
    first = next(
        (
            line.strip().upper()
            for line in raw.replace("\r\n", "\n").replace("\r", "\n").splitlines()
            if line.strip()
        ),
        "",
    )
    if first.startswith("TABLE"):
        return "TABLE"
    if first.startswith("COEF"):
        return "COEF"
    return None


def bearing_file_kinds(project) -> set[str]:
    return {
        kind
        for bearing in project.bearings
        if bearing.file.strip()
        for kind in [_bearing_file_kind(bearing.file)]
        if kind is not None
    }


def resolved_modal_speed_rpm(project) -> float:
    """Resolve MDRPM=0 for variable-bearing modal analyses without mutation."""

    speed = float(project.configuration.modal_rpm)
    if speed != 0.0:
        return speed
    nominal = float(project.identification.rpm_nominal)
    if nominal > 0.0:
        return nominal
    camp_initial, _ = campbell_speed_range(project)
    return camp_initial if camp_initial > 0.0 else 0.0


def _table_speed_range(path: str | Path) -> tuple[float, float] | None:
    source = Path(path).expanduser()
    if not source.is_file():
        return None
    try:
        raw = source.read_text(encoding="utf-8-sig")
    except UnicodeDecodeError:
        raw = source.read_text(encoding="latin-1")
    lines = [line.strip() for line in raw.replace("\r\n", "\n").replace("\r", "\n").splitlines() if line.strip()]
    if not lines or not lines[0].upper().startswith("TABLE"):
        return None

    speeds: list[float] = []
    # TABLE files use one metadata/count row after the title.  Everything until
    # EOF that begins with a number is a native rpm/K/C row.
    for line in lines[2:]:
        if line.upper() == "EOF":
            break
        fields = line.replace(",", ".").split()
        if not fields:
            continue
        try:
            speeds.append(float(fields[0].replace("D", "E").replace("d", "e")))
        except ValueError:
            continue
    if not speeds:
        return None
    return min(speeds), max(speeds)


def common_bearing_table_speed_range(project) -> tuple[float, float] | None:
    ranges = [
        speed_range
        for bearing in project.bearings
        if bearing.file.strip()
        for speed_range in [_table_speed_range(bearing.file)]
        if speed_range is not None
    ]
    if not ranges:
        return None
    lower = max(item[0] for item in ranges)
    upper = min(item[1] for item in ranges)
    return lower, upper


def _patch_data_line(text: str, header_prefix: str, replacements: dict[int, str]) -> str:
    lines = text.splitlines()
    header_index = next(
        (index for index, line in enumerate(lines) if line.startswith(header_prefix)),
        -1,
    )
    if header_index < 0:
        return text
    data_index = next(
        (index for index in range(header_index + 1, len(lines)) if lines[index].strip()),
        -1,
    )
    if data_index < 0:
        return text

    line = lines[data_index]
    required = max((field + 1) * 10 for field in replacements)
    if len(line) < required:
        line = line.ljust(required)
    chars = list(line)
    for field, value in replacements.items():
        start = field * 10
        token = value[-10:].rjust(10)
        chars[start:start + 10] = list(token)
    lines[data_index] = "".join(chars)
    return "\n".join(lines) + ("\n" if text.endswith("\n") else "")


def _install_legacy_import_contract() -> None:
    from . import legacy_import

    current = legacy_import.load_legacy_project
    if getattr(current, "_analysis_speed_contract", False):
        return

    def load_legacy_project(path, *, asset_root=None):
        project = current(path, asset_root=asset_root)

        # c_rpmi/c_rpmf are already held in Identification by the historical
        # importer.  Preserve them explicitly as Campbell analysis data.
        set_campbell_speed_range(
            project,
            project.identification.rpm_initial,
            project.identification.rpm_final,
        )

        legacy = project.settings.get("legacy_import", {})
        unmapped = legacy.get("unmapped_dados", {}) if isinstance(legacy, dict) else {}
        if isinstance(unmapped, dict) and ("nini" in unmapped or "nfin" in unmapped):
            set_machine_operating_range(
                project,
                _number(unmapped.get("nini")),
                _number(unmapped.get("nfin")),
            )

        raw_speed = legacy.get("raw_speed_fields", {}) if isinstance(legacy, dict) else {}
        raw_response_complete = (
            isinstance(raw_speed, dict)
            and all(key in raw_speed for key in ("d_rpmi", "d_rpmf", "d_div"))
            and all(str(raw_speed.get(key, "")).strip() for key in ("d_rpmi", "d_rpmf", "d_div"))
        )
        if raw_response_complete:
            resolved = resolve_legacy_response_grid(
                _number(raw_speed.get("d_rpmi")),
                _number(raw_speed.get("d_rpmf")),
                _number(raw_speed.get("d_div")),
                common_bearing_table_speed_range(project),
            )
            project.configuration.response_initial_rpm = resolved.effective_initial_rpm
            project.configuration.response_final_rpm = resolved.effective_final_rpm
            project.configuration.response_step_rpm = resolved.response_step_rpm
            project.settings["legacy_response_grid"] = {
                "version": 2,
                "raw_d_rpmi": resolved.raw_initial_rpm,
                "raw_d_rpmf": resolved.raw_final_rpm,
                "raw_d_div": resolved.raw_d_div,
                "effective_initial_rpm": resolved.effective_initial_rpm,
                "effective_final_rpm": resolved.effective_final_rpm,
                "converted_step_rpm": resolved.response_step_rpm,
                "number_of_points": resolved.number_of_points,
                "source_semantics": "legacy_unbfd_division_parameter",
                "bearing_table_clipped": resolved.clipped_by_bearing_table,
            }

        # t_rpm=0 is not a valid one-speed resp_t request.  The nominal machine
        # speed is the legacy-safe default, but remains independently editable in
        # the analysis configuration after import.
        if (
            project.configuration.orbit_speed_rpm <= 0.0
            and project.identification.rpm_nominal > 0.0
        ):
            project.configuration.orbit_speed_rpm = project.identification.rpm_nominal
        return project

    load_legacy_project._analysis_speed_contract = True
    load_legacy_project._analysis_speed_original = current
    legacy_import.load_legacy_project = load_legacy_project


def _install_project_dump_contract() -> None:
    """Serialize explicit speed semantics without mutating the live Project."""

    from . import domain

    current = domain.Project.to_dict
    if getattr(current, "_analysis_speed_contract", False):
        return

    def to_dict(self):
        data = current(self)
        settings = data.setdefault("settings", {})

        analysis = settings.get(_CONTRACT_KEY)
        if not isinstance(analysis, dict):
            analysis = {}
            settings[_CONTRACT_KEY] = analysis
        analysis.setdefault("version", _VERSION)
        camp_initial, camp_final = campbell_speed_range(self)
        analysis["campbell_initial_rpm"] = float(camp_initial)
        analysis["campbell_final_rpm"] = float(camp_final)
        analysis["orbit_speed_mode"] = orbit_speed_mode(self)
        analysis["orbit_section_mode"] = orbit_section_mode(self)

        machine = settings.get(_MACHINE_KEY)
        if not isinstance(machine, dict):
            machine = {}
            settings[_MACHINE_KEY] = machine
        machine.setdefault("version", _VERSION)
        op_initial, op_final = machine_operating_range(self)
        machine["initial_rpm"] = float(op_initial)
        machine["final_rpm"] = float(op_final)

        return data

    to_dict._analysis_speed_contract = True
    to_dict._analysis_speed_original = current
    domain.Project.to_dict = to_dict


def _legacy_response_fields_from_source(path_value: Any) -> dict[str, str] | None:
    """Recover d_rpmi/d_rpmf/d_div from the original iRdin [Dados] block.

    Older SQLite projects did not persist raw response-speed provenance.  When the
    original source path is still available, recover only these three historical
    fields; bearing physics and all other project data remain the persisted model.
    """

    text: str | None = None
    source = Path(str(path_value or "")).expanduser()
    if not source.is_file():
        return None
    for encoding in ("utf-8-sig", "cp1252", "latin-1"):
        try:
            text = source.read_text(encoding=encoding)
            break
        except UnicodeDecodeError:
            continue
        except OSError:
            return None
    if text is None:
        return None

    values: dict[str, str] = {}
    in_dados = False
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        if line.startswith("[") and line.endswith("]"):
            in_dados = line.casefold() == "[dados]"
            continue
        if not in_dados or "=" not in line:
            continue
        key, value = line.split("=", 1)
        key = key.strip().casefold()
        if key in {"d_rpmi", "d_rpmf", "d_div"}:
            values[key] = value.strip()
    if all(values.get(key, "").strip() for key in ("d_rpmi", "d_rpmf", "d_div")):
        return values
    return None


def _install_project_load_migration() -> None:
    from . import domain

    current = domain.Project.from_dict
    if getattr(current, "_analysis_speed_contract", False):
        return

    @classmethod
    def from_dict(cls, data):
        project = current(data)
        if not has_explicit_analysis_speed_contract(project):
            set_campbell_speed_range(
                project,
                project.identification.rpm_initial,
                project.identification.rpm_final,
            )
        if not has_explicit_machine_operating_range(project):
            op_initial, op_final = machine_operating_range(project)
            set_machine_operating_range(project, op_initial, op_final)

        provenance = project.settings.get("legacy_response_grid")
        if isinstance(provenance, dict) and provenance.get("source_semantics") == "legacy_unbfd_division_parameter":
            raw_i = _number(provenance.get("raw_d_rpmi"))
            raw_f = _number(provenance.get("raw_d_rpmf"))
            raw_div = _number(provenance.get("raw_d_div"))
            effective_i = _number(provenance.get("effective_initial_rpm"), raw_i)
            effective_f = _number(provenance.get("effective_final_rpm"), raw_f)
            resolved = resolve_legacy_response_grid(
                raw_i, raw_f, raw_div, (effective_i, effective_f)
            )
            project.configuration.response_initial_rpm = effective_i
            project.configuration.response_final_rpm = effective_f
            project.configuration.response_step_rpm = resolved.response_step_rpm
            provenance["converted_step_rpm"] = resolved.response_step_rpm
            provenance["number_of_points"] = resolved.number_of_points
        elif isinstance(project.settings.get("legacy_import"), dict):
            legacy = project.settings["legacy_import"]
            raw_speed = legacy.get("raw_speed_fields", {})
            raw_complete = (
                isinstance(raw_speed, dict)
                and all(str(raw_speed.get(key, "")).strip() for key in ("d_rpmi", "d_rpmf", "d_div"))
            )
            if not raw_complete:
                recovered = _legacy_response_fields_from_source(legacy.get("source_path"))
                if recovered is not None:
                    raw_speed = recovered
                    legacy["raw_speed_fields"] = dict(recovered)
                    raw_complete = True

            if raw_complete:
                try:
                    resolved = resolve_legacy_response_grid(
                        _number(raw_speed["d_rpmi"]),
                        _number(raw_speed["d_rpmf"]),
                        _number(raw_speed["d_div"]),
                        common_bearing_table_speed_range(project),
                    )
                except ValueError:
                    raw_complete = False
                else:
                    project.configuration.response_initial_rpm = resolved.effective_initial_rpm
                    project.configuration.response_final_rpm = resolved.effective_final_rpm
                    project.configuration.response_step_rpm = resolved.response_step_rpm
                    project.settings["legacy_response_grid"] = {
                        "version": 2,
                        "raw_d_rpmi": resolved.raw_initial_rpm,
                        "raw_d_rpmf": resolved.raw_final_rpm,
                        "raw_d_div": resolved.raw_d_div,
                        "effective_initial_rpm": resolved.effective_initial_rpm,
                        "effective_final_rpm": resolved.effective_final_rpm,
                        "converted_step_rpm": resolved.response_step_rpm,
                        "number_of_points": resolved.number_of_points,
                        "source_semantics": "legacy_unbfd_division_parameter",
                        "bearing_table_clipped": resolved.clipped_by_bearing_table,
                    }
                    legacy["response_grid_status"] = "RECOVERED_FROM_LEGACY_SOURCE"

            if not raw_complete:
                message = (
                    "A semântica histórica de d_div/UNBFD não pode ser determinada "
                    "com segurança neste projeto. Reimporte o arquivo iRdin original "
                    "antes de executar a resposta dinâmica."
                )
                legacy["response_grid_status"] = "AMBIGUOUS_REIMPORT_REQUIRED"
                warning_list = legacy.setdefault("warnings", [])
                if message not in warning_list:
                    warning_list.append(message)
                warnings.warn(message, RuntimeWarning, stacklevel=2)
        return project

    from_dict._analysis_speed_contract = True
    from_dict._analysis_speed_original = current
    domain.Project.from_dict = from_dict


def _install_solver_input_contract() -> None:
    from . import solver

    current = solver.SolverInputWriter.render
    if getattr(current, "_analysis_speed_contract", False):
        return

    def render(self, project):
        camp_initial, camp_final = campbell_speed_range(project)
        response_initial = float(project.configuration.response_initial_rpm)
        response_final = float(project.configuration.response_final_rpm)
        orbit_mode = orbit_speed_mode(project)
        orbit_sweep = orbit_mode == ORBIT_SPEED_RESPONSE_SWEEP
        orbit_speed = resolved_orbit_speed_rpm(project)
        section_mode = orbit_section_mode(project)
        all_sections = section_mode == ORBIT_SECTION_ALL
        orbit_position_mm = resolved_orbit_position_mm(project)
        response_grid_required = bool(
            project.analyses.dynamic_response
            or (project.analyses.orbit and orbit_sweep)
        )
        modal_speed = resolved_modal_speed_rpm(project)
        bearing_kinds = bearing_file_kinds(project)
        has_variable_bearings = bool(bearing_kinds)

        if project.analyses.campbell or project.analyses.log_decrement:
            if camp_initial < 0.0 or camp_final <= camp_initial:
                raise solver.SolverError(
                    "Campbell: faixa de rotação inválida. "
                    f"Recebido {camp_initial:g}–{camp_final:g} rpm; esperado "
                    "início >= 0 e fim > início."
                )

        if response_grid_required:
            legacy = project.settings.get("legacy_import")
            provenance = project.settings.get("legacy_response_grid")
            if (
                isinstance(legacy, dict)
                and not (
                    isinstance(provenance, dict)
                    and provenance.get("source_semantics")
                    == "legacy_unbfd_division_parameter"
                )
            ):
                context = (
                    "Resposta dinâmica"
                    if project.analyses.dynamic_response
                    else "Órbita em faixa"
                )
                raise solver.SolverError(
                    f"{context} bloqueada: este projeto legado não possui "
                    "proveniência suficiente para distinguir d_div histórico de "
                    "UNBFD [rpm]. Reimporte o arquivo iRdin original antes de executar."
                )
            if response_initial < 0.0 or response_final <= response_initial:
                raise solver.SolverError(
                    "Resposta em frequência: faixa de rotação inválida. "
                    f"Recebido {response_initial:g}–{response_final:g} rpm; "
                    "esperado início >= 0 e fim > início."
                )
            response_step = float(project.configuration.response_step_rpm)
            if response_step <= 0.0:
                raise solver.SolverError(
                    "Resposta em frequência: UNBFD deve ser maior que zero. "
                    f"Recebido {response_step:g} rpm."
                )
            # resp_f/resp_fv use mtr=2*mtg=1000 speed samples.  Reject the
            # request before the native loop can overrun its historical output
            # capacity; use the same inclusive grid convention as the solver.
            response_points = int(
                (response_final - response_initial) / response_step + 1.0e-9
            ) + 1
            if response_points > 1000:
                raise solver.SolverError(
                    "Resposta em frequência: número de pontos excede a capacidade "
                    "nativa do RotorDin. "
                    f"Recebido {response_points} pontos para "
                    f"{response_initial:g}–{response_final:g} rpm com "
                    f"UNBFD={response_step:g} rpm; esperado no máximo 1000."
                )

        if (
            project.analyses.orbit
            and not all_sections
            and abs(orbit_position_mm) <= 1.0e-12
        ):
            raise solver.SolverError(
                "Órbita em posição específica exige TIMOEP diferente de zero. "
                "Selecione 'Todas as seções' para usar o sentinela nativo TIMOEP=0."
            )

        if project.analyses.orbit and not orbit_sweep and orbit_speed <= 0.0:
            raise solver.SolverError(
                "Órbita em uma rotação: TIMSPD deve ser maior que zero. Informe a "
                "rotação de órbita na configuração da análise ou uma rotação nominal."
            )

        if has_variable_bearings and project.analyses.modes and modal_speed <= 0.0:
            raise solver.SolverError(
                "Modos com mancais dependentes da rotação exigem MDRPM > 0. "
                "Informe a rotação modal, a rotação nominal ou uma rotação inicial "
                "de Campbell positiva."
            )

        # COEF evaluates y=a(rpm)/rpm natively, so every analysis that actually
        # consumes those coefficients must stay strictly above zero rpm.
        if "COEF" in bearing_kinds:
            if (
                (project.analyses.campbell or project.analyses.log_decrement)
                and camp_initial <= 0.0
            ):
                raise solver.SolverError(
                    "Campbell com arquivo COEF exige rotação inicial maior que zero."
                )
            if project.analyses.dynamic_response and response_initial <= 0.0:
                raise solver.SolverError(
                    "Resposta em frequência com arquivo COEF exige rotação inicial "
                    "maior que zero."
                )
            if project.analyses.modes and modal_speed <= 0.0:
                raise solver.SolverError(
                    "Modos com arquivo COEF exigem MDRPM maior que zero."
                )
            if project.analyses.orbit:
                if orbit_sweep and response_initial <= 0.0:
                    raise solver.SolverError(
                        "Órbita em faixa com arquivo COEF exige UNBIF maior que zero."
                    )
                if not orbit_sweep and orbit_speed <= 0.0:
                    raise solver.SolverError(
                        "Órbita com arquivo COEF exige TIMSPD maior que zero."
                    )
            if (
                project.analyses.line
                and project.configuration.static_bearing_stiffness_ratio > 0.0
                and (
                    project.identification.rpm_nominal
                    * project.configuration.static_bearing_stiffness_ratio
                    <= 0.0
                )
            ):
                raise solver.SolverError(
                    "Linha elástica com RKS > 0 e arquivo COEF exige rotação nominal "
                    "positiva para avaliar os coeficientes do mancal."
                )

        table_range = common_bearing_table_speed_range(project)
        if table_range is not None:
            lower, upper = table_range
            if lower > upper:
                raise solver.SolverError(
                    "As tabelas dos mancais não possuem uma faixa de rotação comum."
                )

            if (
                (project.analyses.campbell or project.analyses.log_decrement)
                and (
                    camp_initial < lower - 1.0e-9
                    or camp_final > upper + 1.0e-9
                )
            ):
                raise solver.SolverError(
                    "Campbell: "
                    f"faixa {camp_initial:g}–{camp_final:g} rpm fora da faixa comum "
                    f"das tabelas de mancais ({lower:g}–{upper:g} rpm)."
                )

            if response_grid_required and (
                response_initial < lower - 1.0e-9
                or response_final > upper + 1.0e-9
            ):
                context = (
                    "Resposta em frequência"
                    if project.analyses.dynamic_response
                    else "Órbita em faixa"
                )
                raise solver.SolverError(
                    f"{context}: faixa {response_initial:g}–{response_final:g} rpm "
                    f"fora da faixa comum das tabelas de mancais "
                    f"({lower:g}–{upper:g} rpm). Ajuste a faixa ou use tabelas que "
                    "cubram toda a análise."
                )

            if project.analyses.modes and (
                modal_speed < lower - 1.0e-9 or modal_speed > upper + 1.0e-9
            ):
                raise solver.SolverError(
                    f"Modos: MDRPM {modal_speed:g} rpm fora da faixa comum das "
                    f"tabelas de mancais ({lower:g}–{upper:g} rpm)."
                )

            if project.analyses.orbit and not orbit_sweep and (
                orbit_speed < lower - 1.0e-9 or orbit_speed > upper + 1.0e-9
            ):
                raise solver.SolverError(
                    f"Órbita: rotação {orbit_speed:g} rpm fora da faixa comum das "
                    f"tabelas de mancais ({lower:g}–{upper:g} rpm)."
                )

            if (
                project.analyses.line
                and project.configuration.static_bearing_stiffness_ratio > 0.0
            ):
                static_speed = (
                    project.identification.rpm_nominal
                    * project.configuration.static_bearing_stiffness_ratio
                )
                if static_speed < lower - 1.0e-9 or static_speed > upper + 1.0e-9:
                    raise solver.SolverError(
                        "Linha elástica: rotação de avaliação do mancal "
                        f"{static_speed:g} rpm fora da faixa comum das tabelas "
                        f"({lower:g}–{upper:g} rpm)."
                    )

        text = current(self, project)

        # Campbell: only CPBIS/CPBFS are separated here. SMSPD keeps the
        # established solver fallback (explicit map speed, otherwise the valid
        # bearing-analysis operating point) so this contract does not change
        # bearing-table map behavior.
        text = _patch_data_line(
            text,
            "CPBIS     CPBFS",
            {
                0: solver._f10(camp_initial),
                1: solver._f10(camp_final),
            },
        )

        # Dynamic response: UNBIF/UNBFF come from SolverConfiguration and no
        # longer inherit c_rpmi/c_rpmf through Identification.
        text = _patch_data_line(
            text,
            "EXCT      UNBIF",
            {
                1: solver._f10(response_initial),
                2: solver._f10(response_final),
                **({6: solver._f10(modal_speed)} if has_variable_bearings and modal_speed > 0.0 else {}),
            },
        )

        # BEAFILES carries the same native MDRPM common value. Keep both legacy
        # locations synchronized when a zero UI value resolves to nominal/start.
        if has_variable_bearings and modal_speed > 0.0:
            text = _patch_data_line(
                text,
                "BEAFILES  MINAMP    MDRPM",
                {2: solver._f10(modal_speed)},
            )

        # Orbit/time-harmonic response has two independent selectors:
        # speed: single TIMSPD>0 or response-grid sweep TIMSPD=0;
        # section: one explicit TIMOEP or all native sections TIMOEP=0.
        # The four combinations are resolved here without mutating Project.
        if project.analyses.orbit:
            text = _patch_data_line(
                text,
                "FRESPPOS  NBRMOD",
                {
                    2: solver._f10(
                        0.0 if all_sections else orbit_position_mm / 1000.0
                    ),
                    3: solver._f10(0.0 if orbit_sweep else orbit_speed),
                },
            )
        return text

    render._analysis_speed_contract = True
    render._analysis_speed_original = current
    solver.SolverInputWriter.render = render


def install_analysis_speed_contract() -> None:
    """Install speed-separation behavior once for the whole frontend process."""

    global _installed
    if _installed:
        return
    _install_legacy_import_contract()
    _install_project_dump_contract()
    _install_project_load_migration()
    _install_solver_input_contract()
    _installed = True


__all__ = [
    "bearing_file_kinds",
    "campbell_speed_range",
    "common_bearing_table_speed_range",
    "has_explicit_analysis_speed_contract",
    "has_explicit_machine_operating_range",
    "install_analysis_speed_contract",
    "LegacyResponseGrid",
    "machine_operating_range",
    "orbit_section_mode",
    "orbit_speed_mode",
    "ORBIT_SECTION_ALL",
    "ORBIT_SECTION_POSITION",
    "ORBIT_SPEED_RESPONSE_SWEEP",
    "ORBIT_SPEED_SINGLE",
    "resolve_legacy_response_grid",
    "resolved_modal_speed_rpm",
    "resolved_orbit_position_mm",
    "resolved_orbit_speed_rpm",
    "set_campbell_speed_range",
    "set_machine_operating_range",
    "set_orbit_section_mode",
    "set_orbit_speed_mode",
]
