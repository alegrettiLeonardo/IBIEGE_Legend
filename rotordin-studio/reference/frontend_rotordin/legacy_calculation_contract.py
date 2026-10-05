from __future__ import annotations

"""Compatibility contract for the calculation fields exposed by legacy iRdin.

The VB6 calculation panel mixed three kinds of data:

* native RotorDin input fields (Campbell, response, orbit, stiffness map, etc.);
* geometry/material translation fields (disk density and package divisions);
* presentation-only graph preferences (``gr_*`` keys).

The first two must feed the scientific input exactly.  The graph preferences are
preserved in ``Project.settings`` because they do not belong to the Fortran
physical model.  This keeps old files auditable without inventing new solver
physics.
"""

from typing import Any


_SETTINGS_KEY = "legacy_calculation_settings"
_VERSION = 1
_installed = False


_GRAPH_DEFAULTS: dict[str, Any] = {
    "marker": False,
    "scale": 1.0,
    "x0": 100.0,
    "response_component": 7,
    "modes": 5,
    "map_count": 4,
    "log_count": 2,
}


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


def _integer(value: Any, default: int = 0) -> int:
    number = _number(value, float(default))
    return int(round(number))


def _boolean(value: Any, default: bool = False) -> bool:
    if value is None:
        return bool(default)
    text = str(value).strip().casefold()
    if text in {"true", "yes", "sim", "on"}:
        return True
    if text in {"false", "no", "não", "nao", "off", ""}:
        return False
    return bool(_integer(value, int(default)))


def _legacy_unmapped(project) -> dict[str, Any]:
    legacy = project.settings.get("legacy_import", {})
    if not isinstance(legacy, dict):
        return {}
    data = legacy.get("unmapped_dados", {})
    return data if isinstance(data, dict) else {}


def legacy_graph_settings(project) -> dict[str, Any]:
    """Return persisted graph preferences, with old INI keys as migration fallback."""

    stored = project.settings.get(_SETTINGS_KEY, {})
    stored = stored if isinstance(stored, dict) else {}
    graph = stored.get("graphics", {})
    graph = graph if isinstance(graph, dict) else {}
    legacy = _legacy_unmapped(project)

    def first(name: str, legacy_key: str, default: Any) -> Any:
        if name in graph:
            return graph[name]
        if legacy_key in legacy:
            return legacy[legacy_key]
        return default

    return {
        "marker": _boolean(first("marker", "gr_marker", _GRAPH_DEFAULTS["marker"])),
        "scale": _number(first("scale", "gr_esc", _GRAPH_DEFAULTS["scale"]), _GRAPH_DEFAULTS["scale"]),
        "x0": _number(first("x0", "gr_x0", _GRAPH_DEFAULTS["x0"]), _GRAPH_DEFAULTS["x0"]),
        "response_component": _integer(
            first("response_component", "gr_rcp", _GRAPH_DEFAULTS["response_component"]),
            _GRAPH_DEFAULTS["response_component"],
        ),
        "modes": _integer(first("modes", "gr_mod", _GRAPH_DEFAULTS["modes"]), _GRAPH_DEFAULTS["modes"]),
        "map_count": _integer(first("map_count", "gr_map", _GRAPH_DEFAULTS["map_count"]), _GRAPH_DEFAULTS["map_count"]),
        "log_count": _integer(first("log_count", "gr_log", _GRAPH_DEFAULTS["log_count"]), _GRAPH_DEFAULTS["log_count"]),
    }


def set_legacy_graph_settings(project, **values: Any) -> None:
    root = project.settings.setdefault(_SETTINGS_KEY, {})
    root["version"] = _VERSION
    graph = root.setdefault("graphics", {})
    for key in _GRAPH_DEFAULTS:
        if key in values:
            graph[key] = values[key]


def elastic_line_inclination_deg(project) -> float:
    """Resolve legacy ``le_inc`` to native RotorDin HANGLE semantics."""

    stored = project.settings.get(_SETTINGS_KEY, {})
    if isinstance(stored, dict) and "elastic_line_inclination_deg" in stored:
        return _number(stored["elastic_line_inclination_deg"])
    legacy = _legacy_unmapped(project)
    if "le_inc" in legacy:
        return _number(legacy["le_inc"])
    return float(project.configuration.horizontal_angle_deg)


def set_elastic_line_inclination_deg(project, value: float) -> None:
    numeric = float(value)
    project.configuration.horizontal_angle_deg = numeric
    root = project.settings.setdefault(_SETTINGS_KEY, {})
    root["version"] = _VERSION
    root["elastic_line_inclination_deg"] = numeric


def _install_legacy_import_mapping() -> None:
    from . import legacy_import

    current = legacy_import.load_legacy_project
    if getattr(current, "_legacy_calculation_contract", False):
        return

    def load_legacy_project(path, *, asset_root=None):
        project = current(path, asset_root=asset_root)
        legacy = _legacy_unmapped(project)

        # ``le_inc`` is not just presentation: it maps directly to HANGLE in the
        # native SECTIONS block and therefore must reach the Fortran solver.
        if "le_inc" in legacy:
            set_elastic_line_inclination_deg(project, _number(legacy["le_inc"]))

        graph = legacy_graph_settings(project)
        set_legacy_graph_settings(project, **graph)
        return project

    load_legacy_project._legacy_calculation_contract = True
    load_legacy_project._legacy_calculation_original = current
    legacy_import.load_legacy_project = load_legacy_project


def install_legacy_calculation_contract() -> None:
    global _installed
    if _installed:
        return
    _install_legacy_import_mapping()
    _installed = True


__all__ = [
    "elastic_line_inclination_deg",
    "install_legacy_calculation_contract",
    "legacy_graph_settings",
    "set_elastic_line_inclination_deg",
    "set_legacy_graph_settings",
]
