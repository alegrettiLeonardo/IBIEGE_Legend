from __future__ import annotations

"""Canonical engineering-unit contract for the RotorDin frontend.

This module separates *display/import* engineering units from the native units
consumed by the Fortran solver.  That distinction is essential for legacy iRdin
unbalance: the UI/file value is g·mm, while RotorDin KIND=0 consumes kg·m.

No unproven numerical conversion is applied to legacy UMP values here.
"""

from typing import Any

MM = "mm"
M = "m"
KG = "kg"
G_MM = "g·mm"
KG_M = "kg·m"
KG_M2 = "kg·m²"
N = "N"
N_M = "N·m"
N_PER_M = "N/m"
N_S_PER_M = "N·s/m"
N_PER_M2 = "N/m²"
PA = "Pa"
KG_PER_M3 = "kg/m³"
M_PER_S2 = "m/s²"
RPM = "rpm"
HZ = "Hz"
DEG = "°"
RAD_PER_S = "rad/s"

# Native RotorDin input units by excitation KIND.
_FORCE_SOLVER_UNITS: dict[int, str] = {
    -1: N_M,   # transient torque
    0: KG_M,   # unbalance m*e; solver forms F = (m*e)*omega**2
    1: N,      # concentrated lateral force
    2: N_M,    # harmonic torque
    3: N_M,    # static torque
    4: N_M,    # torsional frequency-response excitation
    5: N,      # total force on the selected shaft section
}

# Units intentionally shown/edited in the migrated iRdin UI.
# KIND=0 is the only confirmed legacy display-unit exception.
_FORCE_DISPLAY_UNITS: dict[int, str] = dict(_FORCE_SOLVER_UNITS)
_FORCE_DISPLAY_UNITS[0] = G_MM

_FORCE_QUANTITIES_PT: dict[int, str] = {
    -1: "Torque transiente",
    0: "Desbalanceamento",
    1: "Força concentrada",
    2: "Torque harmônico",
    3: "Torque estático",
    4: "Torque p/ resposta em frequência",
    5: "Força total na seção",
}


def _kind_code(kind: int | float | str) -> int | None:
    try:
        return int(float(kind))
    except (TypeError, ValueError):
        return None


def force_solver_value_unit(kind: int | float | str) -> str:
    """Return the native RotorDin input unit for an excitation KIND."""
    code = _kind_code(kind)
    return "" if code is None else _FORCE_SOLVER_UNITS.get(code, "")


def force_display_value_unit(kind: int | float | str) -> str:
    """Return the unit shown/edited by the desktop UI for an excitation KIND."""
    code = _kind_code(kind)
    return "" if code is None else _FORCE_DISPLAY_UNITS.get(code, "")


def force_quantity_pt(kind: int | float | str) -> str:
    code = _kind_code(kind)
    return "Excitação" if code is None else _FORCE_QUANTITIES_PT.get(code, "Excitação")


def force_position_unit(kind: int | float | str) -> str:
    """KIND=5 uses a section index; other excitation positions are edited in mm."""
    code = _kind_code(kind)
    return "section" if code == 5 else MM


def legacy_ump_provenance(source_value: float) -> dict[str, Any]:
    """Record the unresolved legacy UMP source scale without inventing one.

    The current RotorDin section field is treated by the frontend as distributed
    electromagnetic stiffness k' [N/m²].  The historical producer-side scale of
    ``ump_crg`` is not proven, so the raw source number is preserved and flagged.
    """
    value = float(source_value)
    return {
        "source_value": value,
        "source_unit": "legacy_unknown",
        "solver_value": value,
        "solver_interpretation": N_PER_M2,
        "automatic_scale_factor": 1.0,
        "conversion_status": "preserved_without_unproven_rescaling",
    }


def install_compatibility_unit_aliases() -> None:
    """Expose explicit UMP aliases without breaking persisted project keys."""
    from .domain import DistributedMass
    from .solver import SolverSection

    if not hasattr(DistributedMass, "ump_stiffness_n_m2"):
        DistributedMass.ump_stiffness_n_m2 = property(  # type: ignore[attr-defined]
            lambda self: self.ump_value,
            lambda self, value: setattr(self, "ump_value", float(value)),
            doc="Distributed UMP electromagnetic stiffness k' [N/m²].",
        )
    if not hasattr(SolverSection, "ump_stiffness_n_m2"):
        SolverSection.ump_stiffness_n_m2 = property(  # type: ignore[attr-defined]
            lambda self: self.ump_n_m,
            lambda self, value: setattr(self, "ump_n_m", float(value)),
            doc="Distributed UMP electromagnetic stiffness k' [N/m²].",
        )


# Backward-compatible name for presentation code created during the unit audit.
def force_value_unit(kind: int | float | str) -> str:
    return force_display_value_unit(kind)


__all__ = [
    "DEG", "G_MM", "HZ", "KG", "KG_M", "KG_M2", "KG_PER_M3", "M", "MM",
    "M_PER_S2", "N", "N_M", "N_PER_M", "N_PER_M2", "N_S_PER_M", "PA",
    "RAD_PER_S", "RPM", "force_display_value_unit", "force_position_unit",
    "force_quantity_pt", "force_solver_value_unit", "force_value_unit",
    "install_compatibility_unit_aliases", "legacy_ump_provenance",
]
