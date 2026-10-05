from __future__ import annotations

"""Install legacy iRdin package/disk and mandatory-station parity.

This module deliberately wraps the existing Python translation layer instead of
changing RotorDin's Fortran matrices.  The Fortran solver remains authoritative for
disk inertias, local shaft-diameter fallback and final mesh generation.
"""

from .solver_disks import build_solver_disks

_installed = False


def _parse_legacy_float(value: object, default: float) -> float:
    text = str(value if value is not None else "").strip()
    if not text:
        return float(default)
    text = text.replace("D", "E").replace("d", "e")
    if "," in text and "." not in text:
        text = text.replace(",", ".")
    return float(text)


def _parse_legacy_int(value: object, default: int) -> int:
    number = _parse_legacy_float(value, float(default))
    rounded = int(round(number))
    if abs(number - rounded) > 1.0e-9:
        raise ValueError(f"Valor inteiro legado inválido: {value!r}")
    return rounded


def _install_legacy_import_mapping() -> None:
    from . import legacy_import

    current = legacy_import.load_legacy_project
    if getattr(current, "_legacy_package_contract", False):
        return

    def load_legacy_project(path, *, asset_root=None):
        project = current(path, asset_root=asset_root)
        metadata = project.settings.setdefault("legacy_import", {})
        unmapped = metadata.setdefault("unmapped_dados", {})

        if "d_masesp" in unmapped:
            project.configuration.disk_density_kg_m3 = _parse_legacy_float(
                unmapped.pop("d_masesp"), project.configuration.disk_density_kg_m3
            )
        if "p_div" in unmapped:
            project.configuration.package_divisions = _parse_legacy_int(
                unmapped.pop("p_div"), project.configuration.package_divisions
            )

        # Validate the newly mapped configuration values immediately.  The original
        # importer intentionally does not run the full Project.validate() because a
        # legacy file may still need user intervention for unrelated fields.
        if project.configuration.disk_density_kg_m3 <= 0.0:
            raise legacy_import.LegacyImportError(
                "d_masesp deve ser maior que zero no cálculo legado."
            )
        if not 1 <= project.configuration.package_divisions <= 99:
            raise legacy_import.LegacyImportError(
                "p_div deve estar entre 1 e 99 no cálculo legado."
            )
        return project

    load_legacy_project._legacy_package_contract = True
    load_legacy_project._legacy_original = current
    legacy_import.load_legacy_project = load_legacy_project


def _install_solver_disk_serialization() -> None:
    """Compatibility no-op: DISK serialization now lives in SolverInputWriter.

    The old runtime monkey-patch rebuilt the DISK block after the canonical writer
    and therefore dropped the new OFFSET column and restored legacy DENSITY=7850.
    Keep this function only so older import paths remain valid; do not wrap render.
    """
    return

def _install_mesh_mandatory_positions() -> None:
    from . import legacy_mesh

    current = legacy_mesh._mandatory_positions
    if getattr(current, "_legacy_package_contract", False):
        return

    def mandatory_positions(project, sections):
        length = float(project.shaft_length_mm)
        values: list[float] = [0.0, length]
        values.extend(float(section.end_m) * 1000.0 for section in sections)

        # Legacy iRdin expands the package into rigid DISK records before predad.
        # Each translated disk centre is therefore a mandatory FEM station.
        values.extend(float(disk.position_mm) for disk in build_solver_disks(project))
        values.extend(float(bearing.position_mm) for bearing in project.bearings)

        # predad/getexps treats every non-distributed excitation as a physical
        # station.  This explicitly includes kind=0 unbalance.  Keeping this here
        # guarantees that the Python preview shows the same unbalance node that the
        # Fortran solver creates (e.g. OV at Z=1959 mm).
        values.extend(
            float(force.position_mm)
            for force in project.forces
            if int(force.kind) != 5
        )
        values.extend(float(mass.xi_mm) for mass in project.concentrated_masses)

        positive_probes = [
            float(probe.position_mm)
            for probe in project.probes
            if float(probe.position_mm) >= 0.0
        ]
        values.extend(positive_probes)

        positions = legacy_mesh._legacy_cluster_positions(values, length)
        for value in positive_probes:
            tolerance = max(1.0e-4, abs(value) * 1.0e-6)
            if not any(abs(existing - value) <= tolerance for existing in positions):
                positions.append(value)
        positions.sort()

        compact: list[float] = []
        for value in positions:
            if not compact or abs(value - compact[-1]) > legacy_mesh._TOL_MM:
                compact.append(value)
        return compact

    mandatory_positions._legacy_package_contract = True
    mandatory_positions._legacy_original = current
    legacy_mesh._mandatory_positions = mandatory_positions


def install_legacy_package_contract() -> None:
    """Install the legacy package/disk contract exactly once per process."""

    global _installed
    if _installed:
        return
    _install_legacy_import_mapping()
    _install_solver_disk_serialization()
    _install_mesh_mandatory_positions()
    _installed = True


__all__ = ["install_legacy_package_contract"]