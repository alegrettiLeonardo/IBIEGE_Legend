from __future__ import annotations

"""Canonical translation from logical RotorDin masses to solver DISK records.

The desktop project keeps one logical DistributedMass for the lamination stack
(`Pacote`). Legacy iRdin expanded that one logical package into ``p_div``
rigid DISK records before calling RotorDin. This module is now the single
translation authority for every logical mass sent to the solver.
"""

from dataclasses import dataclass

from .domain import DomainError, Project
from .mass_properties import effective_mass_properties


@dataclass(frozen=True, slots=True)
class SolverDisk:
    """One rigid DISK record to be serialized to RotorDin."""

    position_mm: float
    length_mm: float
    weight_kg: float
    outer_diameter_mm: float
    inner_diameter_mm: float
    density_kg_m3: float
    diametral_inertia_kg_m2: float
    polar_inertia_kg_m2: float
    offset_mm: float
    cg_position_mm: float
    source_mass_index: int
    package_piece_index: int = 0
    package_piece_count: int = 1

    @property
    def is_package_piece(self) -> bool:
        return self.package_piece_count > 1


def expanded_disk_count(project: Project) -> int:
    """Return the actual RotorDin DISK count after package expansion."""

    divisions = int(project.configuration.package_divisions)
    return sum(divisions if mass.package else 1 for mass in project.masses)


def build_solver_disks(project: Project) -> list[SolverDisk]:
    """Translate logical masses into the exact rigid disks sent to RotorDin.

    A non-package mass remains one DISK. A package is split into equal axial
    slices so total mass, axial extent and centroid are preserved. The same
    logical offset is propagated to every slice, therefore the mass-weighted CG
    of the expanded package remains logical_center + offset.

    D.Int is resolved here from the physical mounting geometry before
    serialization. This makes the frontend, preview and solver input share one
    contract. Fortran remains authoritative for disk inertia: the writer keeps
    IX/IY zero, while the derived values stored here are used for engineering
    display and parity tests against predad.f.
    """

    divisions = int(project.configuration.package_divisions)
    if divisions < 1 or divisions > 99:
        raise DomainError(
            f"DIV pacote deve estar entre 1 e 99; recebido {divisions}."
        )
    count_total = expanded_disk_count(project)
    if count_total > 99:
        raise DomainError(
            "A expansão do pacote gera "
            f"{count_total} discos, mas o RotorDin aceita no máximo 99. "
            "Reduza DIV pacote ou o número de massas/discos."
        )
    disk_density = float(project.configuration.disk_density_kg_m3)

    disks: list[SolverDisk] = []
    for source_index, mass in enumerate(project.masses, start=1):
        count = divisions if mass.package else 1
        length = float(mass.length_mm) / float(count)
        weight = float(mass.kg) / float(count)
        for piece in range(count):
            xi = float(mass.xi_mm) + piece * length
            props = effective_mass_properties(
                project,
                mass,
                xi_mm=xi,
                length_mm=length,
                kg=weight,
            )
            disks.append(
                SolverDisk(
                    position_mm=props.geometric_center_mm,
                    length_mm=props.length_mm,
                    weight_kg=props.mass_kg,
                    outer_diameter_mm=props.outer_diameter_mm,
                    inner_diameter_mm=props.inner_diameter_mm,
                    # Keep the configured legacy material density as metadata.
                    # The writer deliberately serializes DENSITY=0 while WEIGHT
                    # is explicit, so this value cannot silently switch modes.
                    density_kg_m3=disk_density,
                    diametral_inertia_kg_m2=props.diametral_inertia_kg_m2,
                    polar_inertia_kg_m2=props.polar_inertia_kg_m2,
                    offset_mm=props.offset_mm,
                    cg_position_mm=props.cg_position_mm,
                    source_mass_index=source_index,
                    package_piece_index=piece + 1 if mass.package else 0,
                    package_piece_count=count,
                )
            )

    return disks


__all__ = ["SolverDisk", "build_solver_disks", "expanded_disk_count"]
