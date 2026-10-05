from __future__ import annotations

"""Physical-property contract for RotorDin distributed masses/disks.

This module is deliberately independent from the FE-equivalent shaft geometry.
Mass/disc bores are resolved against the *physical* shaft geometry used by the
modeling UI.  In ribbed package regions the physical mounting diameter is DPCT,
not the equivalent bending diameter sent to the shaft finite element model.
"""

from dataclasses import dataclass
import math

from .domain import DistributedMass, DomainError, Project, ShaftSegment


@dataclass(frozen=True, slots=True)
class EffectiveMassProperties:
    geometric_center_mm: float
    cg_position_mm: float
    length_mm: float
    mass_kg: float
    outer_diameter_mm: float
    inner_diameter_mm: float
    diametral_inertia_kg_m2: float
    polar_inertia_kg_m2: float
    density_kg_m3: float
    offset_mm: float
    source_segment_index: int


def _segment_at(project: Project, position_mm: float) -> tuple[int, ShaftSegment, float]:
    if not project.segments:
        raise DomainError("Defina ao menos um segmento antes de resolver propriedades da massa.")
    x0 = 0.0
    for index, segment in enumerate(project.segments):
        x1 = x0 + float(segment.length_mm)
        if position_mm <= x1 + 1.0e-9:
            ratio = 0.0 if segment.length_mm <= 0 else (position_mm - x0) / segment.length_mm
            return index, segment, max(0.0, min(1.0, ratio))
        x0 = x1
    raise DomainError(
        f"Centro da massa em {position_mm:g} mm está fora do comprimento do eixo "
        f"({project.shaft_length_mm:g} mm)."
    )


def local_physical_shaft_outer_diameter_mm(project: Project, position_mm: float) -> float:
    """Return the real shaft OD at x, never the FE-equivalent ribbed diameter."""
    _index, segment, ratio = _segment_at(project, position_mm)
    return float(segment.diameter_mm) + (
        float(segment.effective_final_diameter_mm) - float(segment.diameter_mm)
    ) * ratio


def effective_mass_inner_diameter_mm(project: Project, mass: DistributedMass, position_mm: float | None = None) -> float:
    """Resolve the physical disk bore.

    Priority:
    1. Explicit D.Int entered by the user.
    2. DPCT for a ribbed/package shaft section.
    3. Local physical shaft OD for circular/conical sections.

    The equivalent bending diameter of a ribbed FE section is intentionally not
    used here.
    """
    if mass.inner_diameter_mm > 0.0:
        return float(mass.inner_diameter_mm)
    # Legacy projects may omit both D.Ext and D.Int. In that special case the
    # historical writer lets the disk OD fall back to the local shaft diameter
    # while keeping a solid-disk bore (D.Int=0). Applying the new automatic
    # mounting-bore rule simultaneously to both missing diameters would create
    # the degenerate geometry D.Int == D.Ext. Preserve that legacy solid-disk
    # interpretation only when D.Ext is also unspecified.
    if mass.outer_diameter_mm <= 0.0:
        return 0.0
    x = float(mass.center_mm if position_mm is None else position_mm)
    _index, segment, _ratio = _segment_at(project, x)
    if segment.is_ribbed and segment.package_diameter_mm > 0.0:
        return float(segment.package_diameter_mm)
    return local_physical_shaft_outer_diameter_mm(project, x)


def effective_mass_outer_diameter_mm(project: Project, mass: DistributedMass, position_mm: float | None = None) -> float:
    """Resolve disk OD while preserving the historical local-shaft fallback."""
    if mass.outer_diameter_mm > 0.0:
        return float(mass.outer_diameter_mm)
    x = float(mass.center_mm if position_mm is None else position_mm)
    return local_physical_shaft_outer_diameter_mm(project, x)


def disk_inertias_kg_m2(
    mass_kg: float,
    length_mm: float,
    outer_diameter_mm: float,
    inner_diameter_mm: float,
) -> tuple[float, float]:
    """Return (diametral Ix, polar Iy) using RotorDin predad.f formulae."""
    if mass_kg <= 0.0:
        return 0.0, 0.0
    length_m = float(length_mm) / 1000.0
    de_m = float(outer_diameter_mm) / 1000.0
    di_m = float(inner_diameter_mm) / 1000.0
    polar = float(mass_kg) / 8.0 * (de_m * de_m + di_m * di_m)
    diametral = 0.5 * polar + float(mass_kg) * length_m * length_m / 12.0
    return diametral, polar


def effective_mass_properties(
    project: Project,
    mass: DistributedMass,
    *,
    xi_mm: float | None = None,
    length_mm: float | None = None,
    kg: float | None = None,
) -> EffectiveMassProperties:
    """Resolve the exact physical data for one logical mass or package slice."""
    xi = float(mass.xi_mm if xi_mm is None else xi_mm)
    length = float(mass.length_mm if length_mm is None else length_mm)
    mass_kg = float(mass.kg if kg is None else kg)
    center = xi + 0.5 * length
    segment_index, _segment, _ratio = _segment_at(project, center)
    outer = effective_mass_outer_diameter_mm(project, mass, center)
    inner = effective_mass_inner_diameter_mm(project, mass, center)

    if not all(math.isfinite(v) for v in (center, length, mass_kg, outer, inner, mass.offset_mm)):
        raise DomainError("Propriedades da massa devem ser finitas.")
    if length <= 0.0:
        raise DomainError(f"LC da massa deve ser maior que zero; recebido {length:g} mm.")
    if mass_kg < 0.0:
        raise DomainError(f"Massa deve ser não-negativa; recebido {mass_kg:g} kg.")
    if outer <= 0.0:
        raise DomainError(
            "D.Ext efetivo da massa deve ser maior que zero. "
            "Informe D.Ext explicitamente quando a geometria do disco exceder o eixo."
        )
    if inner < 0.0 or inner >= outer:
        raise DomainError(
            f"Geometria de massa inválida: D.Int efetivo={inner:g} mm e "
            f"D.Ext efetivo={outer:g} mm; esperado 0 <= D.Int < D.Ext. "
            "Informe um D.Ext físico maior que o diâmetro de montagem."
        )

    ix, iy = disk_inertias_kg_m2(mass_kg, length, outer, inner)
    return EffectiveMassProperties(
        geometric_center_mm=center,
        cg_position_mm=center + float(mass.offset_mm),
        length_mm=length,
        mass_kg=mass_kg,
        outer_diameter_mm=outer,
        inner_diameter_mm=inner,
        diametral_inertia_kg_m2=ix,
        polar_inertia_kg_m2=iy,
        # Current UI contract is explicit WEIGHT.  DENSITY remains zero so the
        # solver never guesses a mass-calculation mode from WEIGHT == 0.
        density_kg_m3=0.0,
        offset_mm=float(mass.offset_mm),
        source_segment_index=segment_index,
    )


__all__ = [
    "EffectiveMassProperties",
    "disk_inertias_kg_m2",
    "effective_mass_inner_diameter_mm",
    "effective_mass_outer_diameter_mm",
    "effective_mass_properties",
    "local_physical_shaft_outer_diameter_mm",
]
