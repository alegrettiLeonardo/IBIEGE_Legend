from __future__ import annotations

"""Preview the final FEM stations produced by RotorDin's legacy discretization rule.

The Fortran solver keeps shaft geometry and finite-element discretization separate.
``predad.f`` first assembles mandatory physical stations (section boundaries, disks,
bearings, excitations, concentrated masses and positive response locations) and then
subdivides the intervals between those stations.

For the historical default ``LDR <= 0`` rule, the longest mandatory interval receives
``MXDIV`` divisions and every other interval receives ``DIV`` divisions. Intervals
shorter than 10% of the local shaft diameter are reduced to one element. A positive
per-section ``DIV`` overrides that global rule. If ``LDR > 0``, automatic divisions
are instead increased until element length / local diameter satisfies LDR.

This module mirrors that calculation for UI inspection only. The production Fortran
solver remains authoritative and unchanged.
"""

from dataclasses import dataclass
import math

from .domain import Project


_MTS = 999
_MXO = 150
_POSITION_CLUSTER_MM = 1.0  # Fortran sprec=1e-3 m.
_TOL_MM = 1.0e-7


@dataclass(frozen=True, slots=True)
class LegacyMeshResult:
    nodes_mm: tuple[float, ...]
    interval_positions_mm: tuple[float, ...]
    interval_divisions: tuple[int, ...]
    segment_element_counts: tuple[int, ...]

    @property
    def element_count(self) -> int:
        return max(0, len(self.nodes_mm) - 1)


def _fortran_nint_positive(value: float) -> int:
    return int(math.floor(float(value) + 0.5))


def _translated_sections(project: Project):
    # Import dynamically so Mesh Studio sees the solver translation after
    # install_solver_mesh_contract() has wrapped build_solver_sections.
    from . import solver

    return solver.build_solver_sections(project)


def _mass_position_mm(mass) -> float:
    # The DISK record stores its axial centre, matching ``pd`` in predad.f.
    return float(mass.xi_mm) + 0.5 * float(mass.length_mm)


def _legacy_cluster_positions(values: list[float], length_mm: float) -> list[float]:
    """Approximate predad's sorted 1 mm de-duplication in millimetres."""

    bounded = sorted(
        value
        for value in (float(item) for item in values)
        if -_TOL_MM <= value <= float(length_mm) + _TOL_MM
    )
    if not bounded:
        return [0.0, float(length_mm)]

    # predad retains the last item of a <=1 mm cluster before the next real gap.
    clustered: list[float] = []
    cluster_last = bounded[0]
    for value in bounded[1:]:
        if value - cluster_last <= _POSITION_CLUSTER_MM:
            cluster_last = value
        else:
            clustered.append(cluster_last)
            cluster_last = value
    clustered.append(cluster_last)

    if not clustered or abs(clustered[0]) > _TOL_MM:
        clustered.insert(0, 0.0)
    else:
        clustered[0] = 0.0
    if abs(clustered[-1] - length_mm) > _TOL_MM:
        clustered.append(float(length_mm))
    else:
        clustered[-1] = float(length_mm)
    return clustered


def _mandatory_positions(project: Project, sections) -> list[float]:
    length = float(project.shaft_length_mm)
    values: list[float] = [0.0, length]
    values.extend(float(section.end_m) * 1000.0 for section in sections)
    values.extend(_mass_position_mm(mass) for mass in project.masses)
    values.extend(float(bearing.position_mm) for bearing in project.bearings)
    values.extend(
        float(force.position_mm)
        for force in project.forces
        if int(force.kind) != 5
    )
    values.extend(float(mass.xi_mm) for mass in project.concentrated_masses)

    # predad puts positive response/probe positions into pp *before* the generic
    # sorted 1 mm de-duplication, then explicitly re-inserts them afterwards with a
    # tighter tolerance. Keeping both phases matters when a probe lies within 1 mm of
    # a bearing/disk/section station: the generic pass may retain the later point, but
    # the requested probe must still survive exactly.
    positive_probes = [
        float(probe.position_mm)
        for probe in project.probes
        if float(probe.position_mm) >= 0.0
    ]
    values.extend(positive_probes)

    positions = _legacy_cluster_positions(values, length)

    # predad re-inserts positive response stations with a much tighter tolerance after
    # the historical generic 1 mm de-duplication.
    for value in positive_probes:
        tolerance = max(1.0e-4, abs(value) * 1.0e-6)
        if not any(abs(existing - value) <= tolerance for existing in positions):
            positions.append(value)
    positions.sort()

    compact: list[float] = []
    for value in positions:
        if not compact or abs(value - compact[-1]) > _TOL_MM:
            compact.append(value)
    return compact


def _section_geometry(sections, position_mm: float):
    start = 0.0
    for section in sections:
        end = float(section.end_m) * 1000.0
        if position_mm <= end + _TOL_MM:
            return section, start, end
        start = end
    if sections:
        section = sections[-1]
        return section, start, float(section.end_m) * 1000.0
    return None, 0.0, 0.0


def _local_diameter_mm(section, start_mm: float, end_mm: float, position_mm: float) -> float:
    if section is None:
        return 0.0
    length = end_mm - start_mm
    ratio = 0.0 if length <= 0.0 else (position_mm - start_mm) / length
    ratio = max(0.0, min(1.0, ratio))
    d0 = float(section.diameter_m) * 1000.0
    d1 = float(section.final_diameter_m) * 1000.0
    return d0 + (d1 - d0) * ratio


def _segment_counts(project: Project, nodes: list[float]) -> list[int]:
    counts = [0] * len(project.segments)
    limits: list[tuple[float, float]] = []
    start = 0.0
    for segment in project.segments:
        end = start + float(segment.length_mm)
        limits.append((start, end))
        start = end

    for x0, x1 in zip(nodes, nodes[1:]):
        midpoint = 0.5 * (x0 + x1)
        for index, (start, end) in enumerate(limits):
            if start - _TOL_MM <= midpoint <= end + _TOL_MM:
                counts[index] += 1
                break
    return counts


def _ldr_divisions(intlen_mm: float, diameter_mm: float, ldr: float) -> int:
    """Mirror ``divdia`` including its historical ``mxo=150`` stop condition.

    ``divdia`` increments its loop counter and checks ``cont > mxo`` immediately
    after evaluating the current division count. Consequently a case that would need
    the 150th division is rejected; 149 is the largest successful count.
    """

    from .solver import SolverError

    limit = ldr + ldr / float(_MXO)
    divisions = 1
    while True:
        rel = (intlen_mm / float(divisions)) / diameter_mm
        if divisions >= _MXO:
            raise SolverError(
                "A regra L/D da malha atingiu o limite histórico de divdia: "
                f"LDR={ldr:.6g}, intervalo={intlen_mm:.6g} mm, "
                f"diâmetro local={diameter_mm:.6g} mm. O RotorDin interrompe ao "
                f"atingir {_MXO} divisões (máximo efetivamente aceito: {_MXO - 1}). "
                "Aumente LDR ou revise a geometria/refinamento."
            )
        if rel <= limit:
            return divisions
        divisions += 1


def legacy_mesh_result(project: Project) -> LegacyMeshResult:
    """Return the final-node preview using the same rules as ``predad.f``.

    The translated SECTIONS list is used deliberately. Therefore native geometry
    cuts, conical DIV values, user per-section division requests and specialist cuts
    that are already part of the solver input remain part of the same contract.
    """

    if not project.segments or project.shaft_length_mm <= 0.0:
        return LegacyMeshResult((), (), (), tuple(0 for _ in project.segments))

    sections = list(_translated_sections(project))
    if not sections:
        return LegacyMeshResult((), (), (), tuple(0 for _ in project.segments))

    positions = _mandatory_positions(project, sections)
    if len(positions) < 2:
        return LegacyMeshResult(tuple(positions), tuple(positions), (), tuple(0 for _ in project.segments))

    interval_lengths = [b - a for a, b in zip(positions, positions[1:])]
    largest_index = 0
    largest_length = -1.0
    for index, length in enumerate(interval_lengths):
        # Strict > reproduces the Fortran tie behaviour: the first largest interval
        # keeps MXDIV when equal-length intervals exist.
        if length > largest_length:
            largest_length = length
            largest_index = index

    div_default = max(1, int(project.configuration.min_divisions))
    div_largest = max(1, int(project.configuration.max_divisions))
    ldr = float(getattr(project.configuration, "load_distribution", 0.0) or 0.0)

    interval_divisions: list[int] = []
    for index, (x0, x1) in enumerate(zip(positions, positions[1:])):
        intlen = x1 - x0
        midpoint = 0.5 * (x0 + x1)
        section, section_start, section_end = _section_geometry(sections, midpoint)
        if section is None or intlen <= 0.0:
            interval_divisions.append(1)
            continue

        local_div = int(getattr(section, "divisions", 0) or 0)
        diameter = max(_local_diameter_mm(section, section_start, section_end, midpoint), 1.0e-9)

        if local_div > 0:
            section_length = max(section_end - section_start, 1.0e-9)
            target_length = section_length / float(local_div)
            divisions = max(1, _fortran_nint_positive(intlen / target_length))
        elif ldr > 0.0:
            divisions = _ldr_divisions(intlen, diameter, ldr)
        else:
            divisions = div_largest if index == largest_index else div_default

        # In predad's LDR<=0 branch this reduction is applied after both automatic
        # DIV/MXDIV selection and a user/native local DIV value.
        if ldr <= 0.0 and intlen < 0.1 * diameter:
            divisions = 1

        interval_divisions.append(max(1, int(divisions)))

    nodes: list[float] = [positions[0]]
    for (x0, x1), divisions in zip(zip(positions, positions[1:]), interval_divisions):
        delta = (x1 - x0) / float(divisions)
        for local in range(1, divisions + 1):
            value = x0 + delta * local
            if abs(value - nodes[-1]) > _TOL_MM:
                nodes.append(value)

    if len(nodes) > _MTS:
        from .solver import SolverError

        raise SolverError(
            f"A malha gerou {len(nodes)} nós; o RotorDin aceita no máximo {_MTS}. "
            "Reduza DIV/MXDIV, aumente LDR ou diminua o refinamento por seção."
        )

    counts = _segment_counts(project, nodes)
    return LegacyMeshResult(
        tuple(nodes),
        tuple(positions),
        tuple(interval_divisions),
        tuple(counts),
    )


def legacy_mesh_nodes(project: Project) -> list[float]:
    return list(legacy_mesh_result(project).nodes_mm)


__all__ = ["LegacyMeshResult", "legacy_mesh_nodes", "legacy_mesh_result"]
