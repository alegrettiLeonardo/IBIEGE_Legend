from __future__ import annotations

from dataclasses import dataclass, field
import re
from typing import Iterable

from .solver import CriticalPoint
from .graphics_presentation import read_log_decrement_limit
from .numerical_audit import NumericalAudit, audit_from_sections, excitation_orders, harmonic_label, WHIRL_LABELS


_FLOAT_RE = re.compile(r"(?<![A-Za-z0-9_])[-+]?(?:nan|inf(?:inity)?)(?![A-Za-z0-9_])|[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][-+]?\d+)?", re.IGNORECASE)
_MODE_RE = re.compile(
    r"mode\s*=\s*(\d+)\s+f\s*=\s*([-+0-9.EeDd]+)\s*\(Hz\)\s*dir\s*=\s*(\w+)",
    re.IGNORECASE,
)
_ORBIT_SECTION_RE = re.compile(
    r"sec\.\s*(\d+)\s+y\s*=\s*([-+0-9.EeDd]+)\s*\(m\)\s+dir\s*=\s*(\w+)",
    re.IGNORECASE,
)
_SPEED_RE = re.compile(r"speed\s*=\s*([-+0-9.EeDd]+)\s*\(rpm\)", re.IGNORECASE)


def _number(token: str) -> float:
    return float(token.replace("D", "E").replace("d", "e"))


def _numbers(line: str) -> list[float]:
    return [_number(x) for x in _FLOAT_RE.findall(line)]


def _find_line(lines: list[str], text: str, start: int = 0) -> int:
    needle = text.lower()
    for idx in range(start, len(lines)):
        if needle in lines[idx].lower():
            return idx
    return -1


def _numeric_table_after(
    lines: list[str], marker: str, *, start: int = 0, graph_data: bool = False,
    min_cols: int = 2,
) -> tuple[list[list[float]], int]:
    """Read a whitespace numeric table after a stable RotorDin report marker."""
    pos = _find_line(lines, marker, start)
    if pos < 0:
        return [], -1
    if graph_data:
        gd = _find_line(lines, "Graph Data", pos + 1)
        if gd >= 0:
            pos = gd
    rows: list[list[float]] = []
    started = False
    idx = pos + 1
    for idx in range(pos + 1, len(lines)):
        line = lines[idx]
        values = _numbers(line)
        begins_numeric = bool(re.match(r"^\s*[-+]?(?:\d|\.)", line))
        if begins_numeric and len(values) >= min_cols:
            rows.append(values)
            started = True
            continue
        if started:
            if not line.strip() or not begins_numeric:
                break
    return rows, idx


@dataclass(slots=True)
class XYSeries:
    name: str
    x: list[float]
    y: list[float]


@dataclass(slots=True)
class RootLocusData:
    """Complex eigenvalue trajectories written explicitly by ``s_campbl``."""

    speed_rad_s: list[float] = field(default_factory=list)
    branches: list[XYSeries] = field(default_factory=list)
    eigenvalue_convention: str = "legacy_unspecified"


@dataclass(slots=True)
class CampbellData:
    natural_frequency: list[XYSeries] = field(default_factory=list)
    harmonics: list[XYSeries] = field(default_factory=list)
    logarithmic_decrement: list[XYSeries] = field(default_factory=list)
    # Keep critical_speeds as the fourth field for positional compatibility.
    critical_speeds: list[CriticalPoint] = field(default_factory=list)
    root_locus: RootLocusData | None = None
    frequency_kind: str = "legacy_unspecified"
    logarithmic_decrement_limit: float | None = None


@dataclass(slots=True)
class CriticalMapData:
    title: str
    stiffness_n_m: list[float] = field(default_factory=list)
    modes_rpm: list[XYSeries] = field(default_factory=list)
    # Native metadata written by ucspdmap.out/uacspmap.out.  Keep this field
    # at the end so positional compatibility of title/stiffness/modes is preserved.
    rated_speed_rpm: float = 0.0


@dataclass(slots=True)
class UnbalancePeak:
    """One solver-native peak from the Automatic Response Analysis block."""

    response_index: int
    sequence: int
    rpm: float
    amplitude_m: float
    amplification_factor: float
    status: str


@dataclass(slots=True)
class UnbalanceData:
    response_positions_m: list[float] = field(default_factory=list)
    amplitude: list[XYSeries] = field(default_factory=list)
    phase: list[XYSeries] = field(default_factory=list)
    peaks: list[UnbalancePeak] = field(default_factory=list)
    amplification_low_limit: float | None = None


@dataclass(slots=True)
class ModeShape:
    mode: int
    frequency_hz: float
    direction: str
    position_m: list[float]
    x_m: list[float]
    z_m: list[float]

    @property
    def direction_label(self) -> str:
        return WHIRL_LABELS.get(self.direction, self.direction)


@dataclass(slots=True)
class ModeShapeData:
    rpm: float = 0.0
    modes: list[ModeShape] = field(default_factory=list)


@dataclass(slots=True)
class ElasticLineData:
    position_m: list[float] = field(default_factory=list)
    x_m: list[float] = field(default_factory=list)
    z_m: list[float] = field(default_factory=list)
    gx_rad: list[float] = field(default_factory=list)
    gz_rad: list[float] = field(default_factory=list)


@dataclass(slots=True)
class StaticLateralData:
    """Additional static-lateral outputs explicitly written by ``s_lin_el``."""

    bearing_force: list[XYSeries] = field(default_factory=list)
    bearing_moment: list[XYSeries] = field(default_factory=list)
    bending_moment: list[XYSeries] = field(default_factory=list)
    shear_force: list[XYSeries] = field(default_factory=list)
    bending_stress: list[XYSeries] = field(default_factory=list)

    @property
    def has_graphs(self) -> bool:
        return any((
            self.bearing_force,
            self.bearing_moment,
            self.bending_moment,
            self.shear_force,
            self.bending_stress,
        ))


@dataclass(slots=True)
class OrbitShape:
    section: int
    position_m: float
    direction: str
    x_m: list[float]
    z_m: list[float]


@dataclass(slots=True)
class OrbitSpeedCase:
    """One solver-native orbit block at an explicitly reported shaft speed."""

    speed_rpm: float
    sections: list[OrbitShape] = field(default_factory=list)


@dataclass(slots=True)
class OrbitData:
    # Legacy compatibility fields mirror the first native speed block.
    speed_rpm: float = 0.0
    points: int = 0
    sections: list[OrbitShape] = field(default_factory=list)
    # Multi-speed resp_t output. Empty means an old/single-speed caller created
    # OrbitData directly; consumers then fall back to speed_rpm/sections above.
    speed_cases: list[OrbitSpeedCase] = field(default_factory=list)

    def cases(self) -> list[OrbitSpeedCase]:
        if self.speed_cases:
            return self.speed_cases
        return [OrbitSpeedCase(self.speed_rpm, list(self.sections))]


@dataclass(slots=True)
class ParsedResults:
    campbell: CampbellData | None = None
    angular_map: CriticalMapData | None = None
    critical_map: CriticalMapData | None = None
    unbalance: UnbalanceData | None = None
    modes: ModeShapeData | None = None
    elastic_line: ElasticLineData | None = None
    orbit: OrbitData | None = None
    static_lateral: StaticLateralData | None = None
    numerical_audit: NumericalAudit | None = None

    @property
    def has_graphs(self) -> bool:
        return any((
            self.campbell, self.angular_map, self.critical_map, self.unbalance,
            self.modes, self.elastic_line, self.orbit,
            self.static_lateral and self.static_lateral.has_graphs,
        ))


def _parse_root_locus(lines: list[str]) -> RootLocusData | None:
    """Parse complex Campbell eigenvalues from current and legacy reports.

    Current ``s_campbl`` output includes an ``Eigenvalues`` heading.  Some retained
    RotorDin fixtures/legacy reports omit the heading but still contain the exact
    parenthesized ``speed (real imag) ...`` rows immediately before the logarithmic
    decrement block.  The fallback deliberately accepts only that parenthesized,
    numeric-leading contiguous block, so ordinary Campbell tables cannot be
    misclassified as eigenvalues.
    """

    rows, _ = _numeric_table_after(lines, "Eigenvalues", min_cols=3)

    if not rows:
        log_idx = _find_line(lines, "Logarithmic Decrement")
        if log_idx >= 0:
            legacy_rows: list[list[float]] = []
            idx = log_idx - 1
            # Skip separator whitespace between the two report blocks.
            while idx >= 0 and not lines[idx].strip():
                idx -= 1
            # Collect only the immediately preceding parenthesized complex rows.
            while idx >= 0:
                line = lines[idx]
                begins_numeric = bool(re.match(r"^\s*[-+]?(?:\d|\.)", line))
                if not (begins_numeric and "(" in line and ")" in line):
                    break
                values = _numbers(line)
                if len(values) < 3 or (len(values) - 1) % 2:
                    break
                legacy_rows.append(values)
                idx -= 1
            rows = list(reversed(legacy_rows))

    if not rows:
        return None

    ncols = min(len(row) for row in rows)
    pair_count = (ncols - 1) // 2
    if pair_count <= 0:
        return None

    physical = any('EIGENVALUE_CONVENTION A_NEGATES_PHYSICAL_S' in line for line in lines)
    if physical and any(len(row) != ncols for row in rows):
        raise ValueError('Tabela de polos com colunas inconsistentes')
    factor = -1.0 if physical else 1.0
    speed = [row[0] for row in rows]
    branches = [
        XYSeries(
            f"Modo {index + 1}",
            [factor * row[1 + 2 * index] for row in rows],
            [factor * row[2 + 2 * index] for row in rows],
        )
        for index in range(pair_count)
    ]
    return RootLocusData(speed_rad_s=speed, branches=branches,
                         eigenvalue_convention='physical_s' if physical else 'legacy_unspecified')


def parse_campbell(text: str, critical_speeds: Iterable[CriticalPoint] = ()) -> CampbellData | None:
    if not text.strip():
        return None
    lines = text.splitlines()
    orders = excitation_orders(lines)
    offset = 1 + len(orders)
    rows, _ = _numeric_table_after(lines, "Natural frequencies (rpm) x (Hz)", graph_data=True,
                                   min_cols=offset + 1)
    natural: list[XYSeries] = []
    harmonics: list[XYSeries] = []
    if rows:
        ncols = len(rows[0])
        if any(len(row) != ncols for row in rows):
            raise ValueError("Tabela Campbell com colunas inconsistentes")
        x = [row[0] for row in rows]
        for col, order in enumerate(orders, 1):
            harmonics.append(XYSeries(harmonic_label(order), x, [row[col] for row in rows]))
        for col in range(offset, ncols):
            natural.append(XYSeries(f"Modo {col-offset+1}", x, [row[col] for row in rows]))
    marker = "Logarithmic Decrement raw" if _find_line(lines, "Logarithmic Decrement raw") >= 0 \
        else "Logarithmic Decrement"
    ld_rows, _ = _numeric_table_after(lines, marker, min_cols=2)
    decrement: list[XYSeries] = []
    if ld_rows:
        ncols = len(ld_rows[0])
        if any(len(row) != ncols for row in ld_rows):
            raise ValueError("Tabela de decrementos com colunas inconsistentes")
        x = [row[0] for row in ld_rows]
        for col in range(1, ncols):
            decrement.append(XYSeries(f"Modo {col}", x, [row[col] for row in ld_rows]))
    return CampbellData(
        natural_frequency=natural, harmonics=harmonics,
        logarithmic_decrement=decrement, critical_speeds=list(critical_speeds),
        root_locus=_parse_root_locus(lines),
        frequency_kind="damped_imaginary_part" if "FREQUENCY_KIND DAMPED_IMAGINARY_PART" in text
                       else "legacy_unspecified",
        logarithmic_decrement_limit=read_log_decrement_limit(text),
    )


def parse_critical_map(text: str, title: str) -> CriticalMapData | None:
    if not text.strip():
        return None
    lines = text.splitlines()
    rows, _ = _numeric_table_after(lines, "Critical speed (N/m) x (rpm)", graph_data=True, min_cols=2)
    if not rows:
        return None

    rated_speed_rpm = 0.0
    rated_index = _find_line(lines, "rated speed (rpm)")
    if rated_index >= 0:
        values = _numbers(lines[rated_index])
        if values:
            rated_speed_rpm = float(values[-1])

    ncols = min(len(r) for r in rows)
    k = [r[0] for r in rows]
    modes = [XYSeries(f"Modo {col}", k, [r[col] for r in rows]) for col in range(1, ncols)]
    return CriticalMapData(
        title,
        k,
        modes,
        rated_speed_rpm=rated_speed_rpm,
    )


def _response_positions(lines: list[str]) -> list[float]:
    idx = _find_line(lines, "point")
    if idx < 0:
        return []
    for line in lines[idx + 1: idx + 6]:
        if "y (m)" in line.lower():
            vals = _numbers(line)
            return vals
    return []


def _series_from_rows(rows: list[list[float]], prefix: str) -> list[XYSeries]:
    if not rows:
        return []
    ncols = min(len(r) for r in rows)
    x = [r[0] for r in rows]
    return [XYSeries(f"{prefix} {col}", x, [r[col] for r in rows]) for col in range(1, ncols)]


def _parse_unbalance_peaks(
    lines: list[str],
) -> tuple[list[UnbalancePeak], float | None]:
    """Parse the solver-native automatic peak/AF table without recomputation.

    s_resp_f can write two numeric blocks separated by a blank line: peaks at or
    above the configured AF threshold first, followed by lower-AF peaks. The
    generic numeric-table parser intentionally stops at such a break, so this
    report section needs its own bounded parser.
    """

    start = _find_line(lines, "Automatic Response Analysis")
    if start < 0:
        return [], None
    stop = _find_line(lines, "Amplitude (rpm) x (m)", start + 1)
    if stop < 0:
        stop = len(lines)

    low_limit: float | None = None
    low_marker = _find_line(lines, "amplif. low limit", start + 1)
    if start <= low_marker < stop:
        for line in lines[low_marker + 1 : min(stop, low_marker + 5)]:
            values = _numbers(line)
            if values and re.match(r"^\s*[-+]?(?:\d|\.)", line):
                low_limit = values[0]
                break

    header = -1
    for index in range(start + 1, stop):
        lowered = lines[index].casefold()
        if "max. disp." in lowered and "amplif. factor" in lowered:
            header = index
            break
    if header < 0:
        return [], low_limit

    raw: list[tuple[int, float, float, float, str]] = []
    for line in lines[header + 1 : stop]:
        if not re.match(r"^\s*[-+]?(?:\d|\.)", line):
            continue
        values = _numbers(line)
        if len(values) < 4:
            continue
        match = re.search(r"([nifF])\s*$", line)
        status = match.group(1) if match else ""
        response_index = int(round(values[3]))
        if response_index <= 0:
            continue
        raw.append(
            (
                response_index,
                float(values[0]),
                float(values[1]),
                float(values[2]),
                status,
            )
        )

    raw.sort(key=lambda item: (item[0], item[1]))
    counters: dict[int, int] = {}
    peaks: list[UnbalancePeak] = []
    for response_index, rpm, amplitude_m, amplification_factor, status in raw:
        counters[response_index] = counters.get(response_index, 0) + 1
        peaks.append(
            UnbalancePeak(
                response_index=response_index,
                sequence=counters[response_index],
                rpm=rpm,
                amplitude_m=amplitude_m,
                amplification_factor=amplification_factor,
                status=status,
            )
        )
    return peaks, low_limit


def parse_unbalance(text: str) -> UnbalanceData | None:
    if not text.strip():
        return None
    lines = text.splitlines()
    amp_rows, _ = _numeric_table_after(lines, "Amplitude (rpm) x (m)", graph_data=True, min_cols=2)
    phase_rows, _ = _numeric_table_after(lines, "Phase (rpm) x (rad)", min_cols=2)
    peaks, amplification_low_limit = _parse_unbalance_peaks(lines)
    return UnbalanceData(
        response_positions_m=_response_positions(lines),
        amplitude=_series_from_rows(amp_rows, "Ponto"),
        phase=_series_from_rows(phase_rows, "Ponto"),
        peaks=peaks,
        amplification_low_limit=amplification_low_limit,
    )


def parse_modes(text: str) -> ModeShapeData | None:
    if not text.strip():
        return None
    lines = text.splitlines()
    rpm = 0.0
    idx = _find_line(lines, "Mode Shape - rpm")
    if idx >= 0:
        vals = _numbers(lines[idx])
        if vals:
            rpm = vals[-1]

    meta: list[tuple[int, float, str]] = []
    for line in lines:
        m = _MODE_RE.search(line)
        if m:
            meta.append((int(m.group(1)), _number(m.group(2)), m.group(3).upper()))

    rows, _ = _numeric_table_after(lines, "Graph Data", min_cols=3)
    if not rows or not meta:
        return ModeShapeData(rpm=rpm)
    nmodes = min(len(meta), (min(len(r) for r in rows) - 1) // 2)
    pos = [r[0] for r in rows]
    modes: list[ModeShape] = []
    for i in range(nmodes):
        number, freq, direction = meta[i]
        modes.append(ModeShape(
            number, freq, direction, pos,
            [r[1 + i] for r in rows],
            [r[1 + nmodes + i] for r in rows],
        ))
    return ModeShapeData(rpm, modes)


def parse_elastic_line(text: str) -> ElasticLineData | None:
    if not text.strip():
        return None
    lines = text.splitlines()
    rows, _ = _numeric_table_after(lines, "Displacements", min_cols=5)
    if not rows:
        return None
    rows = [r for r in rows if len(r) >= 5]
    return ElasticLineData(
        [r[0] for r in rows], [r[1] for r in rows], [r[2] for r in rows],
        [r[3] for r in rows], [r[4] for r in rows],
    )


def parse_static_lateral(text: str) -> StaticLateralData | None:
    """Parse solver-native static reactions/internal loads from ``selaline.out``."""

    if not text.strip():
        return None
    lines = text.splitlines()
    result = StaticLateralData()

    reaction_rows, _ = _numeric_table_after(lines, "Bearing Reaction (N, Nm)", min_cols=5)
    if reaction_rows:
        bearing = [row[0] for row in reaction_rows]
        result.bearing_force = [
            XYSeries("X", list(bearing), [row[1] for row in reaction_rows]),
            XYSeries("Z", list(bearing), [row[2] for row in reaction_rows]),
        ]
        result.bearing_moment = [
            XYSeries("X", list(bearing), [row[3] for row in reaction_rows]),
            XYSeries("Z", list(bearing), [row[4] for row in reaction_rows]),
        ]

    moment_rows, _ = _numeric_table_after(lines, "Bending moment at division (Nm)", min_cols=3)
    if moment_rows:
        position = [row[0] for row in moment_rows]
        result.bending_moment = [
            XYSeries("X", list(position), [row[1] for row in moment_rows]),
            XYSeries("Z", list(position), [row[2] for row in moment_rows]),
        ]

    shear_rows, _ = _numeric_table_after(lines, "Shear Force constant on Element (N)", min_cols=4)
    if shear_rows:
        position: list[float] = []
        shear_x: list[float] = []
        shear_z: list[float] = []
        for row in shear_rows:
            position.extend((row[0], row[1]))
            shear_x.extend((row[2], row[2]))
            shear_z.extend((row[3], row[3]))
        result.shear_force = [
            XYSeries("X", position, shear_x),
            XYSeries("Z", list(position), shear_z),
        ]

    stress_rows, _ = _numeric_table_after(lines, "bending stress", min_cols=2)
    if stress_rows:
        result.bending_stress = [
            XYSeries(
                "Tensão de flexão",
                [row[0] for row in stress_rows],
                [row[1] for row in stress_rows],
            )
        ]

    return result if result.has_graphs else None


def _parse_orbit_sections(lines: list[str], start: int, stop: int) -> list[OrbitShape]:
    shapes: list[OrbitShape] = []
    i = start
    while i < stop:
        match = _ORBIT_SECTION_RE.search(lines[i])
        if not match:
            i += 1
            continue
        sec = int(match.group(1))
        pos = _number(match.group(2))
        direction = match.group(3).upper()
        i += 1
        xs: list[float] = []
        zs: list[float] = []
        started = False
        while i < stop:
            line = lines[i]
            vals = _numbers(line)
            begins_numeric = bool(re.match(r"^\s*[-+]?(?:\d|\.)", line))
            if begins_numeric and len(vals) >= 2:
                xs.append(vals[0])
                zs.append(vals[1])
                started = True
                i += 1
                continue
            if started:
                break
            i += 1
        shapes.append(OrbitShape(sec, pos, direction, xs, zs))
    return shapes


def parse_orbit(text: str) -> OrbitData | None:
    if not text.strip():
        return None
    lines = text.splitlines()
    point_count = 0
    for line in lines:
        if "Orbit points" in line:
            vals = _numbers(line)
            if vals:
                point_count = int(vals[-1])
                break

    markers: list[tuple[int, float]] = []
    for index, line in enumerate(lines):
        match = _SPEED_RE.search(line)
        if match:
            markers.append((index, _number(match.group(1))))

    cases: list[OrbitSpeedCase] = []
    for item, (start, speed) in enumerate(markers):
        stop = markers[item + 1][0] if item + 1 < len(markers) else len(lines)
        sections = _parse_orbit_sections(lines, start + 1, stop)
        # timeresp.out can repeat a "speed = ..." line in the scalar/module
        # summary that follows the orbit trajectories. Only blocks containing
        # native section trajectories are selectable orbit speed cases.
        if sections:
            cases.append(OrbitSpeedCase(speed, sections))

    if cases:
        first = cases[0]
        return OrbitData(
            speed_rpm=first.speed_rpm,
            points=point_count,
            sections=list(first.sections),
            speed_cases=cases,
        )

    # Preserve the old parser behavior for malformed/legacy snippets that carry
    # section trajectories but no explicit "speed = ... rpm" line.
    sections = _parse_orbit_sections(lines, 0, len(lines))
    return OrbitData(0.0, point_count, sections)


def parse_solver_sections(sections: dict[str, str], critical_speeds: Iterable[CriticalPoint] = ()) -> ParsedResults:
    elastic_text = sections.get("selaline.out", "")
    return ParsedResults(
        campbell=parse_campbell(sections.get("campbell.out", ""), critical_speeds),
        angular_map=parse_critical_map(sections.get("uacspmap.out", ""), "Mapa de velocidade crítica angular"),
        critical_map=parse_critical_map(sections.get("ucspdmap.out", ""), "Mapa de velocidade crítica"),
        unbalance=parse_unbalance(sections.get("unblresp.out", "")),
        modes=parse_modes(sections.get("modeshp.out", "")),
        elastic_line=parse_elastic_line(elastic_text),
        orbit=parse_orbit(sections.get("timeresp.out", "")),
        static_lateral=parse_static_lateral(elastic_text),
        numerical_audit=audit_from_sections(sections),
    )
