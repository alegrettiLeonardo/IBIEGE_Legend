from __future__ import annotations

"""Presentation metadata only: no matrix assembly, root solving or peak fitting.

Public convention: native x -> X, native z -> Y, native axial y -> Z.
The mapping below changes names, never numbers, signs, or input column order.
"""

from collections import defaultdict
from collections.abc import Iterable, Mapping, Sequence
from copy import deepcopy
from dataclasses import dataclass
import hashlib
import json
import math
import re
from types import MappingProxyType
from typing import Any


SETTINGS_KEY = "result_graphics_presentation"
SCHEMA_VERSION = 1
COEFFICIENT_LABELS = MappingProxyType({
    "Kxx": "Kxx", "Kxz": "Kxy", "Kzx": "Kyx", "Kzz": "Kyy",
    "Cxx": "Cxx", "Cxz": "Cxy", "Czx": "Cyx", "Czz": "Cyy",
})
_COEFFICIENT = re.compile(r"(?<![A-Za-z0-9_])(?:K|C)[xz]{2}(?![A-Za-z0-9_])")
_MODE = re.compile(r"^(?:Modo|Mode)\s+\d+(?:\b.*)?$", re.IGNORECASE)
_RESPONSE = re.compile(r"^r\d+$", re.IGNORECASE)
_ORDER = re.compile(r"^\s*(\d+(?:\.\d+)?)\s*x\s*$", re.IGNORECASE)
_FLOAT = r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][-+]?\d+)?"
_LOG_LIMIT = re.compile(
    rf"(?im)^\s*Logarithmic\s+Decrement\s+max\s*(?:=|:)?\s*({_FLOAT})\s*$"
)


def coefficient_label(native: str) -> str:
    return COEFFICIENT_LABELS.get(native, native)


def bearing_display_text(text: str) -> str:
    return _COEFFICIENT.sub(lambda m: coefficient_label(m.group(0)), str(text))


def native_coefficient(text: str) -> str:
    match = _COEFFICIENT.search(str(text))
    return match.group(0) if match else ""


def default_series_identity(native_name: str, occurrence: int = 0) -> str:
    """Fallback identity from the EXACT native token, never its translated alias.

    Do not remove FW/BW suffixes, normalize case, or convert numeric text to a
    float here: different native names must not collapse to a single identifier.
    Duplicate identical names use their occurrence in the COMPLETE source list.
    Producers may provide stronger native identities instead.
    """
    if isinstance(occurrence, bool) or not isinstance(occurrence, int) or occurrence < 0:
        raise ValueError("occurrence must be a non-negative integer")
    digest = hashlib.sha256(str(native_name).encode("utf-8")).hexdigest()
    key = f"series:{digest}"
    return key if occurrence == 0 else f"{key}:duplicate:{occurrence}"


def series_kind(native_name: str) -> str:
    name = str(native_name)
    if native_coefficient(name):
        return "bearing"
    if _MODE.fullmatch(name):
        return "mode"
    if _RESPONSE.fullmatch(name):
        return "response"
    order = _ORDER.fullmatch(name)
    if order is not None and float(order.group(1)) > 0.0:
        return "order"
    return "series"


@dataclass(frozen=True, slots=True)
class SeriesDescriptor:
    identity: str
    native_name: str
    display_name: str
    native_component: str = ""
    kind: str = "series"


def descriptors_for_names(
    names: Iterable[str], *, identities: Sequence[str] | None = None,
) -> list[SeriesDescriptor]:
    values = [str(name) for name in names]
    if identities is not None and len(identities) != len(values):
        raise ValueError("one identity is required for each native series")
    counts: dict[str, int] = defaultdict(int)
    result: list[SeriesDescriptor] = []
    used: set[str] = set()
    for index, name in enumerate(values):
        base = default_series_identity(name)
        occurrence = counts[base]
        counts[base] += 1
        identity = str(identities[index]) if identities is not None else default_series_identity(name, occurrence)
        if not identity or identity in used:
            raise ValueError(f"duplicate or empty explicit series identity: {identity!r}")
        used.add(identity)
        result.append(SeriesDescriptor(identity, name, bearing_display_text(name), native_coefficient(name), series_kind(name)))
    return result


def canonical_result_page(label: str) -> str:
    value = str(label).strip().casefold()
    return "bode" if value in {"resposta", "response", "bode"} else value


def result_fingerprint(sections: Mapping[str, str], run_identity: str = "") -> str:
    """Scope custom annotations to a report revision and run, not a rounded rpm."""
    digest = hashlib.sha256()
    for name, value in sorted(sections.items()):
        for part in (str(name), str(value)):
            data = part.encode("utf-8")
            digest.update(len(data).to_bytes(8, "big"))
            digest.update(data)
    data = str(run_identity).encode("utf-8")
    digest.update(len(data).to_bytes(8, "big"))
    digest.update(data)
    return digest.hexdigest()


def _finite(value: Any, field: str) -> float:
    if isinstance(value, bool):
        raise ValueError(f"{field}: boolean is not a numeric value")
    try:
        result = float(value)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{field}: a finite number is required") from exc
    if not math.isfinite(result):
        raise ValueError(f"{field}: a finite number is required")
    return result


def validate_operating_range(lower: Any, upper: Any, *, logarithmic: bool) -> tuple[float, float]:
    lo, hi = _finite(lower, "minimum rpm"), _finite(upper, "maximum rpm")
    if lo < 0.0 or hi < lo:
        raise ValueError("Rotação mínima deve ser ≥ 0 e máxima deve ser ≥ mínima.")
    if logarithmic and lo <= 0.0:
        raise ValueError("Faixa operacional no eixo logarítmico requer rotações > 0.")
    return lo, hi


def effective_map_range(configuration: Any, project_range: tuple[float, float] | None) -> tuple[float, float] | None:
    source = str(getattr(configuration, "map_range_source", "project"))
    if source == "project":
        if project_range is None:
            return None
        lower, upper = project_range
    elif source == "custom":
        lower = getattr(configuration, "map_operating_min_rpm", 0.0)
        upper = getattr(configuration, "map_operating_max_rpm", 0.0)
    else:
        raise ValueError("Origem da faixa deve ser Projeto ou Personalizada.")
    return validate_operating_range(lower, upper, logarithmic=str(getattr(configuration, "y_scale", "linear")) == "log")


def normalize_divisions(value: Any, legacy: Any = 5) -> int:
    chosen = legacy if value is None else value
    if isinstance(chosen, bool):
        raise ValueError("divisions must be an integer")
    number = _finite(chosen, "divisions")
    if number != int(number) or not 2 <= number <= 12:
        raise ValueError("divisions must be an integer between 2 and 12")
    return int(number)


def major_ticks(lo: float, hi: float, scale: str, divisions: int = 5) -> list[float]:
    lo, hi = _finite(lo, "lower axis bound"), _finite(hi, "upper axis bound")
    if hi <= lo:
        raise ValueError("upper axis bound must exceed lower bound")
    count = normalize_divisions(divisions)
    if scale != "log":
        return [lo + (hi - lo) * i / count for i in range(count + 1)]
    if lo <= 0:
        raise ValueError("logarithmic axis requires positive bounds")
    left, right = math.log10(lo), math.log10(hi)
    exponents = list(range(math.ceil(left), math.floor(right) + 1))
    if len(exponents) >= 2:
        stride = max(1, math.ceil((len(exponents) - 1) / 12))
        chosen = exponents[::stride]
        if chosen[-1] != exponents[-1]:
            chosen.append(exponents[-1])
        return [10.0 ** exponent for exponent in chosen]
    return [10.0 ** (left + (right - left) * i / 4) for i in range(5)]


def minor_ticks(lo: float, hi: float, scale: str, divisions: int = 5, log_subdivisions: str = "all") -> list[float]:
    lo, hi = _finite(lo, "lower axis bound"), _finite(hi, "upper axis bound")
    if hi <= lo:
        raise ValueError("upper axis bound must exceed lower bound")
    count = normalize_divisions(divisions)
    if scale != "log":
        total = count * 4
        return [lo + (hi - lo) * i / total for i in range(1, total) if i % 4]
    if lo <= 0:
        raise ValueError("logarithmic axis requires positive bounds")
    multipliers = {"all": tuple(range(2, 10)), "2,5": (2, 5), "none": ()}.get(log_subdivisions)
    if multipliers is None:
        raise ValueError("unknown logarithmic subdivisions")
    values = []
    for exponent in range(math.floor(math.log10(lo)), math.ceil(math.log10(hi)) + 1):
        try:
            base = 10.0 ** exponent
        except OverflowError:
            continue
        for multiplier in multipliers:
            value = multiplier * base
            if lo < value < hi and math.isfinite(value):
                values.append(value)
    return values


def validated_references(rows: Iterable[Mapping[str, Any]]) -> list[dict[str, Any]]:
    result = []
    used: set[str] = set()
    for index, raw in enumerate(rows):
        if not isinstance(raw, Mapping):
            raise ValueError("reference must be an object")
        identity = str(raw.get("id", f"reference:{index}"))
        if not identity or identity in used:
            raise ValueError("reference identities must be non-empty and unique")
        used.add(identity)
        value = _finite(raw.get("value"), "reference value")
        label = str(raw.get("label", f"δ = {value:g}"))
        style = str(raw.get("style", "dash"))
        if style not in {"solid", "dash", "dot"}:
            raise ValueError("reference style must be solid, dash or dot")
        visible = raw.get("visible", True)
        if not isinstance(visible, bool):
            raise ValueError("reference visibility must be boolean")
        result.append({"id": identity, "value": value, "label": label, "style": style, "visible": visible})
    return result


def validated_aliases(raw: Any, known_ids: set[str] | None = None) -> dict[str, str]:
    if not isinstance(raw, Mapping):
        raise ValueError("aliases must be an object")
    result = {}
    for key, value in raw.items():
        if not isinstance(key, str) or not isinstance(value, str):
            raise ValueError("alias keys and texts must be strings")
        if known_ids is not None and key not in known_ids:
            continue
        if value.strip():
            result[key] = value
    return result


def validated_marker_state(raw: Any) -> dict[str, dict[str, Any]]:
    if not isinstance(raw, Mapping):
        raise ValueError("marker state must be an object")
    result = {}
    for identity, record in raw.items():
        if not isinstance(identity, str) or not isinstance(record, Mapping):
            raise ValueError("invalid marker state")
        item = {}
        for key in ("hidden_marker", "hidden_label"):
            flag = record.get(key, False)
            if not isinstance(flag, bool):
                raise ValueError("marker visibility must be boolean")
            item[key] = flag
        offset = record.get("offset")
        if offset is not None:
            if not isinstance(offset, (tuple, list)) or len(offset) != 2:
                raise ValueError("marker offset must have two coordinates")
            item["offset"] = [_finite(offset[0], "offset x"), _finite(offset[1], "offset y")]
        result[identity] = item
    return result


def read_log_decrement_limit(text: str) -> float | None:
    match = _LOG_LIMIT.search(text)
    if match is None:
        return None
    try:
        value = float(match.group(1).replace("D", "E").replace("d", "e"))
    except (ValueError, OverflowError):
        return None
    return value if math.isfinite(value) and value > 0.0 else None


def at_reported_limit(value: float, limit: float | None) -> bool:
    return limit is not None and math.isfinite(value) and value == limit


def clean_store(raw: Any) -> dict[str, Any]:
    if raw in (None, {}):
        return {"version": SCHEMA_VERSION, "results": {}}
    if not isinstance(raw, Mapping) or raw.get("version") != SCHEMA_VERSION:
        raise ValueError("unsupported result presentation settings version")
    results = raw.get("results", {})
    if not isinstance(results, Mapping):
        raise ValueError("result presentation settings must contain a results object")
    payload = json.loads(json.dumps(raw, ensure_ascii=False, allow_nan=False))
    for fingerprint, pages in payload["results"].items():
        if not isinstance(fingerprint, str) or not isinstance(pages, dict):
            raise ValueError("invalid result presentation record")
        if not all(isinstance(key, str) and isinstance(value, dict) for key, value in pages.items()):
            raise ValueError("invalid page presentation record")
    return payload


def read_page_state(store: Any, fingerprint: str, page: str) -> dict[str, Any]:
    payload = clean_store(store)
    return deepcopy(payload["results"].get(fingerprint, {}).get(page, {}))


def write_page_state(store: Any, fingerprint: str, page: str, state: Mapping[str, Any]) -> dict[str, Any]:
    payload = clean_store(store)
    safe_state = json.loads(json.dumps(dict(state), ensure_ascii=False, allow_nan=False))
    payload["results"].setdefault(str(fingerprint), {})[str(page)] = safe_state
    return payload


def scale_view_bounds(bounds: tuple[float, float, float, float] | None, y_factor: float) -> tuple[float, float, float, float] | None:
    if bounds is None:
        return None
    factor = _finite(y_factor, "unit factor")
    if factor <= 0:
        raise ValueError("unit factor must be positive")
    x0, x1, y0, y1 = map(float, bounds)
    result = x0, x1, y0 * factor, y1 * factor
    if not all(math.isfinite(value) for value in result) or x1 <= x0 or result[3] <= result[2]:
        raise ValueError("invalid viewport bounds")
    return result


__all__ = [
    "SETTINGS_KEY", "SCHEMA_VERSION", "COEFFICIENT_LABELS", "SeriesDescriptor",
    "coefficient_label", "bearing_display_text", "native_coefficient", "series_kind",
    "default_series_identity", "descriptors_for_names", "canonical_result_page",
    "result_fingerprint", "validate_operating_range", "effective_map_range",
    "normalize_divisions", "major_ticks", "minor_ticks", "validated_references",
    "validated_aliases", "validated_marker_state", "read_log_decrement_limit",
    "at_reported_limit", "clean_store", "read_page_state", "write_page_state",
    "scale_view_bounds",
]
