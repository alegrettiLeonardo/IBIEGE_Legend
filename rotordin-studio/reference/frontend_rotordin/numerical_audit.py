"""Versioned solver diagnostics. No recomputation of physics or certification.

The raw ``rdaudit.out`` section is retained in SolverResult and therefore follows
its existing save/reopen/export path. Unknown future records remain inspectable.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

PREFIX = "RD_AUDIT_V1 "
WHIRL_LABELS = {
    "FW": "Precessão direta", "BW": "Precessão retrógrada",
    "MX": "Precessão mista ao longo do eixo", "LN": "Órbita aproximadamente linear",
    "ND": "Deslocamento lateral desprezível", "UD": "Sentido relativo indefinido",
}

# Column order is shared with the explicit Fortran WRITE statements.
_SCHEMAS: dict[str, tuple[tuple[str, type], ...]] = {
    # HF03: physical poles s, final monotonic spectral-group partition.
    # Quality values belong to the incoming accepted partition, not to an
    # individual eigenvector or a retroactively merged group.
    "FAMILY": (("rpm", float), ("group", int), ("pole_count", int),
               ("selected_members", int), ("regime", str),
               ("accepted_partition_min_cos2", float),
               ("accepted_partition_min_separation", float),
               ("accepted_partition_max_residual", float)),
    "FAMILY_MEMBER": (("rpm", float), ("group", int), ("initial_family", int)),
    "FAMILY_POLE": (("rpm", float), ("group", int), ("local_pole_index", int),
                    ("physical_real_s_inv", float), ("physical_imag_rad_s", float)),
    "FAMILY_FREQUENCY": (("rpm", float), ("group", int), ("frequency_rank", int),
                         ("frequency_rad_s", float)),
    "FAMILY_MIN_COS2": (("rpm", float), ("accepted_partition_min_cos2", float)),
    "FAMILY_METRIC": (("metric", str), ("reference_frequency_rad_s", float)),
    "FAMILY_EVENT_BRACKET": (("lower_rpm", float), ("upper_rpm", float)),
    "FAMILY_MERGE": (("rpm", float), ("previous_group", int), ("union_group", int)),
    "FAMILY_CROSSING": (("order_index", int), ("group", int), ("frequency_rank", int),
                        ("kind", str), ("order", float), ("rpm", float),
                        ("frequency_rad_s", float), ("bracket_width_rpm", float),
                        ("normalized_crossing_residual", float)),

    "SPECTRUM": (("context", str), ("rpm", float), ("status", str),
                 ("maximum_growth_s_inv", float), ("total", int), ("finite", int),
                 ("invalid", int), ("unstable", int), ("nonoscillatory", int)),
    "STABILITY_ROOT": (("context", str), ("rpm", float), ("eigenpair", int),
                       ("growth_s_inv", float), ("physical_imag_rad_s", float), ("damped_frequency_rad_s", float)),
    "ROOT_STATUS": (("context", str), ("rpm", float), ("eigenpair", int), ("status", int),
                    ("alpha_real", float), ("alpha_imag", float),
                    ("beta_real", float), ("beta_imag", float)),
    "MODE": (("rpm", float), ("mode", int), ("eigenpair", int), ("frequency_hz", float),
             ("pole_magnitude_rad_s", float), ("growth_s_inv", float),
             ("damping_ratio", float), ("log_decrement_raw", float), ("whirl", str)),
    "WHIRL_NODE": (("rpm", float), ("mode", int), ("node", int), ("whirl", str)),
    "RESPONSE_H": (("rpm", float), ("status", str), ("augmented_H_rcond_1", float),
                   ("augmented_H_norm_1", float), ("augmented_residual", float),
                   ("physical_equilibrium_residual", float), ("kinematic_residual", float)),
    "CROSSING": (("order_index", int), ("branch", int), ("kind", str), ("order", float),
                 ("rpm", float), ("frequency_rad_s", float), ("bracket_width_rpm", float),
                 ("normalized_crossing_residual", float)),
    "TRACK_SUBSPACE": (("cluster", int), ("requested_vectors", int), ("cluster_vectors", int),
                       ("minimum_principal_cosine_squared", float)),
    "TRACK_MIN_MAC": (("rpm", float), ("minimum_mass_MAC", float)),
    "CAMPBELL_POLICY": (("display_tracking", int), ("requested_speed_samples", int)),
    "CAMPBELL_REFINE": (("rpm", float), ("attempt", int)),
    "TANGENCY_SEARCH": (("rpm", float), ("interval_width_rpm", float),
                        ("normalized_residual", float), ("result", int)),
    "CROSSING_COVERAGE": (("scope", str), ("accepted_speed_samples", int)),
    "BEARING_EXTRAP": (("table", int), ("rpm", float), ("minimum_table_rpm", float),
                       ("maximum_table_rpm", float)),
    "BEARING_COEFFICIENTS": (("bearing", int), ("table", int), ("rpm", float),
         *((name, float) for name in ("Kxx", "Kxz", "Kzx", "Kzz", "Cxx", "Cxz", "Czx", "Czz", "Kphi", "Ktheta"))),
}

@dataclass(slots=True)
class AuditRecord:
    kind: str
    values: dict[str, Any]
    raw: str

@dataclass(slots=True)
class NumericalAudit:
    records: list[AuditRecord] = field(default_factory=list)
    parse_errors: list[str] = field(default_factory=list)
    raw: str = ""

    def select(self, kind: str) -> list[AuditRecord]:
        return [record for record in self.records if record.kind == kind]

    @property
    def qualified(self) -> bool:
        """Diagnostics are not a machine/solver qualification certificate."""
        return False


def parse_numerical_audit(text: str, *, strict: bool = False) -> NumericalAudit:
    result = NumericalAudit(raw=text)
    for line_number, raw in enumerate(text.splitlines(), 1):
        line = raw.strip()
        if not line.startswith(PREFIX):
            continue
        fields = line[len(PREFIX):].split()
        if not fields:
            result.parse_errors.append(f"Linha {line_number}: registro vazio")
            continue
        kind, tokens = fields[0], fields[1:]
        schema = _SCHEMAS.get(kind)
        if schema is None:
            result.records.append(AuditRecord(kind, {"text": " ".join(tokens)}, raw))
            continue
        try:
            if len(tokens) != len(schema):
                raise ValueError(f"{kind}: {len(tokens)} campos; esperados {len(schema)}")
            values = {}
            for (name, converter), value in zip(schema, tokens):
                if converter is float:
                    value = value.replace("D", "E").replace("d", "e")
                values[name] = converter(value)
            result.records.append(AuditRecord(kind, values, raw))
        except ValueError as exc:
            result.parse_errors.append(f"Linha {line_number}: {exc}")
    if strict and result.parse_errors:
        raise ValueError("\n".join(result.parse_errors))
    return result


def audit_from_sections(sections: dict[str, str]) -> NumericalAudit | None:
    text = sections.get("rdaudit.out", "")
    if not text:
        text = "\n".join(line for body in sections.values() for line in body.splitlines()
                         if line.strip().startswith(PREFIX))
    return parse_numerical_audit(text) if text.strip() else None


def excitation_orders(lines: list[str]) -> tuple[float, ...]:
    for line in lines:
        if line.strip().startswith("EXCITATION_ORDERS "):
            import math
            values = tuple(float(token.replace("D", "E").replace("d", "e"))
                           for token in line.split()[1:])
            if not values or any(not math.isfinite(value) or value <= 0 for value in values):
                raise ValueError("EXCITATION_ORDERS inválido no relatório do solver")
            return values
    return (1.0, 2.0, 0.5)  # Explicit legacy fallback only; new reports have metadata.


def harmonic_label(value: float) -> str:
    return f"{value:.1f}x" if float(value).is_integer() else f"{value:.12g}x"
