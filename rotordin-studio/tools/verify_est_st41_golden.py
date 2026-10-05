#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import sys
from typing import Any


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def parse_audit(path: Path) -> dict[str, Any]:
    bearings: dict[int, dict[str, Any]] = {}
    modes: dict[int, dict[str, Any]] = {}
    spectrum: dict[str, Any] | None = None

    for line in path.read_text(encoding="utf-8").splitlines():
        fields = line.split()
        if len(fields) < 2 or fields[0] != "RD_AUDIT_V1":
            continue

        kind = fields[1]
        if kind == "BEARING_COEFFICIENTS":
            bearing = int(fields[2])
            bearings[bearing] = {
                "bearing": bearing,
                "table": int(fields[3]),
                "rpm": float(fields[4]),
                "coefficients": [float(item) for item in fields[5:15]],
            }
        elif kind == "MODE":
            mode = int(fields[3])
            modes[mode] = {
                "rpm": float(fields[2]),
                "mode": mode,
                "eigenpair": int(fields[4]),
                "frequency_hz": float(fields[5]),
                "pole_magnitude_rad_s": float(fields[6]),
                "growth_s_inv": float(fields[7]),
                "damping_ratio": float(fields[8]),
                "log_decrement": float(fields[9]),
                "whirl": fields[10],
            }
        elif kind == "SPECTRUM" and fields[2] == "MODES":
            spectrum = {
                "context": fields[2],
                "rpm": float(fields[3]),
                "status": fields[4],
                "maximum_growth_s_inv": float(fields[5]),
                "total": int(fields[6]),
                "finite": int(fields[7]),
                "invalid": int(fields[8]),
                "unstable": int(fields[9]),
                "nonoscillatory": int(fields[10]),
            }

    return {
        "bearings": bearings,
        "modes": modes,
        "spectrum": spectrum,
    }


def close(a: float, b: float, *, rtol: float, atol: float) -> bool:
    return math.isclose(a, b, rel_tol=rtol, abs_tol=atol)


def compare_float(label: str, expected: float, actual: float, errors: list[str], *, rtol: float, atol: float) -> None:
    if not close(expected, actual, rtol=rtol, atol=atol):
        errors.append(
            f"{label}: expected {expected:.17g}, actual {actual:.17g}, "
            f"rtol={rtol:g}, atol={atol:g}"
        )


def normalized_output(path: Path) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    if lines and re.match(r"^\d{2}-[A-Z]{2}-\d{4}@", lines[0]):
        lines = lines[1:]
    return "\n".join(line.rstrip() for line in lines).strip() + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare a fresh EST-ST41 Modes run with the frozen A10 scientific golden."
    )
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--golden", type=Path, required=True)
    args = parser.parse_args()

    candidate = args.candidate.resolve()
    golden = args.golden.resolve()
    errors: list[str] = []

    required = [
        "qualification.json",
        "input.txt",
        "sections/output.out",
        "sections/rdaudit.out",
        "sections/modeshp.out",
    ]
    for relative in required:
        if not (candidate / relative).is_file():
            errors.append(f"candidate missing {relative}")
        if not (golden / relative).is_file():
            errors.append(f"golden missing {relative}")

    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1

    # Solver input is fixed-width and must be byte-identical for this source
    # contract. This detects any frontend translation drift before physics runs.
    if (candidate / "input.txt").read_bytes() != (golden / "input.txt").read_bytes():
        errors.append(
            "input.txt is not byte-identical to the frozen fixed-width solver input "
            f"(golden={sha256(golden / 'input.txt')}, candidate={sha256(candidate / 'input.txt')})"
        )

    cq = json.loads((candidate / "qualification.json").read_text(encoding="utf-8"))
    gq = json.loads((golden / "qualification.json").read_text(encoding="utf-8"))
    for key in ("repository", "branch", "head", "solver_tree_sha"):
        ev = gq["source_authority"][key]
        av = cq["source_authority"][key]
        if ev != av:
            errors.append(f"source_authority.{key}: expected {ev!r}, actual {av!r}")

    if cq["analysis"]["kind"] != "modes" or cq["analysis"]["modal_rpm"] != 1800.0:
        errors.append("candidate is not the qualified EST-ST41 Modes@1800 rpm analysis")

    golden_audit = parse_audit(golden / "sections" / "rdaudit.out")
    candidate_audit = parse_audit(candidate / "sections" / "rdaudit.out")

    if set(golden_audit["bearings"]) != set(candidate_audit["bearings"]):
        errors.append("bearing audit record set changed")
    else:
        for bearing in sorted(golden_audit["bearings"]):
            e = golden_audit["bearings"][bearing]
            a = candidate_audit["bearings"][bearing]
            if e["table"] != a["table"]:
                errors.append(f"bearing {bearing} table id changed")
            compare_float(f"bearing {bearing} rpm", e["rpm"], a["rpm"], errors, rtol=0.0, atol=1e-10)
            for index, (ev, av) in enumerate(zip(e["coefficients"], a["coefficients"], strict=True), start=1):
                compare_float(
                    f"bearing {bearing} coefficient {index}",
                    ev, av, errors,
                    rtol=1e-10, atol=1e-5,
                )

    if set(golden_audit["modes"]) != set(candidate_audit["modes"]):
        errors.append(
            f"mode set changed: expected {sorted(golden_audit['modes'])}, "
            f"actual {sorted(candidate_audit['modes'])}"
        )
    else:
        for mode in sorted(golden_audit["modes"]):
            e = golden_audit["modes"][mode]
            a = candidate_audit["modes"][mode]
            if e["whirl"] != a["whirl"]:
                errors.append(f"mode {mode} whirl changed: {e['whirl']} -> {a['whirl']}")
            # Eigenpair numbering is part of the current solver contract.
            if e["eigenpair"] != a["eigenpair"]:
                errors.append(f"mode {mode} eigenpair changed: {e['eigenpair']} -> {a['eigenpair']}")
            for field, rtol, atol in (
                ("rpm", 0.0, 1e-9),
                ("frequency_hz", 2e-7, 1e-7),
                ("pole_magnitude_rad_s", 2e-7, 1e-6),
                ("growth_s_inv", 2e-6, 1e-6),
                ("damping_ratio", 2e-6, 1e-9),
                ("log_decrement", 2e-6, 1e-9),
            ):
                compare_float(
                    f"mode {mode} {field}",
                    e[field], a[field], errors,
                    rtol=rtol, atol=atol,
                )

    es = golden_audit["spectrum"]
    ac = candidate_audit["spectrum"]
    if es is None or ac is None:
        errors.append("MODES SPECTRUM audit record missing")
    else:
        for field in ("context", "status", "total", "finite", "invalid", "unstable", "nonoscillatory"):
            if es[field] != ac[field]:
                errors.append(f"spectrum {field}: expected {es[field]!r}, actual {ac[field]!r}")
        compare_float(
            "spectrum maximum_growth_s_inv",
            es["maximum_growth_s_inv"],
            ac["maximum_growth_s_inv"],
            errors,
            rtol=5e-6,
            atol=1e-8,
        )

    # The summary is deterministic except for the first solver stamp line.
    if normalized_output(candidate / "sections" / "output.out") != normalized_output(golden / "sections" / "output.out"):
        errors.append("normalized output.out summary changed")

    if errors:
        print("EST-ST41 GOLDEN VERIFICATION: FAIL", file=sys.stderr)
        for error in errors:
            print(f" - {error}", file=sys.stderr)
        return 1

    print("EST-ST41 GOLDEN VERIFICATION: PASS")
    print(f" input sha256: {sha256(candidate / 'input.txt')}")
    print(f" modes checked: {len(candidate_audit['modes'])}")
    print(f" bearings checked: {len(candidate_audit['bearings'])}")
    print(f" spectrum: {candidate_audit['spectrum']['status']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
