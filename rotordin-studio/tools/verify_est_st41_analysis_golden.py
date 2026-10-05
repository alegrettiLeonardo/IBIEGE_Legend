#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import sys
from typing import Any


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def compare(expected: Any, actual: Any, path: str, errors: list[str], *, rtol: float, atol: float) -> None:
    if expected is None or actual is None:
        if expected != actual:
            errors.append(f"{path}: expected {expected!r}, actual {actual!r}")
        return
    if isinstance(expected, bool) or isinstance(actual, bool):
        if expected != actual:
            errors.append(f"{path}: expected {expected!r}, actual {actual!r}")
        return
    if isinstance(expected, (int, float)) and isinstance(actual, (int, float)):
        if not math.isclose(float(expected), float(actual), rel_tol=rtol, abs_tol=atol):
            errors.append(f"{path}: expected {expected:.17g}, actual {actual:.17g}")
        return
    if isinstance(expected, str) and isinstance(actual, str):
        if expected != actual:
            errors.append(f"{path}: expected {expected!r}, actual {actual!r}")
        return
    if isinstance(expected, list) and isinstance(actual, list):
        if len(expected) != len(actual):
            errors.append(f"{path}: length expected {len(expected)}, actual {len(actual)}")
            return
        for index, (e, a) in enumerate(zip(expected, actual, strict=True)):
            compare(e, a, f"{path}[{index}]", errors, rtol=rtol, atol=atol)
        return
    if isinstance(expected, dict) and isinstance(actual, dict):
        if set(expected) != set(actual):
            errors.append(f"{path}: key set changed")
            return
        for key in sorted(expected):
            compare(expected[key], actual[key], f"{path}.{key}", errors, rtol=rtol, atol=atol)
        return
    if expected != actual:
        errors.append(f"{path}: expected {expected!r}, actual {actual!r}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--golden", type=Path, required=True)
    parser.add_argument("--rtol", type=float, default=2e-5)
    parser.add_argument("--atol", type=float, default=1e-9)
    args = parser.parse_args()

    candidate = args.candidate.resolve()
    golden = args.golden.resolve()
    errors: list[str] = []

    for name in ("qualification.json", "input.txt", "parsed_results.json", "run.json"):
        if not (candidate / name).is_file():
            errors.append(f"candidate missing {name}")
        if not (golden / name).is_file():
            errors.append(f"golden missing {name}")
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1

    candidate_input = (candidate / "input.txt").read_bytes()
    golden_input = (golden / "input.txt").read_bytes()
    if candidate_input != golden_input:
        errors.append(
            "input.txt differs from frozen native contract: "
            f"golden={sha256(golden / 'input.txt')} candidate={sha256(candidate / 'input.txt')}"
        )
        g_lines = golden_input.decode("utf-8", errors="replace").splitlines()
        c_lines = candidate_input.decode("utf-8", errors="replace").splitlines()
        for index, (g_line, c_line) in enumerate(zip(g_lines, c_lines)):
            if g_line != c_line:
                errors.append(
                    f"input first differing line {index + 1}: golden={g_line!r} candidate={c_line!r}"
                )
                break
        if len(g_lines) != len(c_lines):
            errors.append(
                f"input line count differs: golden={len(g_lines)} candidate={len(c_lines)}"
            )

    cq = json.loads((candidate / "qualification.json").read_text(encoding="utf-8"))
    gq = json.loads((golden / "qualification.json").read_text(encoding="utf-8"))
    for key in ("repository", "branch", "head", "solver_tree_sha"):
        if cq["source_authority"][key] != gq["source_authority"][key]:
            errors.append(f"source_authority.{key} changed")
    for key in ("kind", "covers", "command_flags"):
        if cq["analysis"][key] != gq["analysis"][key]:
            errors.append(f"analysis.{key} changed: {gq['analysis'][key]!r} -> {cq['analysis'][key]!r}")

    expected_sections = set(gq["artifacts"]["sections"])
    actual_sections = set(cq["artifacts"]["sections"])
    if expected_sections != actual_sections:
        errors.append(f"section set changed: expected {sorted(expected_sections)}, actual {sorted(actual_sections)}")

    expected = json.loads((golden / "parsed_results.json").read_text(encoding="utf-8"))
    actual = json.loads((candidate / "parsed_results.json").read_text(encoding="utf-8"))

    # RD_AUDIT_V1 contains the complete adaptive numerical trace.  The number
    # and exact ordering of refinement/search records can legitimately differ
    # across LAPACK/OpenBLAS/compiler platforms while converging to the same
    # reported engineering result (Campbell crossings, response, line, map,
    # orbit).  Keep rdaudit.out frozen as provenance, but do not make its raw
    # trace a cross-platform equality gate.  The stable parsed engineering
    # outputs remain tolerance-gated below.
    for payload, label in ((expected, "golden"), (actual, "candidate")):
        audit = payload.pop("numerical_audit", None)
        if isinstance(audit, dict) and audit.get("parse_errors"):
            errors.append(f"{label} numerical_audit contains parser errors: {audit['parse_errors']!r}")

    compare(expected, actual, "parsed_results", errors, rtol=args.rtol, atol=args.atol)

    if errors:
        print("EST-ST41 ANALYSIS GOLDEN: FAIL", file=sys.stderr)
        for error in errors[:200]:
            print(f" - {error}", file=sys.stderr)
        if len(errors) > 200:
            print(f" ... {len(errors)-200} additional differences", file=sys.stderr)
        return 1

    print("EST-ST41 ANALYSIS GOLDEN: PASS")
    print(f" qualification: {cq['qualification_id']}")
    print(f" input sha256: {sha256(candidate / 'input.txt')}")
    print(f" parsed sha256: {sha256(candidate / 'parsed_results.json')}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
