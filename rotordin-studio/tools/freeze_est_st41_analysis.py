#!/usr/bin/env python3
from __future__ import annotations

import argparse
from dataclasses import asdict
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
REFERENCE_ROOT = ROOT / "reference"
if str(REFERENCE_ROOT) not in sys.path:
    sys.path.insert(0, str(REFERENCE_ROOT))

import frontend_rotordin  # noqa: E402,F401
from frontend_rotordin.analysis_speed_contract import (  # noqa: E402
    common_bearing_table_speed_range,
    resolved_modal_speed_rpm,
    resolved_orbit_speed_rpm,
    set_campbell_speed_range,
)
from frontend_rotordin.legacy_import import load_legacy_project  # noqa: E402
from frontend_rotordin.results import parse_solver_sections  # noqa: E402
from frontend_rotordin.solver import RotordinRunner, SolverInputWriter  # noqa: E402

SOURCE_REPOSITORY = "alegrettiLeonardo/frontend_rotordin"
SOURCE_BRANCH = "fix/solver-audit-647d600b"
SOURCE_HEAD = "c669978bdcc42c06507eac8fda8b6dc39e8bce97"
SOLVER_TREE_SHA = "542cbbea29923090bb17562c4120a8d5646ffcba"
FIXTURE_NAME = "EST-ST41_1000_B3_60HZ_1675_63536.irdin.txt"

ANALYSES: dict[str, dict[str, Any]] = {
    "campbell": {
        "flags": ["-std", "-c", "-b"],
        "covers": ["campbell", "stability"],
        "qualified_range_rpm": [500.0, 3000.0],
        "reason": (
            "The source requests Campbell from 0 rpm, while both TABLE bearings start at "
            "500 rpm. Qualification uses the proven table-domain intersection 500–3000 rpm; "
            "no bearing extrapolation below 500 rpm is permitted."
        ),
    },
    "response": {
        "flags": ["-std", "-f", "-b"],
        "covers": ["unbalance-response"],
        "reason": (
            "The audited legacy speed contract converts d_div=400 from a division count into "
            "the current UNBFD increment after clipping the raw 300–3000 rpm range to the common "
            "TABLE domain 500–3000 rpm."
        ),
    },
    "line": {
        "flags": ["-std", "-s", "-b"],
        "covers": ["elastic-line"],
        "reason": (
            "The source file contains no explicit RKS field. The current frontend preserves the "
            "legacy/default RKS=0 static-bearing contract, which uses the solver fixed-stiffness "
            "path instead of inventing a TABLE evaluation speed."
        ),
    },
    "map": {
        "flags": ["-std", "-k", "-b"],
        "covers": ["critical-speed-map"],
        "reason": (
            "The current writer resolves SMSPD to nominal speed 1800 rpm when variable TABLE "
            "bearings are present and no explicit map speed is stored. 1800 rpm lies inside the "
            "common 500–4000 rpm TABLE domain."
        ),
    },
    "orbit": {
        "flags": ["-std", "-t", "-b"],
        "covers": ["orbit"],
        "reason": (
            "Legacy t_rpm is absent/zero, so the audited speed contract resolves the one-speed "
            "orbit to nominal speed 1800 rpm. TIMOEP=0 retains the native all-sections sentinel."
        ),
    },
}


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_text(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def compiler_version() -> str:
    fc = os.environ.get("FC", "gfortran")
    try:
        proc = subprocess.run(
            [fc, "--version"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
            timeout=10,
        )
    except (OSError, subprocess.SubprocessError):
        return "UNAVAILABLE"
    return (proc.stdout.splitlines() or ["UNAVAILABLE"])[0].strip()


def set_analysis(project: Any, kind: str) -> None:
    project.analyses.campbell = False
    project.analyses.dynamic_response = False
    project.analyses.modes = False
    project.analyses.line = False
    project.analyses.map = False
    project.analyses.orbit = False
    project.analyses.log_decrement = False
    if hasattr(project.analyses, "auto"):
        project.analyses.auto = False

    if kind == "campbell":
        # Campbell and log decrement are one native -c computation/report family.
        project.analyses.campbell = True
        project.analyses.log_decrement = True
        set_campbell_speed_range(project, 500.0, 3000.0)
    elif kind == "response":
        project.analyses.dynamic_response = True
    elif kind == "line":
        project.analyses.line = True
    elif kind == "map":
        project.analyses.map = True
    elif kind == "orbit":
        project.analyses.orbit = True
    else:
        raise ValueError(f"Unsupported qualification analysis: {kind}")


def jsonable(value: Any) -> Any:
    if isinstance(value, float) and not math.isfinite(value):
        if math.isnan(value):
            return "NaN"
        return "Infinity" if value > 0 else "-Infinity"
    if hasattr(value, "__dataclass_fields__"):
        return {key: jsonable(item) for key, item in asdict(value).items()}
    if isinstance(value, dict):
        return {str(key): jsonable(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [jsonable(item) for item in value]
    return value


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("analysis", choices=sorted(ANALYSES))
    parser.add_argument("--fixture", type=Path, default=ROOT / "tests" / "fixtures" / FIXTURE_NAME)
    parser.add_argument("--solver", type=Path, default=ROOT / "solver" / ("rotordin.exe" if os.name == "nt" else "rotordin"))
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    kind = args.analysis
    spec = ANALYSES[kind]
    fixture = args.fixture.resolve()
    solver = args.solver.resolve()
    output = (args.output or (ROOT / "artifacts" / f"est-st41-{kind}")).resolve()

    if output.exists():
        shutil.rmtree(output)
    output.mkdir(parents=True)

    project = load_legacy_project(fixture, asset_root=output / "bearing-assets")
    set_analysis(project, kind)

    table_range = common_bearing_table_speed_range(project)
    if table_range != (500.0, 4000.0):
        raise SystemExit(f"Unexpected common TABLE range: {table_range!r}")

    writer = SolverInputWriter()
    input_text = writer.render(project)
    (output / "input.txt").write_text(input_text, encoding="utf-8", newline="\n")
    (output / "project.json").write_text(
        json.dumps(project.to_dict(), ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
        newline="\n",
    )

    runner = RotordinRunner(executable=solver, run_root=output / "runtime")
    result = runner.run(project)

    if len(result.commands) != 1:
        raise SystemExit(f"Expected one isolated solver job, got {len(result.commands)}")
    actual_flags = result.commands[0][1:]
    if actual_flags != spec["flags"]:
        raise SystemExit(f"Unexpected flags {actual_flags}; expected {spec['flags']}")

    sections_dir = output / "sections"
    sections_dir.mkdir()
    for name, body in sorted(result.sections.items()):
        safe = name.replace("/", "_").replace("\\", "_")
        (sections_dir / safe).write_text(body, encoding="utf-8", newline="\n")

    (output / "stdout.out").write_text(result.stdout, encoding="utf-8", newline="\n")
    (output / "stderr.log").write_text(result.stderr, encoding="utf-8", newline="\n")
    shutil.copy2(result.run_dir / "run.json", output / "run.json")

    parsed = parse_solver_sections(result.sections, result.critical_speeds)
    parsed_payload = jsonable(parsed)
    (output / "parsed_results.json").write_text(
        json.dumps(parsed_payload, ensure_ascii=False, indent=2, allow_nan=False) + "\n",
        encoding="utf-8",
    )

    legacy = project.settings.get("legacy_import", {})
    response_grid = project.settings.get("legacy_response_grid", {})
    analysis_meta: dict[str, Any] = {
        "kind": kind,
        "covers": spec["covers"],
        "reason": spec["reason"],
        "command_flags": actual_flags,
        "bearing_table_common_range_rpm": list(table_range),
    }
    if "qualified_range_rpm" in spec:
        analysis_meta["qualified_range_rpm"] = spec["qualified_range_rpm"]
    if kind == "response":
        analysis_meta["response_grid"] = response_grid
    if kind == "map":
        analysis_meta["map_speed_rpm"] = 1800.0
    if kind == "orbit":
        analysis_meta["orbit_speed_rpm"] = resolved_orbit_speed_rpm(project)
        analysis_meta["orbit_position_mm"] = project.configuration.orbit_position_mm

    qualification = {
        "schema_version": 1,
        "qualification_id": f"A11-EST-ST41-{kind.upper()}-V1",
        "status": "REAL_SOLVER_OUTPUT_FROZEN_CANDIDATE",
        "source_authority": {
            "repository": SOURCE_REPOSITORY,
            "branch": SOURCE_BRANCH,
            "head": SOURCE_HEAD,
            "solver_tree_sha": SOLVER_TREE_SHA,
        },
        "fixture": {
            "name": fixture.name,
            "sha256": sha256_file(fixture),
            "reference": project.identification.reference,
            "component": project.identification.component,
            "nominal_rpm": project.identification.rpm_nominal,
        },
        "analysis": analysis_meta,
        "artifacts": {
            "solver_executable_sha256": sha256_file(solver),
            "input_sha256": sha256_text(input_text),
            "stdout_sha256": sha256_text(result.stdout),
            "stderr_sha256": sha256_text(result.stderr),
            "sections": {name: sha256_text(body) for name, body in sorted(result.sections.items())},
            "parsed_results_sha256": sha256_file(output / "parsed_results.json"),
        },
        "environment": {
            "platform": platform.platform(),
            "python": sys.version.split()[0],
            "compiler": compiler_version(),
        },
        "legacy_import_warnings": legacy.get("warnings", []),
    }
    (output / "qualification.json").write_text(
        json.dumps(qualification, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )

    print(json.dumps({
        "qualification_id": qualification["qualification_id"],
        "analysis": kind,
        "flags": actual_flags,
        "sections": sorted(result.sections),
        "input_sha256": qualification["artifacts"]["input_sha256"],
        "parsed_results_sha256": qualification["artifacts"]["parsed_results_sha256"],
        "output": str(output),
    }, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
