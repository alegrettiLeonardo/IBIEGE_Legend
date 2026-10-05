#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
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

# Importing the package executes the same contract-install order as the
# authoritative frontend_rotordin branch pinned in solver/SOURCE_PROVENANCE.json.
from frontend_rotordin.analysis_speed_contract import (  # noqa: E402
    common_bearing_table_speed_range,
    resolved_modal_speed_rpm,
)
from frontend_rotordin.legacy_import import load_legacy_project  # noqa: E402
from frontend_rotordin.solver import RotordinRunner, SolverInputWriter  # noqa: E402


SOURCE_REPOSITORY = "alegrettiLeonardo/frontend_rotordin"
SOURCE_BRANCH = "fix/solver-audit-647d600b"
SOURCE_HEAD = "c669978bdcc42c06507eac8fda8b6dc39e8bce97"
SOLVER_TREE_SHA = "542cbbea29923090bb17562c4120a8d5646ffcba"
FIXTURE_NAME = "EST-ST41_1000_B3_60HZ_1675_63536.irdin.txt"


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_text(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def command_text(command: list[str]) -> str:
    return " ".join(command)


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


def copy_text(source: Path, target: Path) -> None:
    if source.is_file():
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(source.read_text(encoding="utf-8"), encoding="utf-8", newline="\n")


def set_modes_only(project: Any) -> None:
    # The source irDin file stores analysis parameters but does not contain an
    # authoritative enabled-analysis bitset.  Freeze one deterministic physical
    # contract first: prescribed-spin modes at nominal rpm.
    project.analyses.campbell = False
    project.analyses.dynamic_response = False
    project.analyses.modes = True
    project.analyses.line = False
    project.analyses.map = False
    project.analyses.orbit = False
    project.analyses.log_decrement = False
    if hasattr(project.analyses, "auto"):
        project.analyses.auto = False


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Freeze a real RotorDin solver result for EST-ST41 using the pinned audited Fortran source."
    )
    parser.add_argument(
        "--fixture",
        type=Path,
        default=ROOT / "tests" / "fixtures" / FIXTURE_NAME,
    )
    parser.add_argument(
        "--solver",
        type=Path,
        default=ROOT / "solver" / ("rotordin.exe" if os.name == "nt" else "rotordin"),
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=ROOT / "artifacts" / "est-st41-modes",
    )
    args = parser.parse_args()

    fixture = args.fixture.resolve()
    solver = args.solver.resolve()
    output = args.output.resolve()

    if not fixture.is_file():
        raise SystemExit(f"Fixture not found: {fixture}")
    if not solver.is_file():
        raise SystemExit(f"Solver executable not found: {solver}")

    if output.exists():
        shutil.rmtree(output)
    output.mkdir(parents=True)

    asset_root = output / "bearing-assets"
    project = load_legacy_project(fixture, asset_root=asset_root)
    set_modes_only(project)

    # Contract checks derived from the audited current frontend:
    # - TABLE bearings in this case share 500..4000 rpm.
    # - nominal rpm=1800 is inside that range and becomes MDRPM for Modes.
    table_range = common_bearing_table_speed_range(project)
    modal_rpm = resolved_modal_speed_rpm(project)
    if table_range is None:
        raise SystemExit("Expected speed-dependent TABLE bearings in EST-ST41.")
    if not (table_range[0] <= modal_rpm <= table_range[1]):
        raise SystemExit(
            f"Modal rpm {modal_rpm:g} is outside common TABLE range "
            f"{table_range[0]:g}..{table_range[1]:g} rpm."
        )

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

    # This freeze is intentionally a single qualified Modes invocation.
    expected_flags = ["-std", "-m", "-b"]
    if len(result.commands) != 1:
        raise SystemExit(f"Expected exactly one solver job, got {len(result.commands)}.")
    if result.commands[0][1:] != expected_flags:
        raise SystemExit(
            f"Unexpected solver flags: {result.commands[0][1:]}; expected {expected_flags}."
        )

    (output / "stdout.out").write_text(result.stdout, encoding="utf-8", newline="\n")
    (output / "stderr.log").write_text(result.stderr, encoding="utf-8", newline="\n")

    section_dir = output / "sections"
    section_dir.mkdir()
    for name, body in sorted(result.sections.items()):
        safe = name.replace("/", "_").replace("\\", "_")
        (section_dir / safe).write_text(body, encoding="utf-8", newline="\n")

    # Preserve the raw per-job evidence emitted by the authoritative runner.
    runtime_run = result.run_dir
    for name in ("run.json", "rdaudit.out"):
        copy_text(runtime_run / name, output / name)
    job_dirs = sorted(path for path in runtime_run.iterdir() if path.is_dir())
    if len(job_dirs) == 1:
        for name in ("input.txt", "stdout.out", "stderr.log", "rdaudit.out"):
            copy_text(job_dirs[0] / name, output / "job" / name)

    source_manifest = ROOT / "solver" / "SOURCE_MANIFEST.sha256"
    provenance_path = ROOT / "solver" / "SOURCE_PROVENANCE.json"

    metadata = project.settings.get("legacy_import", {})
    response_grid = project.settings.get("legacy_response_grid", {})
    qualification = {
        "schema_version": 1,
        "qualification_id": "A10-EST-ST41-MODES-V1",
        "status": "REAL_SOLVER_OUTPUT_FROZEN_CANDIDATE",
        "source_authority": {
            "repository": SOURCE_REPOSITORY,
            "branch": SOURCE_BRANCH,
            "head": SOURCE_HEAD,
            "solver_tree_sha": SOLVER_TREE_SHA,
            "source_manifest_sha256": sha256_file(source_manifest),
            "source_provenance_sha256": sha256_file(provenance_path),
        },
        "fixture": {
            "name": fixture.name,
            "sha256": sha256_file(fixture),
            "reference": project.identification.reference,
            "component": project.identification.component,
            "nominal_rpm": project.identification.rpm_nominal,
        },
        "analysis": {
            "kind": "modes",
            "reason": (
                "The source file has no authoritative enabled-analysis bitset. "
                "Modes at nominal speed exercises the exact shaft/mass/bearing/support "
                "contract without altering the file's Campbell range."
            ),
            "modal_rpm": modal_rpm,
            "bearing_table_common_range_rpm": list(table_range),
            "command": result.commands[0],
            "command_text": command_text(result.commands[0]),
        },
        "legacy_contract": {
            "package_divisions": project.configuration.package_divisions,
            "disk_density_kg_m3": project.configuration.disk_density_kg_m3,
            "campbell_raw_rpm": [
                project.identification.rpm_initial,
                project.identification.rpm_final,
            ],
            "response_grid": response_grid,
            "import_warnings": metadata.get("warnings", []),
        },
        "artifacts": {
            "solver_executable_sha256": sha256_file(solver),
            "input_sha256": sha256_text(input_text),
            "stdout_sha256": sha256_text(result.stdout),
            "stderr_sha256": sha256_text(result.stderr),
            "sections": {
                name: sha256_text(body)
                for name, body in sorted(result.sections.items())
            },
        },
        "environment": {
            "platform": platform.platform(),
            "python": sys.version.split()[0],
            "compiler": compiler_version(),
        },
    }

    (output / "qualification.json").write_text(
        json.dumps(qualification, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
        newline="\n",
    )

    print(json.dumps({
        "qualification_id": qualification["qualification_id"],
        "solver_sha256": qualification["artifacts"]["solver_executable_sha256"],
        "input_sha256": qualification["artifacts"]["input_sha256"],
        "stdout_sha256": qualification["artifacts"]["stdout_sha256"],
        "sections": sorted(result.sections),
        "modal_rpm": modal_rpm,
        "bearing_table_common_range_rpm": list(table_range),
        "output": str(output),
    }, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
