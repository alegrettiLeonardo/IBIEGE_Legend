from __future__ import annotations

"""Process-launch helpers shared by RotorDin scientific runners.

The Windows build stores MinGW/OpenBLAS runtime DLLs under ``solver/runtime``.
Windows does not recursively search that directory when ``rotordin.exe`` is
started from a calculation run directory, so the child process needs an explicit
PATH prefix.  This module changes only process discovery; it does not alter solver
input, FEM assembly or any rotordynamic physics.
"""

import os
import subprocess
from pathlib import Path


WINDOWS_DLL_NOT_FOUND = 0xC0000135
WINDOWS_DLL_NOT_FOUND_SIGNED = -1073741515
WINDOWS_CREATE_NO_WINDOW = 0x08000000


def solver_process_creationflags() -> int:
    """Return platform-safe creation flags for the scientific solver child.

    The frozen Windows frontend is a GUI process. Launching the console-subsystem
    Fortran executable without CREATE_NO_WINDOW opens Windows Terminal/conhost for
    each analysis job. Hiding that child console preserves stdin/stdout/stderr
    pipes and does not change any solver physics.
    """

    if os.name != "nt":
        return 0
    return int(getattr(subprocess, "CREATE_NO_WINDOW", WINDOWS_CREATE_NO_WINDOW))


def solver_process_environment(
    executable: str | Path,
    *,
    unbuffered_fortran: bool = False,
) -> dict[str, str]:
    """Return an isolated child environment with packaged solver DLL paths.

    ``solver/runtime`` and the executable directory are prepended only for the
    child process.  The parent Python/Qt environment is never mutated.
    """

    env = os.environ.copy()
    exe = Path(executable).expanduser().resolve()
    runtime_dirs: list[Path] = []
    for candidate in (exe.parent / "runtime", exe.parent):
        if candidate.is_dir() and candidate not in runtime_dirs:
            runtime_dirs.append(candidate)

    if runtime_dirs:
        existing = env.get("PATH", "")
        prefix = os.pathsep.join(str(path) for path in runtime_dirs)
        env["PATH"] = prefix + (os.pathsep + existing if existing else "")
    if unbuffered_fortran:
        env["GFORTRAN_UNBUFFERED_ALL"] = "1"
    return env


def windows_solver_launch_error(returncode: int, executable: str | Path) -> str | None:
    """Explain the Windows loader error that otherwise appears as a large integer."""

    if int(returncode) not in (WINDOWS_DLL_NOT_FOUND, WINDOWS_DLL_NOT_FOUND_SIGNED):
        return None
    exe = Path(executable).expanduser().resolve()
    runtime = exe.parent / "runtime"
    return (
        "Windows não conseguiu carregar uma DLL necessária do RotorDin "
        "(0xC0000135 / STATUS_DLL_NOT_FOUND). "
        f"Executável: {exe}. Runtime esperado: {runtime}. "
        "Verifique as DLLs MinGW/OpenBLAS produzidas por solver/build_solver.py."
    )


__all__ = [
    "solver_process_creationflags",
    "solver_process_environment",
    "windows_solver_launch_error",
]
