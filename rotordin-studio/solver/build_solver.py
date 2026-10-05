from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path
import shutil
import subprocess


SOURCE_TREE_SHA256 = None  # per-file manifest is authoritative

SOURCES = [
    "rd_kinds.f90",
    "rd_sys.f90",
    "rd_commons.f90",
    "rd_textfun.f90",
    "rd_modal_metrics.f90",
    "rd_modal_tracking.f90",
    "rd_bearing_contract.f90",
    "rd_response_quality.f90",
    "rd_spectral_families.f90",
    "rd_campbell_families.f90",
    "rd_campbell_engine.f90",
    "blockd.f90",
    "progress.f90",
    "foundation.f90",
    "flexdisk.f90",
    "model_audit.f90",
    "calccm.f90",
    "campbl.f90",
    "entrada.f90",
    "espmod.f90",
    "expgeo.f90",
    "expmat.f90",
    "hpgl.f90",
    "hplots.f90",
    "indsec.f90",
    "init.f90",
    "linhael.f90",
    "maprig.f90",
    "matfun.f90",
    "shaft_timoshenko_cowper.f90",
    "matrizes.f90",
    "messages.f90",
    "modos.f90",
    "parmanv.f90",
    "predad.f90",
    "rdfra.f90",
    "resp_f.f90",
    "resp_fv.f90",
    "resp_t.f90",
    "transient_input.f90",
    "transient_speed.f90",
    "transient_matrix.f90",
    "transient_forcing.f90",
    "transient_newmark.f90",
    "transient_output.f90",
    "transient_solver.f90",
    "rotordin.f90",
    "saidas.f90",
    "lapack/blas.f",
    "lapack/cmpeig.f",
    "lapack/lapack.f",
    "torsion/tblockd.f90",
    "torsion/tcouprops.f90",
    "torsion/tentrada.f90",
    "torsion/tespmod.f90",
    "torsion/thplots.f90",
    "torsion/tinit.f90",
    "torsion/tindsec.f90",
    "torsion/tmatfun.f90",
    "torsion/tmatrices.f90",
    "torsion/tmodos.f90",
    "torsion/tpredad.f90",
    "torsion/tsaidas.f90",
    "torsion/tsecdl.f90",
    "torsion/tstatic.f90",
    "torsion/tstrange.f90",
    "torsion/ttrqdiv.f90",
    "torsion/tcmpbl.f90",
    "torsion/tresp_f.f90",
    "torsion/thhmat.f90",
    "torsion/tpefors.f90",
    "torsion/tprptran.f90",
    "torsion/tnuinteg.f90",
    "torsion/tflextor.f90",
    "torsion/tpefora.f90",
    "torsion/evalf/evalf.f90",
]


def solver_dir() -> Path:
    return Path(__file__).resolve().parent


def source_dir() -> Path:
    return solver_dir() / "src" / "source"


def manifest_path() -> Path:
    return solver_dir() / "SOURCE_MANIFEST.sha256"


def _read_manifest() -> dict[str, str]:
    path = manifest_path()
    if not path.is_file():
        raise SystemExit(f"RotorDin source manifest not found: {path}")
    expected: dict[str, str] = {}
    for lineno, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        try:
            digest, rel = line.split(None, 1)
        except ValueError as exc:
            raise SystemExit(f"Invalid source manifest line {lineno}: {raw!r}") from exc
        rel = rel.strip().replace("\\", "/")
        if len(digest) != 64 or any(ch not in "0123456789abcdefABCDEF" for ch in digest):
            raise SystemExit(f"Invalid SHA-256 in source manifest line {lineno}: {digest!r}")
        if rel.startswith("/") or ".." in Path(rel).parts:
            raise SystemExit(f"Unsafe path in source manifest line {lineno}: {rel!r}")
        if rel in expected:
            raise SystemExit(f"Duplicate path in source manifest: {rel}")
        expected[rel] = digest.lower()
    if not expected:
        raise SystemExit("RotorDin source manifest is empty")
    return expected


def _tree_digest(root: Path, relative_paths: list[str]) -> str:
    digest = hashlib.sha256()
    for rel in sorted(relative_paths):
        data = (root / rel).read_bytes()
        digest.update(rel.encode("utf-8"))
        digest.update(b"\0")
        digest.update(data)
        digest.update(b"\0")
    return digest.hexdigest()


def verify_source() -> str:
    root = source_dir()
    if not root.is_dir():
        raise SystemExit(
            f"RotorDin direct Fortran source tree not found: {root}. "
            "The repository must version solver/src/source directly."
        )

    expected = _read_manifest()
    actual = {
        path.relative_to(root).as_posix()
        for path in root.rglob("*")
        if path.is_file()
    }
    missing = sorted(set(expected) - actual)
    extra = sorted(actual - set(expected))
    if missing or extra:
        raise SystemExit(
            "RotorDin direct source tree differs from SOURCE_MANIFEST.sha256; "
            f"missing={missing}, extra={extra}"
        )

    mismatched: list[str] = []
    for rel, wanted in expected.items():
        got = hashlib.sha256((root / rel).read_bytes()).hexdigest()
        if got != wanted:
            mismatched.append(f"{rel}: expected {wanted}, got {got}")
    if mismatched:
        raise SystemExit("RotorDin source SHA-256 mismatch:\n" + "\n".join(mismatched))

    missing_build_sources = [rel for rel in SOURCES if rel not in expected]
    if missing_build_sources:
        raise SystemExit(
            "Build source list is not covered by SOURCE_MANIFEST.sha256: "
            + ", ".join(missing_build_sources)
        )

    tree_sha = _tree_digest(root, list(expected))
    # SOURCE_MANIFEST.sha256 already verifies the exact file set and every
    # file digest. The aggregate tree digest is retained as reproducibility
    # metadata rather than as a second manually synchronized source of truth.
    if SOURCE_TREE_SHA256 is not None and tree_sha != SOURCE_TREE_SHA256:
        raise SystemExit(
            "RotorDin source-tree fingerprint mismatch: "
            f"expected {SOURCE_TREE_SHA256}, got {tree_sha}"
        )
    return tree_sha


def executable_name(*, debug: bool = False) -> str:
    stem = "rotordin-debug" if debug else "rotordin"
    return f"{stem}.exe" if os.name == "nt" else stem


def _windows_compiler_has_openblas(fc: str) -> bool:
    """Return True when the compiler belongs to a Windows toolchain with OpenBLAS."""
    if os.name != "nt":
        return True
    bindir = Path(fc).resolve().parent
    libdir = bindir.parent / "lib"
    return (
        any(bindir.glob("libopenblas*.dll"))
        or any(libdir.glob("libopenblas*.a"))
        or any(libdir.glob("libopenblas*.dll.a"))
    )


def compiler() -> str:
    explicit = os.environ.get("FC")
    if explicit:
        return explicit

    if os.name == "nt":
        candidates: list[Path] = []
        ucrt_bin = os.environ.get("MSYS2_UCRT_BIN", "").strip()
        if ucrt_bin:
            candidates.append(Path(ucrt_bin) / "gfortran.exe")
        candidates.extend(
            [
                Path(r"C:\\msys64\\ucrt64\\bin\\gfortran.exe"),
                Path(r"C:\\msys64\\mingw64\\bin\\gfortran.exe"),
            ]
        )
        found = shutil.which("gfortran")
        if found:
            candidates.append(Path(found))
        seen: set[str] = set()
        for candidate in candidates:
            key = str(candidate).casefold()
            if key in seen:
                continue
            seen.add(key)
            if candidate.is_file() and _windows_compiler_has_openblas(str(candidate)):
                return str(candidate)
        if found:
            raise SystemExit(
                "gfortran foi encontrado, mas a toolchain selecionada não possui OpenBLAS. "
                "Use o MSYS2/UCRT64 com mingw-w64-ucrt-x86_64-openblas ou defina FC "
                "explicitamente para C:\\msys64\\ucrt64\\bin\\gfortran.exe."
            )
    else:
        found = shutil.which("gfortran")
        if found:
            return found

    raise SystemExit("gfortran not found. Install GNU Fortran or set FC to the compiler executable.")


DEBUG_KERNEL_OPTIMIZED_SOURCES = {"lapack/blas.f"}


def _compile_one(
    fc: str,
    src_root: Path,
    obj_root: Path,
    rel: str,
    *,
    debug: bool = False,
) -> Path:
    src = src_root / rel
    obj = obj_root / Path(rel).with_suffix(".o")
    obj.parent.mkdir(parents=True, exist_ok=True)
    if debug:
        # Keep every GNU runtime/debug check enabled for all sources.  The
        # vendored reference BLAS is a numerical kernel (not RotorDin domain
        # logic) and at -O0 it interposes DGEMM/DTRSM from optimized BLAS,
        # making the checked Schur continuation impractically slow on Linux.
        # Compile only that kernel at -O2; no check, trap, tolerance or solver
        # equation is disabled or changed.
        debug_optimization = "-O2" if rel in DEBUG_KERNEL_OPTIMIZED_SOURCES else "-O0"
        flags = [
            "-c",
            debug_optimization,
            "-g",
            "-cpp",
            "-fcheck=all",
            "-fbacktrace",
            "-ffpe-trap=invalid,zero,overflow",
            "-Wall",
            "-Wextra",
            "-Wimplicit-interface",
            "-Wconversion",
            "-Wsurprising",
        ]
    else:
        flags = ["-c", "-O3", "-cpp", "-ffpe-summary=none"]
    if rel.endswith(".f90"):
        flags.append("-std=f2018")
    if os.name != "nt":
        flags.append("-D_LINUX")
    if rel in {"lapack/blas.f", "lapack/lapack.f"}:
        flags.append("-funroll-loops")
    cmd = [
        fc,
        *flags,
        "-I",
        str(src_root),
        "-I",
        str(obj_root),
        "-J",
        str(obj_root),
        "-I",
        str(src_root / "lapack"),
        "-o",
        str(obj),
        str(src),
    ]
    subprocess.run(cmd, check=True)
    return obj


def _copy_windows_runtime(fc: str) -> None:
    runtime = solver_dir() / "runtime"
    shutil.rmtree(runtime, ignore_errors=True)
    runtime.mkdir(parents=True, exist_ok=True)

    compiler_dir = Path(fc).resolve().parent
    patterns = (
        "libopenblas*.dll",
        "libwinpthread-1.dll",
        "libgcc_s_seh-1.dll",
        "libgfortran-*.dll",
        "libquadmath-*.dll",
        "libgomp-1.dll",
    )
    copied: set[str] = set()
    for pattern in patterns:
        candidates = list(compiler_dir.glob(pattern))
        found_on_path = shutil.which(pattern) if "*" not in pattern else None
        if found_on_path:
            candidates.append(Path(found_on_path))
        for candidate in candidates:
            if candidate.is_file() and candidate.name.lower() not in copied:
                shutil.copy2(candidate, runtime / candidate.name)
                copied.add(candidate.name.lower())

    if not any(name.startswith("libopenblas") for name in copied):
        raise SystemExit(
            f"OpenBLAS runtime DLL not found next to Windows compiler: {compiler_dir}"
        )


def _link(fc: str, objects: list[Path], output: Path) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    env_libs = os.environ.get("ROTORDIN_LIBS", "").strip()
    if env_libs:
        libs = env_libs.split()
    elif os.name == "nt":
        libs = ["-lopenblas", "-static-libgcc", "-static-libgfortran"]
    else:
        libs = ["-llapack", "-lblas"]

    cmd = [fc, "-o", str(output), *(str(path) for path in objects), *libs]
    subprocess.run(cmd, check=True)
    if os.name != "nt":
        output.chmod(output.stat().st_mode | 0o111)
    else:
        _copy_windows_runtime(fc)


def build(clean: bool = False, *, debug: bool = False) -> Path:
    tree_sha = verify_source()
    base = solver_dir()
    src_root = source_dir()
    obj_root = base / "build" / ("obj-debug" if debug else "obj")
    output = base / executable_name(debug=debug)

    if clean:
        shutil.rmtree(base / "build", ignore_errors=True)
        shutil.rmtree(base / "runtime", ignore_errors=True)
        for candidate in (
            base / executable_name(debug=False),
            base / executable_name(debug=True),
        ):
            if candidate.exists():
                candidate.unlink()

    fc = compiler()
    print(f"[RotorDin] compiler: {fc}")
    print(f"[RotorDin] source:   {src_root}")
    print(f"[RotorDin] mode:     {'debug-checks' if debug else 'release'}")
    print(f"[RotorDin] files:    {len(_read_manifest())}")
    print(f"[RotorDin] tree sha: {tree_sha}")
    print(f"[RotorDin] output:   {output}")

    objects = [
        _compile_one(fc, src_root, obj_root, rel, debug=debug)
        for rel in SOURCES
    ]
    _link(fc, objects, output)
    print(f"[RotorDin] built: {output}")
    return output


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Build the directly versioned RotorDin Fortran solver."
    )
    parser.add_argument("--clean", action="store_true")
    parser.add_argument(
        "--debug",
        action="store_true",
        help=(
            "Build a separate rotordin-debug executable with GNU Fortran "
            "runtime, bounds, backtrace and diagnostic checks."
        ),
    )
    parser.add_argument(
        "--verify-source",
        action="store_true",
        help="Verify solver/src/source against SOURCE_MANIFEST.sha256 and exit.",
    )
    args = parser.parse_args()

    if args.verify_source:
        tree_sha = verify_source()
        print(f"[RotorDin] direct Fortran source verified: {source_dir()}")
        print(f"[RotorDin] files: {len(_read_manifest())}")
        print(f"[RotorDin] tree sha256: {tree_sha}")
        return 0

    build(clean=args.clean, debug=args.debug)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
