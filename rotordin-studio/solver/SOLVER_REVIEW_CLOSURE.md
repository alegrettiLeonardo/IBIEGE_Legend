# RotorDin solver review closure

Date: 2026-09-19

## Declared scope

This closure record is intentionally limited to the native RotorDin solver and
its engineering input boundary. It excludes BearingPerf/ROSS feature work,
presentation/layout changes, and unrelated GUI functionality.

The reviewed implementation consists of three consolidated blocks:

1. complete Timoshenko + Cowper lateral shaft element and solver hardening;
2. native engineering-unit contract at the Python -> Fortran boundary;
3. legacy iRdin/VB6 physical parity for unbalance and angular fields.

## Shaft and hardening

The active lateral shaft formulation is the complete Timoshenko/Cowper element.
The former active hybrid routines are removed. The following audit findings are
closed in code:

- RD-001: fixed/variable-bearing unbalance phasor convention aligned;
- RD-002: `mkb` is assembled from mechanical shaft stiffness only; UMP is
  subtracted only in `mk1`;
- RD-003: UMP uses a standalone Hermite distributed-stiffness matrix and has no
  density/mass/inertia term;
- RD-004: UMP mapped by authoritative physical source section;
- RD-005: oscillatory eigenvalue selection hardened against zero roots,
  non-adjacent conjugates and invalid indices;
- RD-006: foundation frequency check no longer relies on Fortran short-circuit;
- RD-007: UNBFD has one RPM-increment contract;
- RD-013: colocated bearing K/C contributions accumulate instead of overwrite;
- RD-014: `cadjf` handles right-justified fixed-width angle flags without
  reversed/blank substrings, so `D` reaches the response routines reliably.

Detailed numerical evidence remains in `TIMOSHENKO_COWPER_VALIDATION.md`.

## Engineering-unit boundary

The solver boundary is defined as follows:

| quantity | project / legacy side | native RotorDin side |
|---|---:|---:|
| axial geometry | mm | m |
| bearing/support K | N/m | N/m |
| bearing/support C | N·s/m | N·s/m |
| mass | kg | kg |
| concentrated inertia | kg·m² | kg·m² |
| legacy unbalance KIND=0 | g·mm | kg·m |
| UMP | raw legacy number, provenance retained | k' [N/m²] |
| excitation/response angles | degrees | degrees, protocol flag D |

The unbalance conversion is exactly `g·mm * 1e-6 -> kg·m` and is applied only
to the non-mutating copy prepared for solver execution/input preview.

Legacy UMP receives no inferred producer-side scale factor. The native finite
element contract is distributed electromagnetic stiffness `k' [N/m²]`.
`DistributedMass.ump_value` and `SolverSection.ump_n_m` remain compatibility
storage names, while both classes expose `ump_stiffness_n_m2` directly.

The UMP element matrix uses `fac = ump * L / 420`; with `ump [N/m²]`,
the translational stiffness scale is `N/m`, dimensionally consistent with the
assembled stiffness matrix.

## Provenance and fail-closed behavior

`load_legacy_project()` records unresolved `ump_crg` source provenance even
when no GUI is used. UMP enabled values must be finite and non-negative.

The textual writer emits degree flags for degree-named phase/orientation fields,
preventing stale project metadata from advertising radians for degree values.

## Regression gate

The solver release-hardening gate includes:

- `tests/test_solver_release_hardening.py`
- `tests/test_solver_unit_contract.py`
- `tests/test_legacy_physical_unit_parity.py`
- `tests/test_analysis_speed_contract.py`
- compiled `chklamf` edge cases;
- compiled `cadjf` left/right-justified angle-flag regression;
- Timoshenko/Cowper element and THIAGOK integration gates;
- release and checked-debug solver builds;
- Linux and Windows execution.

## Integration status

Implementation is closed for the declared solver-review scope.

Final merge to `main` remains fail-closed until the exact consolidated PR HEAD
receives real Linux and Windows GitHub Actions runners and both solver-hardening
jobs execute successfully. A job ending with `runner_id=0` and zero executed
steps is infrastructure evidence only.


## Direct frequency-response hardening

Independent validation of the consolidated solver found that the historical
fixed-bearing modal receptance path could become non-physical when UNBMD
truncated the state spectrum with an incomplete conjugate pair.  A reproduced
case at UNBMD=19 reported an approximately 85 m displacement where the direct
dynamic-stiffness solution was of nanometre order.

The production lateral frequency-response path now uses `resp_fv` for both
fixed and speed-dependent bearings.  For fixed bearings it calls
`resp_fv(...,fx=1,...)`, preserving fixed K/C while solving the physical
dynamic stiffness matrix directly.  This removes both the odd-modal-truncation
failure and the fixed-vs-variable 180-degree phase discrepancy from the active
solver path without changing the force convention.

The integration regression
`test_fixed_and_constant_table_bearings_match_complex_unbalance_response`
uses `response_modes=19` deliberately and now requires tight complex-response
agreement between a fixed bearing and an equivalent constant bearing table.

The shared UNBFD contract is also enforced in torsion: `tresp_f` now consumes
`dw_r` directly as the rpm increment, matching the lateral response paths.

Tests that previously failed before reaching their engineering assertions were
corrected to synchronize imported Campbell ranges through the canonical
analysis-speed contract, and two brittle/incorrect regression fixtures were
repaired without weakening the physical gates.

## Modal hardening follow-up: A-1F / A-1M / A-1T / RD-016

This follow-up is implemented on the dedicated modal-hardening branch and is
not declared closed by this document until the exact branch HEAD completes the
numeric and platform gates.

### A-1F — direct frequency response

The productive lateral frequency-response route is now structurally independent
of modal space.  `opt(3)` dispatches directly to `resp_fv`; it no longer enters
`espmod` merely because fixed bearing coefficients are in use.  A combined
modes + frequency-response request therefore performs both analyses, but
`resp_fv` still solves the physical dynamic stiffness directly.

### A-1M — modal conditioning

`adjeig` now evaluates the scale-independent indicator

`eta = |VL^H A VR| / (||VL||_2 ||A VR||_2)`.

LAPACK `ZGGEVX` returns VL and VR.  RotorDin stores the left vector after
unconjugating it, so the historical downstream product `PSI^T A FI` is
mathematically the same `VL^H A VR` product.  The centralized acceptance floor
is `modal_condition_min = 1e-10`.

A pair that is non-finite, has a zero norm, or has `eta` below the floor is
marked invalid and is not divided by `sqrt(psi^H A phi)`.  Requested consumers
then fail closed with the eigenpair index, requested state order, `eta`, the
minimum accepted value and `|lambda|`.  Campbell/MAC tracking skips invalid
vector candidates rather than normalizing them.

The threshold study includes explicit 1e-6, 1e-8, 1e-10 and 1e-12 candidates.
The nearly-defective regression has `eta ~= 1e-14`, so all four reject the
known bad mechanism, while the scale-invariant well-conditioned regression has
`eta = 1`.  Final acceptance of 1e-10 remains contingent on the real-solver
and benchmark gates below.

### A-1T — historical resp_t

`resp_t` keeps its historical synchronous modal basis; this is deliberate,
because its legacy transfer expression is written in that basis and changing
the basis would alter historical results without a separate derivation.
Its second normalization is now an independent numerical boundary: only the
requested modal columns are normalized, each column recomputes the same
dimensionless conditioning metric against the time-response matrix, and an
invalid requested pair terminates the analysis before division.

The dedicated regression exercises the 18/19-mode boundary.  Eighteen modes
must remain finite and physical.  Nineteen modes may either remain physical or
terminate with the explicit ill-conditioned-eigenpair diagnostic; silent
continuation, NaN/Inf and metre-scale response are forbidden.

### RD-016 — two distinct modal contracts

RotorDin now exposes the two previously conflated formulations explicitly:

- `espmod_speed`: natural modes at prescribed spin, solving
  `M qdd + (C + Omega G) qd + K q = 0`; `MDRPM` supplies `Omega`.
- `espmod_sync`: the historical synchronous formulation represented by
  `(M - iG) lambda^2 + C lambda + K = 0`.

The UI/solver `Modes / MDRPM` path uses `espmod_speed`.  Campbell continues
to assemble the same prescribed-spin equation at every sweep speed.  Historical
`resp_t` and modal export retain the synchronous route pending any future
separate contract change.

The integration gate compares modal-at-speed with Campbell at 0, 1000, 3600
and 6000 rpm using nearest-frequency matching rather than raw array index, so
a branch rank exchange does not create a false mismatch.

### Qualification gates for this follow-up

The branch adds:

- `tests/test_modal_biorthogonal_conditioning.py`;
- `tests/test_modal_rd016_integration.py`;
- the modal tests to `tools/run_solver_release_hardening.py`;
- the same release/debug modal gates to Linux and Windows GitHub Actions;
- debug-only `-ffpe-trap=invalid,zero,overflow`.

Final status is determined from the exact final SHA.  A missing GitHub Actions
runner is recorded as BLOCKED / NOT EXECUTED, never as solver PASS.

### Current-repository reproducibility inventory

The current repository contains the THIAGOK legacy input fixture
`tests/fixtures/legacy/OV-5341943410-EL-M18-IAIB-user.txt`, which is used by
the dedicated 18/19-mode `resp_t` regression.  The repository does not contain
a self-contained Friswell benchmark fixture nor an RM12/DyRoBeS solver
input/reference-output fixture.  The literature/external benchmark numbers
recorded in the earlier Timoshenko/Cowper validation note are therefore
historical evidence, not reproducible benchmark gates for this follow-up.

Accordingly, this follow-up must report Friswell and RM12/DyRoBeS as
`NOT REPRODUCIBLE` until the corresponding solver inputs and independent
reference outputs are versioned in the repository.

### Platform execution status

GitHub Actions attempts for this follow-up reached completed/failure state
without allocating any workflow steps on either Linux or Windows.  Per the
fail-closed platform rule, zero executed steps are infrastructure evidence only:
the platform gates remain `BLOCKED` / `NOT EXECUTED`, never solver failure or
solver PASS.  A later real run must execute the release/debug builds and numeric
gates on the exact final SHA before solver-scope qualification can be declared.

