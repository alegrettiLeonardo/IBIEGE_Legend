# A10 — Current RotorDin solver contract audit and EST-ST41 real-output freeze

## Status

**A10 scientific contract audit: implemented.**  
**A10 EST-ST41 Modes real output: frozen on Linux/GNU Fortran.**  
**Direct React `Run Selected → solver`: still intentionally gated.**

This gate uses the current audited solver authority requested for the migration:

- source repository: `alegrettiLeonardo/frontend_rotordin`
- source branch: `fix/solver-audit-647d600b`
- source HEAD: `c669978bdcc42c06507eac8fda8b6dc39e8bce97`
- solver subtree: `542cbbea29923090bb17562c4120a8d5646ffcba`

The exact `solver/` tree is vendored under `rotordin-studio/solver/`.
The frontend translation files required to reproduce the current input contract are vendored read-only under
`rotordin-studio/reference/frontend_rotordin/`.

No Fortran physics was rewritten as part of this React migration block.

---

## 1. Authoritative build contract

The vendored `solver/build_solver.py` is the authority used by CI.

CI executes:

```text
python solver/build_solver.py --verify-source
python solver/build_solver.py --clean
```

The first command verifies `solver/src/source` against `SOURCE_MANIFEST.sha256`.
The second produces the executable from the directly versioned Fortran source.

The source provenance is pinned in:

`solver/SOURCE_PROVENANCE.json`

The initial frozen run was built with:

```text
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
Linux x86_64
Python 3.12.14
```

This is a Linux scientific golden. Windows packaged parity remains a separate gate before desktop solver execution is promoted.

---

## 2. Fortran command-line contract

The audited `rotordin.f90` / `init.f90` command-line contract includes:

| flag | analysis |
|---|---|
| `-std` | stdin/stdout marked-section mode |
| `-c` | Campbell |
| `-m` | prescribed-spin modes |
| `-f` | direct frequency/unbalance response |
| `-t` | historical time/orbit response |
| `-s` | static elastic line |
| `-k` | critical-speed stiffness map |
| `-a` | angular map |
| `-b` | bearing-support coupling |
| `-j` | variable-bearing parameter output |

The current frontend deliberately runs one isolated solver process per selected analysis rather than combining incompatible options into one process.

For the first EST-ST41 frozen output the exact command is:

```text
rotordin -std -m -b
```

The analysis selection is **Modes only**, with bearing supports enabled.

---

## 3. Why Modes was selected for the first real golden

The source irDin calculation stores analysis parameters but does not expose an authoritative persisted on/off bitset for all analyses.

The EST-ST41 source also asks Campbell to start at 0 rpm while both embedded TABLE bearings start at 500 rpm.

The current frontend/Fortran contract does not fabricate a zero-speed TABLE law when the first explicit row is greater than zero.

Therefore the first immutable real-output golden uses:

- unchanged physical geometry;
- unchanged masses;
- unchanged bearing tables;
- unchanged supports;
- nominal speed = 1800 rpm;
- Modes analysis only.

1800 rpm lies inside the common bearing-table domain:

```text
500 <= 1800 <= 4000 rpm
```

This allows the real TABLE interpolation, gyroscopic terms, support matrices and prescribed-spin eigensystem to be exercised without altering the source calculation's physical model.

A Campbell golden requires a separately documented analysis-range decision (for example 500–3000 rpm) and is intentionally not hidden inside this Modes qualification.

---

## 4. Current frontend import/translation contract

The reference package `frontend_rotordin` installs its compatibility contracts in this order:

1. `legacy_package_contract`
2. `analysis_speed_contract`
3. `legacy_calculation_contract`
4. presentation-only analysis-speed i18n

The A10 freeze harness imports this exact package, so the same wrappers used by the audited frontend are active.

### Package translation

For the supplied case:

```text
p_div = 6
disk density = 7850 kg/m³
```

The logical rotor package is expanded into six rigid DISK records.

The two remaining logical masses remain one disk each.

Therefore the native input has:

```text
DISK count = 8
```

### Response-grid provenance

The historical `d_div` field is a division parameter, not a modern rpm increment.

For this case:

```text
raw response range = 300–3000 rpm
raw d_div          = 400
TABLE common range = 500–4000 rpm

effective range    = 500–3000 rpm
UNBFD step         = (3000-500)/(400+1)
                   = 6.234413965087282 rpm
number of points   = 402
```

This contract is frozen in `qualification.json`, even though the A10 analysis itself is Modes.

---

## 5. Native fixed-width input contract

The Fortran reader and current `SolverInputWriter` agree on the following primary blocks:

```text
NAME
DESCRIPTION
SECTIONS
DISK
BEARING
CPBIS/CPBFS/CPBSPD...
EXCT/UNBIF/UNBFF/UNBFD...
FRESPPOS
OPTIONS
CONCENT
KIND / BEAFILES
inline bearing TABLE/COEF blocks
SUPPORT
```

All dimensional solver records are serialized in SI units.

The exact EST-ST41 native fixed-width input is frozen as:

`golden/est-st41/modes-v1/input.txt`

Input SHA-256:

`c499c0b94baf79db1c76991b201a3905d2d3826c0116ecc70d33431367b866c2`

Future A10 CI requires this input to remain **byte-identical**.

---

## 6. Shaft translation authority

The current frontend does not send ribbed geometry directly as the old GUI columns.

For a ribbed section it derives an equivalent circular FE section using
`ribbed_equivalent_properties`:

- preserves real lateral second moment;
- keeps the physical inner diameter;
- derives equivalent outer diameter;
- derives equivalent density so mass/length is preserved;
- derives `GYCOF=Aeq/Areal` for the native gyroscopic contract.

This is important for EST-ST41 because the production project contains sections that are simultaneously ribbed and hollow.

The old BIEGE14 ST/HL global representation cannot preserve that model; the current RotorDin contract can.

The frozen native input contains 17 translated SECTIONS records.

---

## 7. Mass/disk translation authority

`solver_disks.py` and `mass_properties.py` are the current authority.

For each logical mass:

- package masses are expanded according to `p_div`;
- total mass, axial extent and centroid are preserved;
- disk bore priority is:
  1. explicit D.Int;
  2. package/ribbed DPCT;
  3. physical local shaft OD;
- the native writer sends explicit WEIGHT and DENSITY=0;
- final rigid-disk inertia remains Fortran-authoritative.

Frozen result:

```text
DISK records        = 8
disk mass           = 10680 kg
section/shaft mass  = 4935.4 kg
reported rotor mass = 15616 kg
rotor length        = 4.9544 m
center of mass      = 2.3071 m
```

---

## 8. Bearing TABLE contract — corrected and frozen

The Fortran `entrada.f90` + `rd_bearing_contract.f90` are the authority.

A TABLE row is:

```text
rpm,
Kxx, Kxz, Kzx, Kzz,
Cxx, Cxz, Czx, Czz
[, Kphi, Ktheta]
```

This is **not** the same order as the legacy GUI grid.

The React importer was corrected in A10 to follow the native TABLE order.

At 1800 rpm the real solver interpolated:

### Bearing 1

```text
Kxx =  2.3126122448979586E+08
Kxz = -4.3605714285714284E+07
Kzx = -7.5695510204081631E+08
Kzz =  2.0080204081632650E+09

Cxx =  6.5328571428571420E+05
Cxz = -1.4666326530612248E+06
Czx = -1.4666326530612248E+06
Czz =  8.4234693877551015E+06
```

### Bearing 2

```text
Kxx =  2.5421836734693879E+08
Kxz = -8.3526734693877563E+07
Kzx = -8.5429183673469388E+08
Kzz =  2.4276938775510201E+09

Cxx =  6.3702040816326544E+05
Cxz = -1.4884693877551020E+06
Czx = -1.4884693877551020E+06
Czz =  9.3026530612244885E+06
```

Signed cross-coupled terms are not symmetrized or clamped.

---

## 9. Response-coordinate contract

The Fortran `saidas.f90` documents:

```text
desp = 1 -> horizontal
desp = 2 -> vertical
```

The native lateral generalized displacement convention is `x,z`.

Therefore the React legacy-import contract is now authoritative as:

```text
1 -> X
2 -> Z
```

The original numeric source code is still retained for auditability.

---

## 10. Real EST-ST41 Modes result

The first real output was produced by the vendored Fortran solver with return code 0.

Native state:

```text
physical DOF = 124
state size   = 248
```

The `rdaudit.out` spectrum record is:

```text
status                    = ASYMPTOTICALLY_STABLE
maximum physical growth   = -7.3481164820371984E-03 1/s
total eigenpairs          = 248
finite                    = 248
invalid                   = 0
unstable                  = 0
nonoscillatory            = 4
```

### First 10 reported modes at 1800 rpm

| mode | f [Hz] | growth [1/s] | damping ratio | log decrement | whirl |
|---:|---:|---:|---:|---:|:---:|
| 1 | 21.4884740037 | -5.5743516849 | 0.0412514379 | 0.2594112399 | BW |
| 2 | 35.8258248438 | -8.6685072384 | 0.0384810100 | 0.2419625306 | MX |
| 3 | 39.5391437170 | -32.0717992013 | 0.1280345083 | 0.8111404595 | MX |
| 4 | 69.3717044145 | -32.0338708296 | 0.0732955277 | 0.4617714254 | MX |
| 5 | 71.9364453656 | -17.5256699984 | 0.0387453475 | 0.2436271338 | MX |
| 6 | 137.5337167313 | -55.7087666998 | 0.0643330166 | 0.4050553422 | MX |
| 7 | 138.1124278675 | -71.1726830026 | 0.0817419358 | 0.5153242478 | MX |
| 8 | 208.8730078117 | -57.2494236051 | 0.0435808916 | 0.2740872275 | MX |
| 9 | 212.6785430633 | -46.6915533971 | 0.0349196459 | 0.2195404987 | MX |
| 10 | 253.6012226794 | -30.5053372060 | 0.0191410186 | 0.1202886046 | MX |

---

## 11. Frozen artifacts

Persistent golden location:

`golden/est-st41/modes-v1/`

Contains:

- `qualification.json`
- exact native `input.txt`
- imported `project.json`
- `run.json`
- native bearing assets
- `sections/output.out`
- full `sections/modeshp.out`
- `sections/rdaudit.out`
- job stderr/progress evidence
- `scientific_summary.json`

The raw mode-shape output is frozen for provenance.

It is not used as a strict byte-equality scientific gate because eigenvector sign/phase is not a portable physical invariant across LAPACK/toolchain implementations.

---

## 12. Regression policy

`tools/verify_est_st41_golden.py` enforces:

### Exact
- native fixed-width `input.txt`;
- source repository/branch/HEAD/tree;
- mode IDs/eigenpair IDs;
- whirl labels;
- spectrum counts/status.

### Numeric tolerance
- interpolated bearing coefficients;
- modal frequencies;
- physical pole magnitude;
- growth rates;
- damping ratios;
- log decrement;
- maximum spectrum growth.

### Report normalization
Timestamped solver stamp lines are excluded from normalized `output.out` comparison.

This avoids accepting physical drift while also avoiding false failures from wall-clock stamps or nonphysical eigenvector sign choices.

---

## 13. CI gates now active

Every branch update performs:

1. React/TypeScript build;
2. Electron typecheck/bundle;
3. unit tests;
4. Fortran source-manifest verification;
5. Fortran build;
6. real EST-ST41 Modes execution;
7. scientific comparison against the frozen golden;
8. artifact upload.

The initial frozen raw output was persisted by CI itself and then made immutable by workflow policy.

---

## 14. Remaining gates before Run Selected may spawn RotorDin

The process runner remains intentionally unavailable to the React preload.

A10-Modes alone is not enough to authorize every analysis button.

Required next qualification increments:

1. **Windows solver parity** for A10-Modes using the packaged compiler/runtime.
2. **Campbell**:
   - source requests 0–3000 rpm;
   - TABLE begins at 500 rpm;
   - an explicit qualified range decision is required before execution.
3. **Unbalance/direct response**:
   - preserve `d_div=400` as historical division semantics;
   - qualified effective grid is 500–3000 rpm / 402 points for this case.
4. **Elastic line**.
5. **Critical-speed map**.
6. **Orbit/time response**.
7. Confirm result parsers and report-section names for each analysis.

Only after those gates should `runQualifiedProcess` be exposed through the Electron preload and connected to the React `Run Selected` action.

---

## 15. Conclusion

A10 establishes the first end-to-end scientific authority chain:

```text
EST-ST41 irDin source
        ↓
audited current frontend import/translation contract
        ↓
fixed-width native RotorDin input
        ↓
vendored audited Fortran source
        ↓
real compiled solver
        ↓
marked native outputs + RD_AUDIT_V1
        ↓
persistent scientific golden
        ↓
CI regression gate
```

The UI remains decoupled from direct executable launch until the remaining per-analysis and Windows gates are closed.
