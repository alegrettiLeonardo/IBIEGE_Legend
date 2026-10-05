# A11 — EST-ST41 per-analysis RotorDin qualification

## Authority

- source repository: `alegrettiLeonardo/frontend_rotordin`
- source branch: `fix/solver-audit-647d600b`
- source HEAD: `c669978bdcc42c06507eac8fda8b6dc39e8bce97`
- vendored Fortran solver tree: `542cbbea29923090bb17562c4120a8d5646ffcba`
- production case: `EST-ST41_1000_B3_60HZ_1675_63536`

A11 extends A10 Modes qualification to the remaining lateral analyses exposed by the React UI.

## Linux real-solver closure

The audited Fortran solver has completed all five A11 analyses with return code 0 on Linux/GNU Fortran.

### Campbell + Stability / Log Decrement

Native job:

```text
rotordin -std -c -b
```

The source calculation requests 0–3000 rpm, but both bearing TABLEs begin at 500 rpm.
The qualified Campbell domain is therefore explicitly:

```text
500–3000 rpm
```

No TABLE extrapolation below 500 rpm is permitted.

The native result contains:
- 9 natural-frequency branches;
- 9 logarithmic-decrement branches;
- 23 reported excitation-order crossings.

Campbell and the UI Stability / Log Decrement card share the same qualified native `-c` job.

### Direct unbalance response

Native job:

```text
rotordin -std -f -b
```

Legacy response-grid semantics are preserved:

```text
raw range            = 300–3000 rpm
raw d_div            = 400
common TABLE range   = 500–4000 rpm
effective range      = 500–3000 rpm
UNBFD                 = 6.234413965087282 rpm
native point count   = 402
response positions   = 0.55, 0.55, 3.977, 3.977 m
```

The frozen candidate contains four amplitude series, four phase series and ten solver-native automatic peaks.

### Static elastic line

Native job:

```text
rotordin -std -s -b
```

The irDin source has no explicit RKS field. The current audited frontend therefore preserves the historical/default:

```text
RKS = 0
```

This invokes the native fixed-stiffness static-bearing path; the qualification does not invent a TABLE evaluation speed.

The Linux candidate contains 30 displacement stations.
For this case the largest resultant displacement is approximately:

```text
1.597994595e-4 m at y = 2.347983 m
```

### Critical-speed stiffness map

Native job:

```text
rotordin -std -k -b
```

With variable TABLE bearings and no explicit stored map speed, the audited writer resolves:

```text
SMSPD = nominal speed = 1800 rpm
```

which lies inside the common bearing TABLE domain.

The candidate contains:
- 37 stiffness points;
- 9 critical-speed branches;
- stiffness range 1e7–1e11 N/m;
- rated speed metadata = 1800 rpm.

### Orbit / historical time-harmonic response

Native job:

```text
rotordin -std -t -b
```

The legacy project has no positive `t_rpm`, so the audited speed contract resolves the single-speed case to nominal speed:

```text
TIMSPD = 1800 rpm
TIMOEP = 0   (all native sections)
```

The candidate contains:
- one speed case;
- 50 orbit points per section;
- 30 native orbit sections.

This is the historical RotorDin time-harmonic/orbit path, not the newer physical-space transient solver.

## Native TypeScript serialization gate

The new TypeScript serializer:

`src/adapters/rotordinNative.ts`

is gated byte-for-byte against the already frozen A10 EST-ST41 Modes `input.txt`.

This validates the React/Electron boundary for:
- equivalent ribbed/hollow shaft sections;
- GYCOF;
- package-to-DISK expansion;
- TABLE bearing payloads;
- response-grid translation;
- probes;
- supports;
- fixed-width F10/I10 records.

No Fortran equations are reproduced in TypeScript.

## Desktop execution policy

The Electron runner exists, but each analysis is controlled by
`src/runs/qualification.ts`.

An analysis whose registry status is `pending` is rejected before process launch with `RUN-QUAL-002`.

The runner:
- resolves only the vendored/explicit RotorDin executable;
- serializes stdin through the audited TypeScript adapter;
- invokes one isolated solver process per qualified native analysis;
- captures stdout/stderr and exit code;
- parses only native marked sections;
- writes per-run artifacts and hashes to `run_manifest.json`;
- never consumes a pre-existing solver output file.

## Windows parity gate — CLOSED

The initial Windows run identified a checkout-byte issue: Git converted one audited Fortran source file at checkout, causing `SOURCE_MANIFEST.sha256` verification to fail before compilation.

The repository carries the source authority's byte-preservation rule:

```gitattributes
rotordin-studio/solver/src/source/** -text
rotordin-studio/solver/SOURCE_MANIFEST.sha256 text eol=lf
```

After that correction, the Windows UCRT64 build passed source-manifest verification, rebuilt the audited solver, executed Campbell, response, elastic line, map and orbit, and passed the cross-platform engineering-result comparison against the Linux candidates.

Qualification workflow evidence:
- run 37317869479: SUCCESS
- build-and-test: PASS
- analysis-contract-linux: PASS
- solver-contract-freeze: PASS
- analysis-contract-windows: PASS
- persist-analysis-goldens: PASS

The registry is therefore promoted to `qualified` for Modes, Campbell/Stability, Unbalance Response, Elastic Line, Critical Speed Map and Orbit.


## Final A11 execution matrix

| React analysis | Native job | Qualified flags | Golden | Linux | Windows | Desktop launch |
|---|---|---|---|:---:|:---:|:---:|
| Modes | modes | `-std -m -b` | `modes-v1` | PASS | PASS | ENABLED |
| Campbell | campbell | `-std -c -b` | `campbell-v1` | PASS | PASS | ENABLED |
| Stability / Log Decrement | campbell | `-std -c -b` | `campbell-v1` | PASS | PASS | ENABLED |
| Unbalance Response | response | `-std -f -b` | `response-v1` | PASS | PASS | ENABLED |
| Elastic Line | line | `-std -s -b` | `line-v1` | PASS | PASS | ENABLED |
| Critical Speed Map | map | `-std -k -b` | `map-v1` | PASS | PASS | ENABLED |
| Orbit | orbit | `-std -t -b` | `orbit-v1` | PASS | PASS | ENABLED |

Campbell and Stability intentionally deduplicate to one native `-c` process when both are selected.

## Desktop runner release rule

The renderer still has no generic `child_process` or shell capability.

The only production execution path is:

```text
React Analysis Plan
  → ProjectContext.executeQualifiedRun()
  → typed preload IPC
  → Electron executeQualifiedRotorDinRun()
  → nativeJobsForQualifiedAnalyses()
  → runQualifiedProcess()
  → vendored RotorDin executable
```

`nativeJobsForQualifiedAnalyses()` is the mandatory gate. Any future analysis added with status `pending` is rejected before process creation with `RUN-QUAL-002`.

A11 status: **CLOSED / QUALIFIED / DESKTOP RUNNER ENABLED FOR QUALIFIED ANALYSES ONLY**.


## Cross-platform fixed-width input policy

The solver input contract is exact at the **line-content and fixed-column level**. Linux and Windows may persist the final text artifact with different EOL/terminal-newline bytes even when every Fortran input record is identical.

The A11 verifier therefore:

- normalizes only line terminators to LF and exactly one final LF before comparing the input contract across platforms;
- still requires every character within every input record to be identical;
- still freezes the raw platform-specific file hash as provenance;
- never normalizes numeric fields, whitespace inside records, field widths, ordering, TABLE data or solver options.

This policy was added after a Windows candidate had the same `splitlines()` content as the Linux golden but a different raw SHA-256. The difference was textual record termination, not the Fortran fixed-width payload.
