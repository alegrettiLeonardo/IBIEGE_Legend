# A12 — Native RotorDin results connected to React Results Workspace

## Scope

A12 replaces the demonstration curves and hard-coded result values in the React Results Workspace with data produced by the qualified RotorDin runner.

Qualified source authority remains:

- repository: `alegrettiLeonardo/frontend_rotordin`
- branch: `fix/solver-audit-647d600b`
- source HEAD: `c669978bdcc42c06507eac8fda8b6dc39e8bce97`
- vendored solver tree: `542cbbea29923090bb17562c4120a8d5646ffcba`

No engineering quantity is recomputed in React.

## Authoritative result contracts audited

The TypeScript result parser was ported from the audited current frontend result contract in:

`reference/frontend_rotordin/results.py`

Native files connected:

| Native output | React workspace |
|---|---|
| `campbell.out` | Campbell branches, excitation orders, critical-speed crossings, log decrement / Stability |
| `unblresp.out` | response amplitude, phase, automatic peaks / amplification factor |
| `selaline.out` | elastic line X/Z, bearing reactions, bending moment, shear and stress data |
| `ucspdmap.out` | critical-speed stiffness map |
| `timeresp.out` | orbit speed cases and section trajectories |
| `modeshp.out` | mode frequencies, whirl labels and projected X/Z mode shapes |

The React parser preserves the same report markers and table semantics as the Python authority.

## Parser qualification

`src/adapters/rotordinResultData.golden.test.ts` reads the frozen EST-ST41 native files directly.

Frozen checks include:

- Campbell: 9 natural-frequency branches, 3 excitation orders, 9 logarithmic-decrement branches and 23 crossings.
- Unbalance response: 4 amplitude series, 4 phase series, 402 native points and 10 solver-native peaks.
- Elastic line: 30 displacement stations plus native static reactions/internal loads.
- Critical-speed map: 37 stiffness points and 9 modal branches.
- Orbit: one 1800 rpm speed case, 30 native sections and 50 points per orbit.
- Modes: 10 modes at 1800 rpm and 30 axial stations.

The response test intentionally freezes the native final speed reported by the solver (`2999.9944 rpm`) instead of rounding it to the requested nominal endpoint.

## Runtime result artifact

After every successful qualified native job, Electron now:

1. extracts marked sections from stdout;
2. parses the native result using the audited TypeScript parser;
3. writes:
   `03_Results/<native-job>/parsed_results.json`;
4. hashes the parsed artifact;
5. records its relative path and SHA-256 in the job entry of `run_manifest.json`;
6. returns the same typed parsed payload over IPC.

The manifest therefore remains the authority for associating a rendered result with:
- exact run ID;
- solver executable hash;
- input hash;
- qualification ID;
- native flags;
- native output sections;
- parsed-result artifact hash.

## Results Workspace behavior

The previous demonstration:
- synthetic Campbell curves;
- fake critical-speed table;
- illustrative unbalance curves;
- illustrative orbit;

is no longer used by `ResultsPage`.

The new `features/results/ResultsWorkspace.tsx` renders only the selected persisted qualified run.

Available tabs are derived from actual parsed native sections:

- Campbell
- Stability / Log Dec
- Unbalance Response
- Modes
- Elastic Line
- Critical Speed Map
- Orbit

If an analysis is absent from the selected run, its tab is absent.

## Presentation units

Source units remain authoritative.

Presentation-only conversions are explicit:

- unbalance amplitude: native meters → µm for plotting/table display;
- elastic-line displacement: native meters → µm;
- mode-shape projected displacement: native meters → µm;
- map stiffness is plotted against `log10(K [N/m])` only as an axis transform; the stored data remains N/m;
- orbit uses the native coordinates reported by `timeresp.out`.

No conversion modifies persisted native results.

## Persisted run selection

A12 also adds a typed run-history boundary.

Electron scans only:

`<userData>/runs/<run-id>/run_manifest.json`

A run is selectable only when:
- solver kind is RotorDin;
- a job has a persisted `parsedResultPath`;
- the manifest is valid.

Loading a run:
- validates the run identifier;
- prevents parsed-result paths from escaping the run root;
- reloads the parsed JSON artifacts;
- reconstructs the typed `ExecuteQualifiedRunResponse`.

The Results Workspace exposes a selected-run dropdown and automatically selects the newest persisted qualified run at desktop startup.

## Security boundary

Renderer access remains limited to typed preload APIs.

There is still no generic renderer access to:
- filesystem;
- child_process;
- shell;
- arbitrary paths.

Result loading occurs in Electron Main from paths already recorded inside the qualified run manifest.

## Main implementation files

- `src/adapters/rotordinResultData.ts`
- `src/adapters/rotordinResultData.golden.test.ts`
- `src/features/results/ResultsWorkspace.tsx`
- `src/state/ProjectContext.tsx`
- `src/runs/model.ts`
- `electron/contracts.ts`
- `electron/services/qualifiedRotorDinRun.ts`
- `electron/services/runHistory.ts`
- `electron/main.ts`
- `electron/preload.ts`

## A12 gate

A12 is considered closed when:

1. TypeScript/Electron build passes;
2. native-result parser Golden tests pass;
3. A10 Modes Golden passes;
4. A11 Campbell/Response/Line/Map/Orbit Linux tests pass;
5. A11 Windows cross-platform qualification passes;
6. Results Workspace contains no hard-coded engineering curves in the active rendering path.

