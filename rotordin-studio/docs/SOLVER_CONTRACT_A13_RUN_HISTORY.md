# A13 — Runs & Reports backed by real run_manifest.json history

## Objective

A13 removes the demonstration run rows and placeholder artifact cards from the React `Runs & Reports` screen.

The only history authority is now the per-run workspace written by the qualified Electron runner:

```text
<userData>/runs/<run-id>/
  run_manifest.json
  01_Input/
  02_Solvers/
  03_Results/
  04_Logs/
  05_Temp/
```

No fabricated run IDs, elapsed times, hashes, statuses or file names remain in the active Runs/Reports rendering path.

## History discovery

Electron scans direct child directories of `<userData>/runs` and reads `run_manifest.json`.

A row is exposed only when:
- the manifest parses;
- `schemaVersion === 1`;
- the solver kind is `rotordin`.

Unlike the Results selector, the Runs history intentionally includes successful, failed, warning, prepared and cancelled manifests. This preserves execution evidence even when no parsed result exists.

Each row reports:
- run ID;
- project reference;
- start / finish timestamps;
- status;
- selected qualified analyses;
- elapsed time derived from manifest timestamps;
- aggregate input SHA-256;
- solver executable SHA-256;
- artifact count;
- whether parsed native results exist.

## Run inspection

Selecting a row performs a typed Electron `run:inspect` call and returns the real manifest.

The detail workspace provides four tabs:

1. Artifacts
2. Logs
3. Hashes & Traceability
4. Compare Runs

## Artifact security model

The renderer never receives arbitrary filesystem access.

Artifact preview/reveal must pass:

```text
runId
relativePath
    ↓
Electron
    ↓
load run_manifest.json
    ↓
relativePath registered in manifest?
    ↓
resolved path still inside run root?
    ↓
read/reveal
```

`run_manifest.json` itself is the only allowed implicit artifact.

Text preview:
- accepts manifest-registered text artifacts;
- is capped at 2 MB;
- returns an explicit `truncated` flag;
- rejects binary data containing NUL bytes.

The UI can also reveal an artifact in the operating-system file browser or open the entire run workspace. No generic `shell.openPath` or filesystem API is exposed to React.

## Logs

The Logs tab is populated from manifest artifacts whose kind is `log` plus registered `stdout.out` / `stderr.log`.

The preview is the actual persisted file. The UI does not synthesize solver messages.

## Hashes and traceability

The traceability view renders directly from `RunManifest`:

- solver executable SHA-256;
- project SHA-256;
- aggregate input SHA-256;
- start / finish timestamps;
- qualification IDs by analysis;
- per-job native flags;
- per-job input hash;
- parsed-result hash;
- exit code;
- native section list;
- job status.

## Opening selected results

The **Open Results** action is enabled only when the selected manifest has persisted parsed native results.

The action:
1. loads the selected qualified run from disk;
2. makes it the `selectedRun` in React project context;
3. navigates to the A12 Results Workspace.

Failed/prepared runs remain inspectable but cannot masquerade as result-bearing runs.

## Run comparison

A13 adds `src/runs/comparison.ts`.

Comparison is performed on already persisted native parsed results; it does not invoke the solver and does not recompute rotor physics.

The comparison reports:

### Authority / identity
- solver hash SAME / DIFF;
- project hash SAME / DIFF;
- aggregate input hash SAME / DIFF;
- common analyses;
- analyses present in only one run.

### Engineering metrics where both runs contain the same result family
- modal frequency delta per mode;
- Campbell crossing count and maximum matched crossing shift by harmonic;
- minimum log decrement per branch;
- maximum unbalance amplitude per response point;
- native response peak count;
- maximum elastic-line resultant displacement;
- maximum branch shift on the critical-speed stiffness map;
- maximum orbit radius.

Presentation conversion to µm is explicit for displacement metrics.

The comparison table always labels the selected manifest as baseline and the second run as candidate.

## Typed IPC added

```text
run:inspect
run:read-artifact
run:reveal-artifact
run:open-workspace
```

Existing:
```text
run:list-qualified
run:load-qualified
```

The historical API name `listQualifiedRuns` remains for compatibility, but A13 now returns all valid RotorDin run manifests and marks each row with `hasResults`.

## Main implementation files

- `src/features/runs/RunsWorkspace.tsx`
- `src/runs/comparison.ts`
- `src/runs/comparison.test.ts`
- `src/state/ProjectContext.tsx`
- `electron/contracts.ts`
- `electron/main.ts`
- `electron/preload.ts`
- `electron/services/runHistory.ts`

## A13 qualification gate

A13 is closed only when:

1. TypeScript renderer builds;
2. Electron main/preload typecheck and bundle pass;
3. comparison unit tests pass;
4. A12 native-result Golden parser tests remain green;
5. A10 Modes Golden remains green;
6. A11 Linux qualified analyses remain green;
7. A11 Windows cross-platform qualified analyses remain green;
8. no demonstration `runs[]` data remains in the active Runs/Reports route.
