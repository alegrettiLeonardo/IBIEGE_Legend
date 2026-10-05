# A7 — Run workspace and stale-output protection

Legacy sources:
- `frmMain.inicializa_configuracoes`
- `frmMain.efetua_calculo`
- `frmFlexa.DoCl`
- `frmFlexa.VfFl`
- `frmFlexa.Form_Unload`
- `iBiege.ini`
- `iBiege3.ini`

## Legacy behavior

BIEGE and FLECHA use shared configured files under `C:\tmp`.

BIEGE input/output are deleted at application initialization, but `efetua_calculo` does not remove the previous BIEGE output immediately before every run. It only checks whether the output path exists after `ExecCmd`.

Therefore:
1. run A may create a valid `Biegedat.bga`;
2. run B may fail before replacing it;
3. the old file still exists;
4. VB6 can parse the old NKRIT as the result of run B.

FLECHA removes its result before launch, but still uses shared paths and later deletes shared artifacts in `Form_Unload`.

## Target rule

Every execution receives an isolated workspace:

```text
<run-id>/
  01_Input/
  02_Solvers/
  03_Results/
  04_Logs/
  05_Temp/
  run_manifest.json
```

No solver adapter may consume an output merely because a pathname exists.

At minimum the runner must verify:
- process launch succeeded;
- process exit status;
- expected output exists;
- output timestamp is fresh relative to the run;
- output is copied/created inside the current run workspace;
- hashes are recorded before parsing.

## Implementation

- `src/runs/model.ts`
- `src/runs/model.test.ts`

Filesystem/process execution still belongs to the future Electron Main layer. This module is intentionally runtime-neutral and testable.
