# A8 — Electron boundary from legacy process/configuration audit

Legacy sources audited:
- `Module1.ExecCmd`
- `frmMain.inicializa_configuracoes`
- `frmMain.inicializa_conexao`
- `Publico.DoConn`
- `Publico.SetaObjConexao`
- `frmFlexa.LdVlsS`
- `frmFlexa.DoCl`
- `frmFlexa.Form_Unload`

## Why privileged operations move out of React

The VB6 form directly:
- reads INI paths;
- creates COM database objects;
- assembles connection strings including credentials;
- starts external executables;
- waits for processes;
- deletes files and wildcard directories.

The React renderer must not inherit those privileges.

The Electron window is configured with:
- `contextIsolation=true`;
- `nodeIntegration=false`;
- `sandbox=true`;
- a minimal typed preload API.

Only these operations are currently exposed:
- save a project;
- open a project;
- prepare an isolated run workspace.

No generic filesystem, shell or process API is exposed.

## Save/open

Legacy save is DB-coupled and has a P0 dirty-state defect elsewhere in the UI. The new desktop layer first supports an atomic local `.rdin.json` project envelope.

Writing uses:
1. temporary file in the destination directory;
2. complete JSON write;
3. atomic rename to target.

Live Oracle/ODBC compatibility remains behind `ProjectRepository`; it is not available to renderer code.

## Process execution audit

Legacy `ExecCmd` calls `CreateProcessA` and then `WaitForSingleObject`, but it does not:
- validate process creation result;
- capture stdout/stderr;
- read child exit code;
- safely model argument boundaries;
- cancel a timed-out process robustly.

A safe `runQualifiedProcess` service has been added with:
- `spawn(executable, args, {shell:false})`;
- stdout/stderr capture;
- exit code/signal;
- timeout termination;
- explicit qualification ID.

It is **not exposed in preload** until BIEGE/FLECHA solver Golden Masters are frozen.

## FDE MSS deletion hazard

Legacy `LdVlsS` ends with:

```vb
Kill(pthvpmss & "*.*")
Kill(pthspmss & "*.*")
```

That behavior is not implemented.

All future FDE files must reside inside the current isolated run workspace so cleanup can be scoped to the current run.

## Credentials

Legacy `DoConn` builds a connection string with username/password from INI and a reversible source-controlled cipher/key.

The Electron boundary intentionally exposes no credential value to the renderer. The future database implementation must use OS/vault-backed credentials.

## Implementation

- `electron/main.ts`
- `electron/preload.ts`
- `electron/contracts.ts`
- `electron/services/projectFiles.ts`
- `electron/services/runWorkspace.ts`
- `electron/services/processRunner.ts`
- `src/types/window.d.ts`

Status: desktop security boundary and local project/run infrastructure implemented; live solver/database execution remains gated.
