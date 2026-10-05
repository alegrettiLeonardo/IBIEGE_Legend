# A5 — Legacy persistence audit

Sources audited:
- `DOCS/d_ibiege.sql`
- `Publico.RecGrd`
- `Publico.SlvGrd`
- `frmMain.Salva`
- `frmMain.busca_mancais`
- `frmMain.salva_mancal`
- `frmMain.abre_calculo`

## Schema

`d_idnBIEGE` stores:
- ID
- NR_REF
- DS_CALC
- NM_USU
- DT_DAT

`d_carBIEGE` stores EAV-like engineering characteristics:
- ID_D_IDNBIEGE
- DS_CARACT
- VL_CARACT
- VL_LINHA

Current SQL declares `VL_CARACT NUMBER(12,2)`, so the adapter emits a precision-loss warning when a value has more than two decimal places.

## Shaft and load rows

`SlvGrd` uses positive `VL_LINHA` values starting at 1.

Shaft names:
`L,D,DPCT,A,B,C,NR_COST,D_INT,D_F`

Load names:
`Xi,LC,KG,Empuxo`

The TypeScript adapter preserves this mapping.

## Bearings

`salva_mancal` stores:
- -1: mancal 1 position
- -2: mancal 1 constante
- -3: mancal 2 position
- -4: mancal 2 constante

The adapter reproduces this convention.

### Reload defect fixed

`abre_calculo` restores each stiffness using:

```vb
Left(PegaItem(...), 1)
```

This truncates arbitrary stiffness values.

The TypeScript decoder reads the complete numeric value and tests exact preservation.

## Rotor stack is not persisted

There is no `DS_CARACT` for the FLECHA rotor-stack selection. VB6 stores this state only as grid background color.

The TypeScript EAV adapter therefore:
- warns when encoding because a pure legacy DB save drops it;
- warns after decoding that explicit rotor-stack confirmation is required.

The future Electron repository may keep a sidecar/new-schema extension so the React project can persist this state without modifying legacy tables during the compatibility phase.

## Implementation

- `src/persistence/legacyEav.ts`
- `src/persistence/legacyEav.test.ts`

No live SQL is executed by the renderer. `ProjectRepository` is a port; the actual Oracle/ODBC implementation belongs in Electron Main.
