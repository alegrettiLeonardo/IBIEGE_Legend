# A1 — Legacy audit → TypeScript domain model

Baseline audited: `main@40babc4d63bebfbd876bae744e44541188ca4a7f`.

This document records the legacy evidence used for the A1 domain implementation. The React model does not invent replacements silently: where the target model extends the legacy behavior, that extension is called out explicitly.

## Shaft grid

Source: `ibiege/frmMain.frm`, `CnfGrdDimensoes`.

Legacy columns:

| Column | Legacy name | TypeScript meaning |
|---|---|---|
| 0 | L | segment length [mm] |
| 1 | D | outer/start diameter [mm] |
| 2 | DPCT | ribbed package diameter [mm] |
| 3 | A | rib thickness used as `esp_cost` by `exporta_biege` |
| 4 | B | hammer/flange thickness used as `esp_mart` |
| 5 | C | hammer/flange width used as `larg_mart` |
| 6 | NR_COST | number of ribs |
| 7 | D_INT | hollow inner diameter [mm] |
| 8 | D_F | taper final diameter [mm] |

### Important modeling decision

The legacy UI chooses Redondo / Costelado / Oco with the segment type combo, but conicity is an independent checkbox. Therefore the TypeScript domain **does not** model taper as a mutually exclusive union variant. It stores `endOuterDiameterMm` independently from `sectionType`.

This preserves combinations actually representable by the VB6 code.

## Hollow vs ribbed rule

Sources: `frmMain.IncSgmnt` and `frmMain.AltSgmnt`.

The legacy code explicitly rejects:
- adding a ribbed segment when any hollow segment exists;
- adding a hollow segment when any ribbed segment exists.

The rule is implemented as `LEGACY-SHAFT-002`.

The React validator validates before committing changes. It intentionally does **not** preserve the VB6 defect where `AltSgmnt` clears columns before completing incompatibility validation.

## Loads / distributed mass grid

Source: `frmMain.CnfGrdCargas`.

Columns:
- Xi
- LC
- KG
- Empuxo

`exporta_biege` interprets:
- `LC = 0`: concentrated load/mass;
- `LC > 0`: distributed region;
- Empuxo nonzero: magnetic thrust flag.

The TypeScript model keeps these semantics in `LegacyMassLoad`.

## Load containment

Source: `frmMain.ultrapassaescalonamento`.

The legacy UI requires the full interval `Xi .. Xi+LC` to fit in a single shaft segment. The TypeScript validator keeps this rule for legacy compatibility.

## Load overlap

Source: `frmMain.sobrepoecarga`.

The VB6 code checks only whether either endpoint of the new interval is inside an existing interval. It misses the case where a new load completely contains an existing load.

The React implementation intentionally corrects this defect using full interval intersection and records the source in the validation issue.

## Rotor stack

Sources: `frmMain.grdCargas_DblClick / DfPcRt` and `frmFlexa.LdPc`.

The legacy FLECHA workflow does not persist rotor-stack identity in `d_carbiege`. It recovers the selected row through `MSFlexGrid.CellBackColor = BkgPck`.

The React domain adds explicit `isRotorStack: boolean`.

This is a deliberate persistence correction, not a claim that the legacy database stored this value.

## Bearings

Sources: `frmMain.Salva`, `salva_mancal`, and `exporta_biege`.

The IBIEGE source persists two values per bearing:
- axial position;
- one scalar support stiffness.

It does **not** persist a full K/C matrix in this repository.

Therefore the TypeScript bearing model uses a discriminated union:

- `legacy-scalar`: exact IBIEGE representation;
- `matrix`: target RotorDin Studio representation required by the supplied current RotorDin UI.

The React bearing view projects a scalar legacy stiffness to a diagonal K matrix only for display. The exact scalar is retained so that a legacy serializer can reproduce the original input.

This also eliminates the legacy reload defect where custom stiffness was truncated with `Left(..., 1)`.

## FLECHA assumptions observed

Source: `frmFlexa.WrFl`.

The current legacy writer assumes:
- exactly 2 bearings;
- steel density = 7850;
- E = 207 GPa;
- Poisson ratio = 0.3;
- maximum section count read from INI (`NRESC`);
- zero X/Y/Z acceleration;
- up to one Y and one Z concentrated force from the form fields.

These assumptions are **not** generalized in A1. They will be represented explicitly when the FLECHA adapter is implemented and tested against Golden Masters.

## A1 files

- `src/domain/model.ts`
- `src/domain/legacy.ts`
- `src/domain/validation.ts`
- `src/data/demoProject.ts`
- unit tests in `src/domain/*.test.ts`

## Next audit gate

Before implementing the BIEGE serializer:
1. characterize VB6 numeric formatting from `exporta_biege`;
2. freeze at least one known `temp.txt` golden input;
3. verify bearing order and the meaning of zero/infinite support;
4. characterize `NKRIT` output parsing;
5. do not connect the React runner until stale-output and per-run workspace protections exist.


## A9 update

The user-supplied current irDin production case shows geometry that is simultaneously ribbed and hollow. Therefore the old IBIEGE hollow-vs-ribbed prohibition is no longer a global RotorDin Studio domain rule.

Current rule:
- native/current RotorDin domain: combined ribbed + hollow geometry is allowed when physically valid;
- IBIEGE/BIEGE14 compatibility profile: the old prohibition remains enforced because BIEGE14 selects ST or HL globally and cannot preserve both.

See `LEGACY_AUDIT_A9_IRDIN_IMPORT.md`.
