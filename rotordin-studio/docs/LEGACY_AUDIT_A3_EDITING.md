# A3 — Atomic React editing vs. legacy mutation behavior

Audited source: `frmMain.AltSgmnt`, `frmMain.IncSgmnt`, `frmMain.sobrepoecarga`, `frmMain.ultrapassaescalonamento`.

## Legacy defect being removed

`AltSgmnt` clears columns DPCT/A/B/C/NR_COST/D_INT/D_F **before** it completes hollow/ribbed compatibility checks.

That means an edit rejected by a later `MsgBox` can already have mutated the MSFlexGrid row.

The React implementation must not reproduce this.

## Target behavior

All model edits are:

1. applied to a candidate immutable project;
2. validated;
3. committed only when valid.

The original project remains untouched when validation fails.

Implemented in:
- `src/domain/edit.ts`
- `src/state/ProjectContext.tsx`
- `src/domain/edit.test.ts`

## First UI integration

The Shaft Modeler uses the project context rather than treating text fields or table cells as authoritative engineering state.

The next editing increments should apply the same transaction pattern to:
- bearing coefficient edits;
- load/mass insertion and overlap validation;
- rotor-stack selection;
- support linkage.


## A9 update

The user-supplied current irDin production case shows geometry that is simultaneously ribbed and hollow. Therefore the old IBIEGE hollow-vs-ribbed prohibition is no longer a global RotorDin Studio domain rule.

Current rule:
- native/current RotorDin domain: combined ribbed + hollow geometry is allowed when physically valid;
- IBIEGE/BIEGE14 compatibility profile: the old prohibition remains enforced because BIEGE14 selects ST or HL globally and cannot preserve both.

See `LEGACY_AUDIT_A9_IRDIN_IMPORT.md`.
