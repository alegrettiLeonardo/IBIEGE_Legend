# A6 — FLECHA adapter audit

Sources:
- `frmFlexa.Vld`
- `frmFlexa.GtAxDt`
- `frmFlexa.SgtCKg`
- `frmFlexa.AdDvSgt`
- `frmFlexa.AdSgt`
- `frmFlexa.WrFl`
- `frmFlexa.VfFl`
- `frmFlexa.SwRs`
- `frmFlexa.DoCl`
- `iBiege3.ini [FLECHA]`

## Section preparation

The legacy adapter builds `TpBur` sections with:
- D_E
- D_I
- length
- distributed mass
- air gap
- rib web/flange geometry
- rib count

A load belongs to a segment only when its full interval is inside that segment.

The configured split threshold is:

```text
PERC_LIM_DIV_SEGM=0.95
```

If any load inside a segment occupies less than 95% of that segment, the segment is subdivided around load boundaries.

The TypeScript adapter reproduces this behavior.

## Ribbed geometry

Legacy `GtAxDt` maps:
- A → web/rib width
- DPCT - D - 2*B → web height
- C → flange/hammer width
- B → flange/hammer height
- NR_COST → rib count

This is implemented literally.

## Taper limitation discovered

`GtAxDt` reads only grid column D for D_E. It does not read D_F.

Therefore the legacy FLECHA chain ignores taper end diameter.

The React adapter preserves that legacy behavior for parity and emits `FLECHA-TAPER-001`.

A future engineering change may introduce tapered FLECHA sections, but that must be qualified separately and must not be hidden inside the UI migration.

## Fixed FLECHA assumptions

`WrFl` writes:
- exactly two bearings;
- density 7850;
- E = 207;
- Poisson ratio = 0.3;
- maximum section capacity from NRESC (40 in iBiege3.ini);
- zero concentrated-mass components;
- zero X/Y/Z accelerations;
- one optional Y force and one optional Z force;
- rotor type 1=Rings, 2=Cage;
- amplification 1=Vibration, 2=Impact;
- shear toggle at the final input line.

These values are explicit in `FlechaCase` or the serializer rather than hidden in React controls.

## Output

`SwRs` assumes eight lines but does not validate count before indexing the descriptor array.

The TypeScript parser requires exactly 8 numeric values:
1. front bearing reaction Y [kN]
2. front bearing reaction Z [kN]
3. rear bearing reaction Y [kN]
4. rear bearing reaction Z [kN]
5. maximum deflection / air gap [%]
6. position of maximum deflection [mm]
7. front shaft deflection [mm]
8. rear shaft deflection [mm]

Malformed output is rejected explicitly.

## Execution still intentionally disabled

The legacy code:
- launches `Flecha.exe F`;
- polls one shared output path;
- uses a 180 s timeout;
- later deletes shared report/plot/input files.

The React implementation does not reproduce those shared-file behaviors. Solver execution will be enabled only after the Electron per-run workspace exists.
