import { describe, expect, it } from 'vitest';
import {
  importLegacyBearing,
  importLegacyLoadGrid,
  importLegacyShaftGrid,
  matrixViewForBearing,
  toLegacyShaftGrid,
} from './legacy';

describe('legacy shaft grid mapping', () => {
  it('maps L,D,DPCT,A,B,C,NR_COST,D_INT,D_F without losing taper orthogonality', () => {
    const imported = importLegacyShaftGrid([
      { L: 100, D: 80, DPCT: 120, A: 5, B: 6, C: 7, NR_COST: 8, D_F: 70 },
    ]);

    expect(imported.issues).toHaveLength(0);
    expect(imported.value[0]).toMatchObject({
      sectionType: 'ribbed',
      lengthMm: 100,
      outerDiameterMm: 80,
      endOuterDiameterMm: 70,
      ribbed: {
        packageDiameterMm: 120,
        ribThicknessMm: 5,
        hammerThicknessMm: 6,
        hammerWidthMm: 7,
        ribCount: 8,
      },
    });

    const roundTrip = toLegacyShaftGrid(imported.value)[0];
    expect(roundTrip).toMatchObject({ L: 100, D: 80, DPCT: 120, A: 5, B: 6, C: 7, NR_COST: 8, D_F: 70 });
  });

  it('flags an impossible legacy row containing hollow and ribbed data', () => {
    const imported = importLegacyShaftGrid([
      { L: 100, D: 80, DPCT: 120, A: 5, NR_COST: 8, D_INT: 30 },
    ]);
    expect(imported.issues.some((issue) => issue.code === 'LEGACY-SHAFT-001')).toBe(true);
  });
});

describe('legacy load mapping', () => {
  it('keeps Xi, LC, KG and magnetic thrust and accepts an explicit rotor stack row', () => {
    const imported = importLegacyLoadGrid([{ Xi: 10, LC: 20, KG: 30, Empuxo: 1 }], 0);
    expect(imported.value[0]).toMatchObject({
      startMm: 10,
      lengthMm: 20,
      massKg: 30,
      magneticThrust: true,
      isRotorStack: true,
    });
  });

  it('warns when rotor stack information is unavailable because VB6 did not persist CellBackColor', () => {
    const imported = importLegacyLoadGrid([{ Xi: 10, LC: 20, KG: 30, Empuxo: 0 }]);
    expect(imported.issues.some((issue) => issue.code === 'LEGACY-FLEXA-001')).toBe(true);
  });
});

describe('legacy bearing mapping', () => {
  it('preserves custom scalar stiffness exactly and projects it to a diagonal matrix for the React view', () => {
    const bearing = importLegacyBearing(0, 500, 3.75e9);
    expect(bearing.coefficients.kind).toBe('legacy-scalar');
    if (bearing.coefficients.kind !== 'legacy-scalar') throw new Error('unexpected model');
    expect(bearing.coefficients.stiffnessNPerM).toBe(3.75e9);

    const view = matrixViewForBearing(bearing);
    expect(view.stiffness.xx).toBe(3.75e9);
    expect(view.stiffness.zz).toBe(3.75e9);
    expect(view.stiffness.xz).toBe(0);
  });
});
