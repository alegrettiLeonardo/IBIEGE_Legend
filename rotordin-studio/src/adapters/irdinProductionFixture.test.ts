import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { legacyCompatibilityNotes, validateProject } from '../domain';
import { importIrDinText } from './irdinText';

const fixtureUrl = new URL('../../tests/fixtures/EST-ST41_1000_B3_60HZ_1675_63536.irdin.txt', import.meta.url);
const text = readFileSync(fileURLToPath(fixtureUrl), 'utf8');

describe('EST-ST41 production irDin fixture', () => {
  const imported = importIrDinText(text);

  it('imports project identity and global settings', () => {
    expect(imported.project.reference).toBe('EST');
    expect(imported.project.component).toBe('ST41_1000_B3_60HZ_1675_63536');
    expect(imported.project.nominalSpeedRpm).toBe(1800);
    expect(imported.project.settings?.campbell).toMatchObject({
      initialRpm: 0,
      finalRpm: 3000,
      divisions: 25,
      rotations: 9,
      interpolationPoints: 50,
    });
  });

  it('imports all 17 shaft sections and preserves combined ribbed+hollow section 9', () => {
    expect(imported.project.segments).toHaveLength(17);
    const section9 = imported.project.segments[8];
    expect(section9).toMatchObject({
      lengthMm: 566.5,
      outerDiameterMm: 390,
      innerDiameterMm: 60,
      ribbed: {
        packageDiameterMm: 620,
        ribThicknessMm: 127,
        ribCount: 4,
      },
    });
    expect(validateProject(imported.project).valid).toBe(true);
    expect(legacyCompatibilityNotes(imported.project).some((issue) => issue.code === 'LEGACY-SHAFT-003')).toBe(true);
  });

  it('imports masses including the explicit rotor-package flag', () => {
    expect(imported.project.legacyLoads).toHaveLength(3);
    expect(imported.project.legacyLoads[0]).toMatchObject({
      startMm: 1370.9,
      lengthMm: 1675,
      massKg: 10090,
      outerDiameterMm: 1140,
      isRotorStack: true,
    });
  });

  it('imports both embedded bearing tables with 11 speed points', () => {
    expect(imported.project.bearings).toHaveLength(2);
    for (const bearing of imported.project.bearings) {
      expect(bearing.coefficients.kind).toBe('matrix');
      if (bearing.coefficients.kind !== 'matrix') throw new Error('unexpected bearing model');
      expect(bearing.coefficients.source).toBe('speed-dependent');
      expect(bearing.coefficients.table).toHaveLength(11);
      expect(bearing.coefficients.table?.[0].speedRpm).toBe(500);
      expect(bearing.coefficients.table?.[10].speedRpm).toBe(4000);
    }

    const first = imported.project.bearings[0];
    if (first.coefficients.kind !== 'matrix') throw new Error('unexpected bearing model');
    expect(first.coefficients.table?.[0]).toMatchObject({
      speedRpm: 500,
      stiffness: {
        xx: 27.35e7,
        xz: -16.07e7,
        zx: -98.04e7,
        zz: 24.86e8,
      },
      damping: {
        xx: 30.66e5,
        xz: -68.33e5,
        zx: -68.33e5,
        zz: 37.28e6,
      },
    });
  });

  it('imports unbalance, response points and supports', () => {
    expect(imported.project.forces).toHaveLength(2);
    expect(imported.project.forces[0]).toMatchObject({
      positionMm: 1370.9,
      phaseDeg: 0,
      magnitude: 110175.3,
      unit: 'g-mm',
    });
    expect(imported.project.responsePoints).toHaveLength(4);
    expect(imported.project.supports).toHaveLength(2);
    expect(imported.project.supports[0]).toMatchObject({
      locationMm: 550,
      massKg: 415,
      stiffness: { xx: 2730000000, zz: 3410000000, xz: 0, zx: 0 },
    });
  });
});
