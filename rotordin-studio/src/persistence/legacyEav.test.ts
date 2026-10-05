import { describe, expect, it } from 'vitest';
import { demoProject } from '../data/demoProject';
import { decodeLegacyEav, encodeLegacyEav } from './legacyEav';

describe('legacy d_idnbiege / d_carbiege adapter', () => {
  it('encodes bearing records using the audited negative line convention', () => {
    const encoded = encodeLegacyEav(demoProject, 42, '05/10/2026 08:30:00');
    expect(encoded.characteristics).toEqual(expect.arrayContaining([
      expect.objectContaining({ calculationId: 42, line: -1, name: 'mancal', value: 536.47 }),
      expect.objectContaining({ calculationId: 42, line: -2, name: 'constante', value: 5.7e7 }),
      expect.objectContaining({ calculationId: 42, line: -3, name: 'mancal', value: 3027 }),
      expect.objectContaining({ calculationId: 42, line: -4, name: 'constante', value: 2.029e9 }),
    ]));
  });

  it('round-trips legacy-representable shaft/load/bearing data without truncating custom stiffness', () => {
    const encoded = encodeLegacyEav(demoProject, 42, '05/10/2026 08:30:00');
    const decoded = decodeLegacyEav(encoded.header, encoded.characteristics);

    expect(decoded.value.segments).toHaveLength(demoProject.segments.length);
    expect(decoded.value.legacyLoads).toHaveLength(demoProject.legacyLoads.length);
    expect(decoded.value.bearings).toHaveLength(2);

    const brg2 = decoded.value.bearings[1];
    expect(brg2.coefficients.kind).toBe('legacy-scalar');
    if (brg2.coefficients.kind !== 'legacy-scalar') throw new Error('unexpected model');
    expect(brg2.coefficients.stiffnessNPerM).toBe(2.029e9);
  });

  it('explicitly reports that rotor-stack identity is lost in the legacy database schema', () => {
    const encoded = encodeLegacyEav(demoProject, 42, '05/10/2026 08:30:00');
    expect(encoded.issues.some((issue) => issue.code === 'LEGACY-DB-ROTOR-STACK-DROP')).toBe(true);

    const decoded = decodeLegacyEav(encoded.header, encoded.characteristics);
    expect(decoded.issues.some((issue) => issue.code === 'LEGACY-DB-ROTOR-STACK')).toBe(true);
  });
});
