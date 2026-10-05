import { describe, expect, it } from 'vitest';
import { demoProject } from '../data/demoProject';
import {
  interpretNkrit,
  parseNkrit,
  requiresMagneticThrustFile,
  serializeBiege14,
} from './biege';

describe('BIEGE14 legacy adapter', () => {
  it('serializes the audited current BIEGE14 contract semantically', () => {
    const result = serializeBiege14(demoProject);
    expect(result.issues.filter((issue) => issue.severity === 'error')).toEqual([]);
    expect(result.text).toBeDefined();
    expect(result.text).toContain('BIEGE14\r\n');
    expect(result.text).toContain("'MM'  2 2.06E+011 7850 'JA'");
    expect(result.text).toContain('3330.000000');
    expect(result.text).toContain("'ST'");
    expect(result.text).toContain('1146.000000\t2467.000000\t2838.000000\t0.0\t1');
  });

  it('recognizes the magnetic-thrust auxiliary-file dependency', () => {
    expect(requiresMagneticThrustFile(demoProject)).toBe(true);
  });

  it('parses multiple NKRIT occurrences without assuming their meaning in the parser', () => {
    const parsed = parseNkrit('header\nNKRIT = 3600 RPM\nother\n NKRIT=4200 something\n');
    expect(parsed.valuesRpm).toEqual([3600, 4200]);

    const interpreted = interpretNkrit(parsed, true);
    expect(interpreted.magneticCriticalRpm).toBe(3600);
    expect(interpreted.criticalRpm).toBe(4200);
    expect(interpreted.criticalHz).toBe(70);
  });

  it('uses the first NKRIT as the ordinary critical speed when no magnetic thrust is active', () => {
    const interpreted = interpretNkrit(parseNkrit('NKRIT = 3000\n'), false);
    expect(interpreted.criticalRpm).toBe(3000);
    expect(interpreted.criticalHz).toBe(50);
  });
});
