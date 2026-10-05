import { describe, expect, it } from 'vitest';
import { demoProject } from '../data/demoProject';
import { loadsOverlapInclusive, validateProject } from './validation';

describe('validation derived from legacy behavior', () => {
  it('detects the interval-containment overlap missed by the VB6 endpoint-only implementation', () => {
    const outer = { id: 'outer', startMm: 0, lengthMm: 100, massKg: 1, magneticThrust: false, isRotorStack: false };
    const inner = { id: 'inner', startMm: 20, lengthMm: 10, massKg: 1, magneticThrust: false, isRotorStack: false };
    expect(loadsOverlapInclusive(outer, inner)).toBe(true);
  });

  it('keeps the demonstration project valid for the audited legacy-compatible geometry', () => {
    const result = validateProject(demoProject);
    expect(result.issues.filter((issue) => issue.severity === 'error')).toEqual([]);
  });

  it('rejects a shaft containing both hollow and ribbed segments because legacy IncSgmnt/AltSgmnt forbids it', () => {
    const project = structuredClone(demoProject);
    project.segments = [
      { id: 'h', sectionType: 'hollow', lengthMm: 100, outerDiameterMm: 80, innerDiameterMm: 30 },
      {
        id: 'r',
        sectionType: 'ribbed',
        lengthMm: 100,
        outerDiameterMm: 80,
        ribbed: { packageDiameterMm: 120, ribThicknessMm: 5, hammerThicknessMm: 5, hammerWidthMm: 8, ribCount: 6 },
      },
    ];
    project.legacyLoads = [];
    const result = validateProject(project);
    expect(result.issues.some((issue) => issue.code === 'LEGACY-SHAFT-002')).toBe(true);
  });
});
