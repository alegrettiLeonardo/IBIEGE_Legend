import { describe, expect, it } from 'vitest';
import { demoProject } from '../data/demoProject';
import { parseFlechaResult, prepareFlechaSections, serializeFlecha } from './flecha';

const config = {
  frame: '630',
  powerKwOrKva: 2000,
  speedRpm: 375,
  poles: 16,
  airGapMm: 7.4,
  meanAirGapInductionT: 0.82,
  rotorLamDiameterMm: 1130,
  rotorType: 'cage' as const,
  rotorStackLengthMm: 1321,
  shaftInclinationDeg: 0,
  amplification: 'vibration' as const,
  shearDeformation: true,
  forceY_N: -1850,
  forceZ_N: 0,
  forcePositionMm: 3270,
};

describe('FLECHA legacy adapter', () => {
  it('subdivides loaded shaft segments using the audited 0.95 threshold', () => {
    const prepared = prepareFlechaSections(demoProject, config.airGapMm, 0.95);
    expect(prepared.issues.filter((issue) => issue.severity === 'error')).toEqual([]);
    expect(prepared.sections.length).toBeGreaterThan(demoProject.segments.length);
    expect(prepared.sections.some((section) => section.distributedMassKg === 2838 && section.airGapMm === 7.4)).toBe(true);
  });

  it('serializes the fixed legacy material constants and two-bearing contract', () => {
    const result = serializeFlecha(demoProject, config);
    expect(result.issues.filter((issue) => issue.severity === 'error')).toEqual([]);
    expect(result.text).toBeDefined();
    const lines = result.text!.split('\r\n');
    expect(lines[0]).toBe('630');
    expect(lines[4]).toBe('2');
    expect(lines[9]).toBe('7850');
    expect(lines[10]).toBe('207');
    expect(lines[11]).toBe('0.3');
  });

  it('documents that taper is ignored by the legacy FLECHA section preparation', () => {
    const result = prepareFlechaSections(demoProject, config.airGapMm, 0.95);
    expect(result.issues.some((issue) => issue.code === 'FLECHA-TAPER-001')).toBe(true);
  });

  it('parses exactly eight result lines', () => {
    const parsed = parseFlechaResult('18.42\n1.16\n17.98\n0.94\n14.2\n1136\n0.21\n0.18\n');
    expect(parsed.frontBearingReactionY_kN).toBe(18.42);
    expect(parsed.maxDeflectionPercentOfAirGap).toBe(14.2);
    expect(parsed.rearShaftDeflectionMm).toBe(0.18);
  });

  it('rejects malformed result schemas instead of indexing beyond the descriptor list like VB6 SwRs', () => {
    expect(() => parseFlechaResult('1\n2\n3\n')).toThrow(/exactly 8/);
  });
});
