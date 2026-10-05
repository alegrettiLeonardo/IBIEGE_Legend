import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { parseCampbellCriticalSpeeds } from './rotordinResults';
import {
  parseCampbellData,
  parseCriticalMapData,
  parseElasticLineData,
  parseModeShapeData,
  parseOrbitData,
  parseStaticLateralData,
  parseUnbalanceData,
} from './rotordinResultData';

function golden(relative: string): string {
  const url = new URL(`../../golden/est-st41/${relative}`, import.meta.url);
  return readFileSync(fileURLToPath(url), 'utf8');
}

describe('RotorDin native result parsers against EST-ST41 frozen outputs', () => {
  it('parses Campbell frequencies, logarithmic decrement and critical-speed crossings', () => {
    const text = golden('campbell-v1/sections/campbell.out');
    const critical = parseCampbellCriticalSpeeds(text);
    const parsed = parseCampbellData(text, critical);
    expect(parsed).toBeDefined();
    expect(parsed?.naturalFrequency).toHaveLength(9);
    expect(parsed?.harmonics).toHaveLength(3);
    expect(parsed?.logarithmicDecrement).toHaveLength(9);
    expect(parsed?.criticalSpeeds).toHaveLength(23);
    expect(parsed?.naturalFrequency[0].x[0]).toBeCloseTo(500, 8);
    expect(parsed?.frequencyKind).toBe('damped_imaginary_part');
  });

  it('parses the full qualified unbalance response grid and native peaks', () => {
    const parsed = parseUnbalanceData(golden('response-v1/sections/unblresp.out'));
    expect(parsed).toBeDefined();
    expect(parsed?.responsePositionsM).toHaveLength(4);
    expect(parsed?.amplitude).toHaveLength(4);
    expect(parsed?.phase).toHaveLength(4);
    expect(parsed?.amplitude[0].x).toHaveLength(402);
    expect(parsed?.phase[0].x).toHaveLength(402);
    expect(parsed?.peaks).toHaveLength(10);
    expect(parsed?.amplitude[0].x[0]).toBeCloseTo(500, 8);
    expect(parsed?.amplitude[0].x.at(-1)).toBeCloseTo(2999.9944, 6);
  });

  it('parses elastic-line displacement and static lateral reactions', () => {
    const text = golden('line-v1/sections/selaline.out');
    const line = parseElasticLineData(text);
    const staticData = parseStaticLateralData(text);
    expect(line?.positionM).toHaveLength(30);
    expect(line?.xM).toHaveLength(30);
    expect(line?.zM).toHaveLength(30);
    expect(staticData?.bearingForce).toHaveLength(2);
    expect(staticData?.bendingMoment).toHaveLength(2);
    expect(staticData?.shearForce).toHaveLength(2);
    expect(staticData?.bendingStress).toHaveLength(1);

    const resultant = line!.positionM.map((_, index) =>
      Math.hypot(line!.xM[index], line!.zM[index]));
    expect(Math.max(...resultant)).toBeCloseTo(1.597994595e-4, 12);
  });

  it('parses the critical-speed stiffness map', () => {
    const parsed = parseCriticalMapData(
      golden('map-v1/sections/ucspdmap.out'),
      'Critical speed map',
    );
    expect(parsed?.stiffnessNPerM).toHaveLength(37);
    expect(parsed?.modesRpm).toHaveLength(9);
    expect(parsed?.ratedSpeedRpm).toBeCloseTo(1800, 8);
    expect(parsed?.stiffnessNPerM[0]).toBe(1e7);
    expect(parsed?.stiffnessNPerM.at(-1)).toBe(1e11);
  });

  it('parses all qualified orbit sections and points at 1800 rpm', () => {
    const parsed = parseOrbitData(golden('orbit-v1/sections/timeresp.out'));
    expect(parsed?.speedRpm).toBeCloseTo(1800, 8);
    expect(parsed?.points).toBe(50);
    expect(parsed?.speedCases).toHaveLength(1);
    expect(parsed?.sections).toHaveLength(30);
    expect(parsed?.sections[0].xM).toHaveLength(50);
    expect(parsed?.sections[0].zM).toHaveLength(50);
  });

  it('parses the ten frozen mode shapes from modeshp.out', () => {
    const parsed = parseModeShapeData(golden('modes-v1/sections/modeshp.out'));
    expect(parsed?.rpm).toBeCloseTo(1800, 8);
    expect(parsed?.modes).toHaveLength(10);
    expect(parsed?.modes[0].frequencyHz).toBeCloseTo(21.4884740037, 7);
    expect(parsed?.modes[0].direction).toBe('BW');
    expect(parsed?.modes[0].positionM).toHaveLength(30);
    expect(parsed?.modes[0].xM).toHaveLength(30);
    expect(parsed?.modes[0].zM).toHaveLength(30);
  });
});
