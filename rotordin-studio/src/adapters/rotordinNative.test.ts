import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { importIrDinText } from './irdinText';
import { serializeRotorDinNative } from './rotordinNative';

const fixtureUrl = new URL('../../tests/fixtures/EST-ST41_1000_B3_60HZ_1675_63536.irdin.txt', import.meta.url);
const goldenUrl = new URL('../../golden/est-st41/modes-v1/input.txt', import.meta.url);

const project = importIrDinText(
  readFileSync(fileURLToPath(fixtureUrl), 'utf8'),
).project;

describe('audited current RotorDin native TypeScript serializer', () => {
  it('reproduces the frozen EST-ST41 Modes native input byte-for-byte', () => {
    const candidate = serializeRotorDinNative(project, 'modes');
    const golden = readFileSync(fileURLToPath(goldenUrl), 'utf8');
    expect(candidate.input).toBe(golden);
    expect(candidate.bearingTableRangeRpm).toEqual([500, 4000]);
    expect(candidate.responseGrid).toMatchObject({
      initialRpm: 500,
      finalRpm: 3000,
      points: 402,
    });
  });

  it('qualifies Campbell only inside the common bearing TABLE domain', () => {
    const candidate = serializeRotorDinNative(project, 'campbell');
    expect(candidate.input).toContain(
      '5.0000E+023.0000E+032.5000E+01',
    );
  });

  it('uses the audited native command flags per analysis', async () => {
    const { nativeFlags } = await import('./rotordinNative');
    expect(nativeFlags('campbell', true)).toEqual(['-std', '-c', '-b']);
    expect(nativeFlags('unbalance-response', true)).toEqual(['-std', '-f', '-b']);
    expect(nativeFlags('elastic-line', true)).toEqual(['-std', '-s', '-b']);
    expect(nativeFlags('critical-speed-map', true)).toEqual(['-std', '-k', '-b']);
    expect(nativeFlags('orbit', true)).toEqual(['-std', '-t', '-b']);
    expect(nativeFlags('modes', true)).toEqual(['-std', '-m', '-b']);
  });
});
