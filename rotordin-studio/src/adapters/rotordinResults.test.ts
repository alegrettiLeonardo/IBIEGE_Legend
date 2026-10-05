import { describe, expect, it } from 'vitest';
import { parseCampbellCriticalSpeeds, parseRotorDinMarkedSections } from './rotordinResults';

describe('RotorDin marked stdout contract', () => {
  it('extracts #BEGIN/#END blocks and RD_AUDIT_V1 records without recomputing physics', () => {
    const sections = parseRotorDinMarkedSections([
      '#BEGIN output.out',
      'hello',
      '#END output.out',
      'RD_AUDIT_V1 SPECTRUM MODES 1800 STABLE -1 4 4 0 0 0',
      '',
    ].join('\n'));
    expect(sections['output.out']).toBe('hello\n');
    expect(sections['rdaudit.out']).toContain('RD_AUDIT_V1 SPECTRUM');
  });

  it('extracts solver-native Campbell crossings', () => {
    const text = [
      'EXCITATION_ORDERS 1.0 2.0 0.5',
      'Critical Speeds (rpm)',
      'header',
      '1000 2000 3000',
      '',
      'Critical Frequencies (Hz)',
      'header',
      '16.6667 33.3333 50',
      '',
    ].join('\n');
    expect(parseCampbellCriticalSpeeds(text)).toEqual([
      { rpm: 1000, hz: 16.6667, harmonic: '1.0x' },
      { rpm: 2000, hz: 33.3333, harmonic: '2.0x' },
      { rpm: 3000, hz: 50, harmonic: '0.5x' },
    ]);
  });
});
