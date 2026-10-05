import { readFileSync, existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { ANALYSIS_QUALIFICATIONS } from './qualification';

describe('qualified-analysis golden evidence', () => {
  it('requires every qualified analysis to reference a persisted EST-ST41 golden', () => {
    for (const record of Object.values(ANALYSIS_QUALIFICATIONS)) {
      if (record.status !== 'qualified') continue;
      const root = resolve(record.goldenPath);
      const qualification = resolve(root, 'qualification.json');
      expect(existsSync(qualification), `${record.analysis} missing ${qualification}`).toBe(true);

      const payload = JSON.parse(readFileSync(qualification, 'utf8')) as {
        qualification_id: string;
        source_authority?: { head?: string };
      };
      expect(payload.qualification_id).toBe(record.qualificationId);
      expect(payload.source_authority?.head).toBe('c669978bdcc42c06507eac8fda8b6dc39e8bce97');
    }
  });

  it('keeps the exact native input frozen for every qualified job', () => {
    for (const record of Object.values(ANALYSIS_QUALIFICATIONS)) {
      if (record.status !== 'qualified') continue;
      expect(existsSync(resolve(record.goldenPath, 'input.txt'))).toBe(true);
    }
  });
});
