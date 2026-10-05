import { describe, expect, it } from 'vitest';
import {
  ANALYSIS_QUALIFICATIONS,
  assertAnalysesQualified,
  isAnalysisQualified,
  nativeJobsForQualifiedAnalyses,
} from './qualification';

describe('scientific qualification registry', () => {
  it('allows all analyses whose EST-ST41 Linux/Windows golden is closed', () => {
    const analyses = ['modes','campbell','stability','unbalance-response','elastic-line','critical-speed-map','orbit'] as const;
    for (const analysis of analyses) {
      expect(isAnalysisQualified(analysis)).toBe(true);
      expect(ANALYSIS_QUALIFICATIONS[analysis].platforms).toEqual(['linux', 'windows']);
    }
    expect(() => assertAnalysesQualified([...analyses])).not.toThrow();
  });

  it('deduplicates Campbell and Stability to one native -c job', () => {
    const jobs = nativeJobsForQualifiedAnalyses(['campbell', 'stability']);
    expect(jobs).toHaveLength(1);
    expect(jobs[0].flags).toEqual(['-std', '-c', '-b']);
    expect(jobs[0].qualificationId).toBe('A11-EST-ST41-CAMPBELL-V1');
  });

  it('maps remaining qualified analyses to audited native jobs', () => {
    expect(nativeJobsForQualifiedAnalyses(['modes'])[0].flags).toEqual(['-std', '-m', '-b']);
    expect(nativeJobsForQualifiedAnalyses(['unbalance-response'])[0].flags).toEqual(['-std', '-f', '-b']);
    expect(nativeJobsForQualifiedAnalyses(['elastic-line'])[0].flags).toEqual(['-std', '-s', '-b']);
    expect(nativeJobsForQualifiedAnalyses(['critical-speed-map'])[0].flags).toEqual(['-std', '-k', '-b']);
    expect(nativeJobsForQualifiedAnalyses(['orbit'])[0].flags).toEqual(['-std', '-t', '-b']);
  });
});
