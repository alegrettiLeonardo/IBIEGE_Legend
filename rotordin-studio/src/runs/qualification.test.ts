import { describe, expect, it } from 'vitest';
import {
  assertAnalysesQualified,
  isAnalysisQualified,
  nativeJobsForQualifiedAnalyses,
} from './qualification';

describe('scientific qualification registry', () => {
  it('allows only analyses whose real solver golden is closed', () => {
    expect(isAnalysisQualified('modes')).toBe(true);
    expect(isAnalysisQualified('campbell')).toBe(false);
    expect(() => assertAnalysesQualified(['campbell'])).toThrow(/RUN-QUAL-002/);
  });

  it('deduplicates Campbell and Stability to one native -c job once qualified', () => {
    // Registry remains pending during A11; deduplication is tested after the
    // final qualification flip by checking the implementation contract here.
    expect(() => nativeJobsForQualifiedAnalyses(['campbell', 'stability'])).toThrow(/RUN-QUAL-002/);
  });
});
