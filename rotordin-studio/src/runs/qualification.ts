import type { AnalysisKind } from '../domain';

export type QualifiedAnalysis = AnalysisKind;

export interface AnalysisQualification {
  analysis: QualifiedAnalysis;
  nativeJob:
    | 'modes'
    | 'campbell'
    | 'response'
    | 'line'
    | 'map'
    | 'orbit';
  qualificationId: string;
  status: 'qualified' | 'pending';
  flags: string[];
  goldenPath: string;
  notes?: string;
}

export const ANALYSIS_QUALIFICATIONS: Record<QualifiedAnalysis, AnalysisQualification> = {
  modes: {
    analysis: 'modes',
    nativeJob: 'modes',
    qualificationId: 'A10-EST-ST41-MODES-V1',
    status: 'qualified',
    flags: ['-std', '-m', '-b'],
    goldenPath: 'golden/est-st41/modes-v1',
  },
  campbell: {
    analysis: 'campbell',
    nativeJob: 'campbell',
    qualificationId: 'A11-EST-ST41-CAMPBELL-V1',
    status: 'pending',
    flags: ['-std', '-c', '-b'],
    goldenPath: 'golden/est-st41/campbell-v1',
    notes: 'Qualified range decision is 500–3000 rpm; no TABLE extrapolation below 500 rpm.',
  },
  stability: {
    analysis: 'stability',
    nativeJob: 'campbell',
    qualificationId: 'A11-EST-ST41-CAMPBELL-V1',
    status: 'pending',
    flags: ['-std', '-c', '-b'],
    goldenPath: 'golden/est-st41/campbell-v1',
    notes: 'Log decrement/stability is emitted by the same native Campbell computation.',
  },
  'unbalance-response': {
    analysis: 'unbalance-response',
    nativeJob: 'response',
    qualificationId: 'A11-EST-ST41-RESPONSE-V1',
    status: 'pending',
    flags: ['-std', '-f', '-b'],
    goldenPath: 'golden/est-st41/response-v1',
  },
  'elastic-line': {
    analysis: 'elastic-line',
    nativeJob: 'line',
    qualificationId: 'A11-EST-ST41-LINE-V1',
    status: 'pending',
    flags: ['-std', '-s', '-b'],
    goldenPath: 'golden/est-st41/line-v1',
  },
  'critical-speed-map': {
    analysis: 'critical-speed-map',
    nativeJob: 'map',
    qualificationId: 'A11-EST-ST41-MAP-V1',
    status: 'pending',
    flags: ['-std', '-k', '-b'],
    goldenPath: 'golden/est-st41/map-v1',
  },
  orbit: {
    analysis: 'orbit',
    nativeJob: 'orbit',
    qualificationId: 'A11-EST-ST41-ORBIT-V1',
    status: 'pending',
    flags: ['-std', '-t', '-b'],
    goldenPath: 'golden/est-st41/orbit-v1',
  },
};

export function qualificationFor(analysis: QualifiedAnalysis): AnalysisQualification {
  return ANALYSIS_QUALIFICATIONS[analysis];
}

export function isAnalysisQualified(analysis: QualifiedAnalysis): boolean {
  return qualificationFor(analysis).status === 'qualified';
}

export function assertAnalysesQualified(analyses: QualifiedAnalysis[]): AnalysisQualification[] {
  const unique = [...new Set(analyses)];
  const records = unique.map(qualificationFor);
  const pending = records.filter((record) => record.status !== 'qualified');
  if (pending.length) {
    throw new Error(
      'RUN-QUAL-002: Analysis execution is blocked until scientific qualification closes: '
      + pending.map((record) => `${record.analysis} (${record.qualificationId})`).join(', '),
    );
  }
  return records;
}

export function nativeJobsForQualifiedAnalyses(
  analyses: QualifiedAnalysis[],
): AnalysisQualification[] {
  const records = assertAnalysesQualified(analyses);
  const byJob = new Map<AnalysisQualification['nativeJob'], AnalysisQualification>();
  for (const record of records) {
    if (!byJob.has(record.nativeJob)) byJob.set(record.nativeJob, record);
  }
  return [...byJob.values()];
}
