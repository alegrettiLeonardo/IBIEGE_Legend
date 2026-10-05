import { describe, expect, it } from 'vitest';
import type { ExecuteQualifiedRunResponse } from '../../electron/contracts';
import type { RunManifest } from './model';
import { compareQualifiedRuns } from './comparison';

function manifest(runId: string, inputHash: string): RunManifest {
  return {
    schemaVersion: 1,
    runId,
    projectId: 'p',
    projectReference: 'EST',
    solver: { kind: 'rotordin', executableHash: 'solver-hash' },
    startedAt: '2026-10-05T10:00:00.000Z',
    finishedAt: '2026-10-05T10:00:01.000Z',
    projectHash: 'project-hash',
    inputHash,
    exitCode: 0,
    status: 'success',
    warnings: [],
    artifacts: [],
    jobs: [],
  };
}

function run(runId: string, frequency: number, displacement: number): ExecuteQualifiedRunResponse {
  return {
    workspaceAbsolutePath: `/tmp/${runId}`,
    manifest: manifest(runId, `input-${runId}`),
    jobs: [
      {
        analysis: 'modes',
        qualificationId: 'A10',
        flags: ['-std', '-m', '-b'],
        sectionNames: ['modeshp.out'],
        criticalSpeeds: [],
        parsedResultPath: '03_Results/modes/parsed_results.json',
        results: {
          modes: {
            rpm: 1800,
            modes: [{
              mode: 1,
              frequencyHz: frequency,
              direction: 'BW',
              positionM: [0, 1],
              xM: [0, displacement],
              zM: [0, 0],
            }],
          },
          elasticLine: {
            positionM: [0, 1],
            xM: [0, displacement],
            zM: [0, 0],
            gxRad: [0, 0],
            gzRad: [0, 0],
          },
        },
      },
    ],
  };
}

describe('qualified run engineering comparison', () => {
  it('reports hash identity and numerical deltas without recomputing solver physics', () => {
    const baseline = run('run-a', 20, 1e-4);
    const candidate = run('run-b', 21, 1.2e-4);
    candidate.manifest.projectHash = baseline.manifest.projectHash;
    candidate.manifest.solver.executableHash = baseline.manifest.solver.executableHash;

    const result = compareQualifiedRuns(baseline, candidate);
    expect(result.solverHashSame).toBe(true);
    expect(result.projectHashSame).toBe(true);
    expect(result.inputHashSame).toBe(false);

    const mode = result.metrics.find((item) => item.metric === 'Mode 1 frequency');
    expect(mode).toMatchObject({ baseline: 20, candidate: 21, delta: 1, unit: 'Hz' });

    const line = result.metrics.find((item) => item.metric === 'Maximum resultant displacement');
    expect(line?.baseline).toBeCloseTo(100, 9);
    expect(line?.candidate).toBeCloseTo(120, 9);
    expect(line?.unit).toBe('µm');
  });

  it('reports analysis-set differences', () => {
    const baseline = run('run-a', 20, 1e-4);
    const candidate = run('run-b', 20, 1e-4);
    candidate.jobs[0].analysis = 'elastic-line';

    const result = compareQualifiedRuns(baseline, candidate);
    expect(result.commonAnalyses).toEqual([]);
    expect(result.baselineOnlyAnalyses).toEqual(['modes']);
    expect(result.candidateOnlyAnalyses).toEqual(['elastic-line']);
  });
});
