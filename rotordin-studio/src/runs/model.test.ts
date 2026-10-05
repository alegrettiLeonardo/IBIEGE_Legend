import { describe, expect, it } from 'vitest';
import {
  artifactIsFresh,
  createRunId,
  planRunWorkspace,
  requireFreshArtifact,
} from './model';

describe('per-run workspace and output freshness', () => {
  it('creates deterministic collision-resistant run paths', () => {
    const id = createRunId('0V / MGF', new Date('2026-10-05T11:30:45Z'), 7);
    expect(id).toBe('0V_MGF-20261005-113045-007');

    const workspace = planRunWorkspace(id);
    expect(workspace.inputDir).toContain('/01_Input');
    expect(workspace.resultDir).toContain('/03_Results');
    expect(workspace.manifestPath).toContain('run_manifest.json');
  });

  it('rejects the stale-output scenario present in legacy efetua_calculo', () => {
    const start = Date.parse('2026-10-05T11:30:00Z');
    const oldOutput = Date.parse('2026-10-05T11:20:00Z');

    expect(artifactIsFresh({ runStartedAtMs: start, artifactModifiedAtMs: oldOutput })).toBe(false);
    expect(() => requireFreshArtifact({
      runStartedAtMs: start,
      artifactModifiedAtMs: oldOutput,
      artifactPath: 'Biegedat.bga',
    })).toThrow(/STALE-RUN-OUTPUT/);
  });

  it('accepts an output created after the run starts', () => {
    const start = Date.parse('2026-10-05T11:30:00Z');
    const output = Date.parse('2026-10-05T11:30:05Z');
    expect(artifactIsFresh({ runStartedAtMs: start, artifactModifiedAtMs: output })).toBe(true);
  });
});
