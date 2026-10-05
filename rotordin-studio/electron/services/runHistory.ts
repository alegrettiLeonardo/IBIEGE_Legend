import { app } from 'electron';
import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';
import type {
  ExecuteQualifiedRunResponse,
  LoadQualifiedRunRequest,
  QualifiedRunJobResponse,
  QualifiedRunSummary,
} from '../contracts';
import type { AnalysisKind } from '../../src/domain';
import type { RotorDinParsedResults } from '../../src/adapters/rotordinResultData';
import type { RunManifest } from '../../src/runs/model';

function runsRoot(): string {
  return path.join(app.getPath('userData'), 'runs');
}

function safeRunId(runId: string): string {
  if (!/^[A-Za-z0-9._-]+$/.test(runId)) {
    throw new Error('RUN-HISTORY-001: Invalid run identifier.');
  }
  return runId;
}

async function readManifest(root: string): Promise<RunManifest> {
  const text = await readFile(path.join(root, 'run_manifest.json'), 'utf8');
  const value = JSON.parse(text) as RunManifest;
  if (value.schemaVersion !== 1 || typeof value.runId !== 'string') {
    throw new Error('RUN-HISTORY-002: Invalid run_manifest.json.');
  }
  return value;
}

function qualifiedAnalyses(manifest: RunManifest): AnalysisKind[] {
  return Object.keys(manifest.qualifications ?? {}) as AnalysisKind[];
}

export async function listQualifiedRuns(): Promise<QualifiedRunSummary[]> {
  const root = runsRoot();
  let entries: Array<{ name: string; isDirectory(): boolean }>;
  try {
    entries = await readdir(root, { withFileTypes: true });
  } catch (error) {
    const code = (error as NodeJS.ErrnoException).code;
    if (code === 'ENOENT') return [];
    throw error;
  }

  const rows: QualifiedRunSummary[] = [];
  for (const entry of entries) {
    if (!entry.isDirectory()) continue;
    const runRoot = path.join(root, entry.name);
    try {
      const manifest = await readManifest(runRoot);
      if (manifest.solver.kind !== 'rotordin') continue;
      if (!manifest.jobs?.some((job) => job.parsedResultPath)) continue;
      rows.push({
        runId: manifest.runId,
        projectReference: manifest.projectReference,
        startedAt: manifest.startedAt,
        finishedAt: manifest.finishedAt,
        status: manifest.status,
        analyses: qualifiedAnalyses(manifest),
        artifactCount: manifest.artifacts.length,
      });
    } catch {
      // Corrupt/incomplete directories remain on disk as evidence but are not
      // exposed as selectable qualified result runs.
    }
  }

  return rows.sort((a, b) => b.startedAt.localeCompare(a.startedAt));
}

export async function loadQualifiedRun(
  request: LoadQualifiedRunRequest,
): Promise<ExecuteQualifiedRunResponse> {
  const runId = safeRunId(request.runId);
  const root = path.join(runsRoot(), runId);
  const manifest = await readManifest(root);

  if (manifest.runId !== runId) {
    throw new Error('RUN-HISTORY-003: Manifest/run-directory identity mismatch.');
  }
  if (manifest.solver.kind !== 'rotordin') {
    throw new Error('RUN-HISTORY-004: Selected run is not a RotorDin qualified run.');
  }

  const jobs: QualifiedRunJobResponse[] = [];
  for (const job of manifest.jobs ?? []) {
    if (!job.parsedResultPath) continue;
    const absolute = path.resolve(root, job.parsedResultPath);
    const rootPrefix = path.resolve(root) + path.sep;
    if (!absolute.startsWith(rootPrefix)) {
      throw new Error('RUN-HISTORY-005: Parsed result path escapes the run workspace.');
    }

    const results = JSON.parse(await readFile(absolute, 'utf8')) as RotorDinParsedResults;
    const criticalSpeeds = results.campbell?.criticalSpeeds ?? [];
    jobs.push({
      analysis: job.analysis as AnalysisKind,
      qualificationId: job.qualificationId,
      flags: [...job.flags],
      sectionNames: [...job.sections],
      criticalSpeeds,
      parsedResultPath: job.parsedResultPath,
      results,
    });
  }

  if (!jobs.length) {
    throw new Error('RUN-HISTORY-006: Selected run has no persisted parsed native results.');
  }

  return {
    workspaceAbsolutePath: root,
    manifest,
    jobs,
  };
}
