import { app, shell } from 'electron';
import { readdir, readFile, stat } from 'node:fs/promises';
import path from 'node:path';
import type {
  ExecuteQualifiedRunResponse,
  InspectRunRequest,
  InspectRunResponse,
  LoadQualifiedRunRequest,
  OpenRunWorkspaceRequest,
  QualifiedRunJobResponse,
  QualifiedRunSummary,
  ReadRunArtifactRequest,
  ReadRunArtifactResponse,
  RevealRunArtifactRequest,
} from '../contracts';
import type { AnalysisKind } from '../../src/domain';
import type { RotorDinParsedResults } from '../../src/adapters/rotordinResultData';
import type { RunArtifact, RunManifest } from '../../src/runs/model';

function runsRoot(): string {
  return path.join(app.getPath('userData'), 'runs');
}

function safeRunId(runId: string): string {
  if (!/^[A-Za-z0-9._-]+$/.test(runId)) {
    throw new Error('RUN-HISTORY-001: Invalid run identifier.');
  }
  return runId;
}

function runRoot(runId: string): string {
  return path.join(runsRoot(), safeRunId(runId));
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

function elapsedMs(manifest: RunManifest): number | undefined {
  if (!manifest.finishedAt) return undefined;
  const start = Date.parse(manifest.startedAt);
  const end = Date.parse(manifest.finishedAt);
  if (!Number.isFinite(start) || !Number.isFinite(end) || end < start) return undefined;
  return end - start;
}

function artifactMap(manifest: RunManifest): Map<string, RunArtifact> {
  return new Map(manifest.artifacts.map((artifact) => [artifact.relativePath, artifact]));
}

function resolveWorkspacePath(root: string, relativePath: string): string {
  const absolute = path.resolve(root, relativePath);
  const rootPrefix = path.resolve(root) + path.sep;
  if (absolute !== path.resolve(root) && !absolute.startsWith(rootPrefix)) {
    throw new Error('RUN-HISTORY-005: Artifact path escapes the run workspace.');
  }
  return absolute;
}

function allowedArtifactPath(manifest: RunManifest, relativePath: string): boolean {
  if (relativePath === 'run_manifest.json') return true;
  return artifactMap(manifest).has(relativePath);
}

async function resolveArtifact(
  runId: string,
  relativePath: string,
): Promise<{ root: string; manifest: RunManifest; absolute: string }> {
  const root = runRoot(runId);
  const manifest = await readManifest(root);
  if (manifest.runId !== runId) {
    throw new Error('RUN-HISTORY-003: Manifest/run-directory identity mismatch.');
  }
  if (!allowedArtifactPath(manifest, relativePath)) {
    throw new Error('RUN-HISTORY-007: Artifact is not registered in run_manifest.json.');
  }
  const absolute = resolveWorkspacePath(root, relativePath);
  return { root, manifest, absolute };
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
    const currentRoot = path.join(root, entry.name);
    try {
      const manifest = await readManifest(currentRoot);
      if (manifest.solver.kind !== 'rotordin') continue;
      rows.push({
        runId: manifest.runId,
        projectReference: manifest.projectReference,
        startedAt: manifest.startedAt,
        finishedAt: manifest.finishedAt,
        status: manifest.status,
        analyses: qualifiedAnalyses(manifest),
        artifactCount: manifest.artifacts.length,
        hasResults: Boolean(manifest.jobs?.some((job) => job.parsedResultPath)),
        solverHash: manifest.solver.executableHash,
        inputHash: manifest.inputHash,
        elapsedMs: elapsedMs(manifest),
      });
    } catch {
      // Corrupt/incomplete directories remain on disk as evidence but are not
      // exposed as valid run-manifest rows.
    }
  }

  return rows.sort((a, b) => b.startedAt.localeCompare(a.startedAt));
}

export async function inspectRun(
  request: InspectRunRequest,
): Promise<InspectRunResponse> {
  const root = runRoot(request.runId);
  const manifest = await readManifest(root);
  if (manifest.runId !== request.runId) {
    throw new Error('RUN-HISTORY-003: Manifest/run-directory identity mismatch.');
  }
  return { workspaceAbsolutePath: root, manifest };
}

export async function loadQualifiedRun(
  request: LoadQualifiedRunRequest,
): Promise<ExecuteQualifiedRunResponse> {
  const runId = safeRunId(request.runId);
  const root = runRoot(runId);
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
    const absolute = resolveWorkspacePath(root, job.parsedResultPath);
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

export async function readRunArtifact(
  request: ReadRunArtifactRequest,
): Promise<ReadRunArtifactResponse> {
  const { absolute } = await resolveArtifact(request.runId, request.relativePath);
  const info = await stat(absolute);
  if (!info.isFile()) {
    throw new Error('RUN-HISTORY-008: Selected artifact is not a regular file.');
  }

  const maximum = Math.max(1024, Math.min(request.maxBytes ?? 1_000_000, 2_000_000));
  const bytes = await readFile(absolute);
  const truncated = bytes.length > maximum;
  const slice = truncated ? bytes.subarray(0, maximum) : bytes;

  if (slice.includes(0)) {
    throw new Error('RUN-HISTORY-009: Binary artifact cannot be previewed as text.');
  }

  return {
    relativePath: request.relativePath,
    text: new TextDecoder('utf-8', { fatal: false }).decode(slice),
    bytes: bytes.length,
    truncated,
  };
}

export async function revealRunArtifact(
  request: RevealRunArtifactRequest,
): Promise<void> {
  const { absolute } = await resolveArtifact(request.runId, request.relativePath);
  shell.showItemInFolder(absolute);
}

export async function openRunWorkspace(
  request: OpenRunWorkspaceRequest,
): Promise<void> {
  const root = runRoot(request.runId);
  const manifest = await readManifest(root);
  if (manifest.runId !== request.runId) {
    throw new Error('RUN-HISTORY-003: Manifest/run-directory identity mismatch.');
  }
  const error = await shell.openPath(root);
  if (error) throw new Error(`RUN-HISTORY-010: ${error}`);
}
