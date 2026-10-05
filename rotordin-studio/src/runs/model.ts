export type SolverKind = 'rotordin' | 'biege' | 'flecha' | 'fde-mit' | 'fde-mss';

export type RunStatus = 'prepared' | 'running' | 'success' | 'warning' | 'failed' | 'cancelled';

export interface RunArtifact {
  kind: 'input' | 'output' | 'log' | 'plot' | 'report' | 'manifest' | 'auxiliary';
  relativePath: string;
  sha256?: string;
  sizeBytes?: number;
  modifiedAt?: string;
}

export interface RunJobRecord {
  analysis: string;
  qualificationId: string;
  flags: string[];
  inputSha256: string;
  stdoutSha256?: string;
  stderrSha256?: string;
  parsedResultPath?: string;
  parsedResultSha256?: string;
  exitCode?: number | null;
  status: RunStatus;
  sections: string[];
}

export interface RunManifest {
  schemaVersion: 1;
  runId: string;
  projectId: string;
  projectReference: string;
  solver: {
    kind: SolverKind;
    executablePath?: string;
    executableHash?: string;
    version?: string;
  };
  startedAt: string;
  finishedAt?: string;
  projectHash?: string;
  inputHash?: string;
  exitCode?: number;
  status: RunStatus;
  warnings: string[];
  artifacts: RunArtifact[];
  qualifications?: Record<string, string>;
  jobs?: RunJobRecord[];
}

export interface RunWorkspacePlan {
  rootRelativePath: string;
  inputDir: string;
  solverDir: string;
  resultDir: string;
  logDir: string;
  tempDir: string;
  manifestPath: string;
}

const safe = (value: string) => value.replace(/[^A-Za-z0-9._-]+/g, '_').replace(/^_+|_+$/g, '') || 'project';

export function createRunId(reference: string, timestamp: Date, sequence: number): string {
  const pad = (value: number, width = 2) => String(value).padStart(width, '0');
  const stamp = [
    timestamp.getUTCFullYear(),
    pad(timestamp.getUTCMonth() + 1),
    pad(timestamp.getUTCDate()),
    '-',
    pad(timestamp.getUTCHours()),
    pad(timestamp.getUTCMinutes()),
    pad(timestamp.getUTCSeconds()),
  ].join('');
  return `${safe(reference)}-${stamp}-${pad(sequence, 3)}`;
}

export function planRunWorkspace(runId: string): RunWorkspacePlan {
  const root = safe(runId);
  return {
    rootRelativePath: root,
    inputDir: `${root}/01_Input`,
    solverDir: `${root}/02_Solvers`,
    resultDir: `${root}/03_Results`,
    logDir: `${root}/04_Logs`,
    tempDir: `${root}/05_Temp`,
    manifestPath: `${root}/run_manifest.json`,
  };
}

export function createRunManifestDraft(args: {
  runId: string;
  projectId: string;
  projectReference: string;
  solver: SolverKind;
  startedAt: string;
}): RunManifest {
  return {
    schemaVersion: 1,
    runId: args.runId,
    projectId: args.projectId,
    projectReference: args.projectReference,
    solver: { kind: args.solver },
    startedAt: args.startedAt,
    status: 'prepared',
    warnings: [],
    artifacts: [],
  };
}

export function artifactIsFresh(args: {
  runStartedAtMs: number;
  artifactModifiedAtMs: number;
  toleranceMs?: number;
}): boolean {
  const tolerance = args.toleranceMs ?? 1000;
  return args.artifactModifiedAtMs + tolerance >= args.runStartedAtMs;
}

export function requireFreshArtifact(args: {
  runStartedAtMs: number;
  artifactModifiedAtMs: number;
  artifactPath: string;
  toleranceMs?: number;
}): void {
  if (!artifactIsFresh(args)) {
    throw new Error(
      `STALE-RUN-OUTPUT: ${args.artifactPath} predates the current run and must not be parsed as a new solver result.`,
    );
  }
}
