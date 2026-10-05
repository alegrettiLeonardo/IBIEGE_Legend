import type { AnalysisKind, RotorProject, ValidationIssue } from '../src/domain';
import type { RotorDinParsedResults } from '../src/adapters/rotordinResultData';
import type { RunManifest, RunWorkspacePlan, SolverKind } from '../src/runs/model';

export const IPC_CHANNELS = {
  projectSave: 'project:save',
  projectOpen: 'project:open',
  runPrepare: 'run:prepare',
  runExecuteQualified: 'run:execute-qualified',
  runListQualified: 'run:list-qualified',
  runLoadQualified: 'run:load-qualified',
  runInspect: 'run:inspect',
  runReadArtifact: 'run:read-artifact',
  runRevealArtifact: 'run:reveal-artifact',
  runOpenWorkspace: 'run:open-workspace',
} as const;

export interface ProjectSaveRequest {
  project: RotorProject;
  filePath?: string;
}

export interface ProjectSaveResponse {
  cancelled: boolean;
  filePath?: string;
}

export interface ProjectOpenResponse {
  cancelled: boolean;
  filePath?: string;
  sourceFormat?: 'native-json' | 'irdin-text';
  project?: RotorProject;
  issues?: ValidationIssue[];
}

export interface PrepareRunRequest {
  project: RotorProject;
  solver: SolverKind;
}

export interface PrepareRunResponse {
  workspaceAbsolutePath: string;
  workspace: RunWorkspacePlan;
  manifest: RunManifest;
}

export interface ExecuteQualifiedRunRequest {
  project: RotorProject;
  analyses: AnalysisKind[];
}

export interface QualifiedRunJobResponse {
  analysis: AnalysisKind;
  qualificationId: string;
  flags: string[];
  sectionNames: string[];
  criticalSpeeds: Array<{ rpm: number; hz: number; harmonic: string }>;
  parsedResultPath: string;
  results: RotorDinParsedResults;
}

export interface ExecuteQualifiedRunResponse {
  workspaceAbsolutePath: string;
  manifest: RunManifest;
  jobs: QualifiedRunJobResponse[];
}

export interface QualifiedRunSummary {
  runId: string;
  projectReference: string;
  startedAt: string;
  finishedAt?: string;
  status: RunManifest['status'];
  analyses: AnalysisKind[];
  artifactCount: number;
  hasResults: boolean;
  solverHash?: string;
  inputHash?: string;
  elapsedMs?: number;
}

export interface LoadQualifiedRunRequest {
  runId: string;
}

export interface InspectRunRequest {
  runId: string;
}

export interface InspectRunResponse {
  workspaceAbsolutePath: string;
  manifest: RunManifest;
}

export interface ReadRunArtifactRequest {
  runId: string;
  relativePath: string;
  maxBytes?: number;
}

export interface ReadRunArtifactResponse {
  relativePath: string;
  text: string;
  bytes: number;
  truncated: boolean;
}

export interface RevealRunArtifactRequest {
  runId: string;
  relativePath: string;
}

export interface OpenRunWorkspaceRequest {
  runId: string;
}

export interface RotorDinDesktopApi {
  saveProject(request: ProjectSaveRequest): Promise<ProjectSaveResponse>;
  openProject(): Promise<ProjectOpenResponse>;
  prepareRun(request: PrepareRunRequest): Promise<PrepareRunResponse>;
  executeQualifiedRun(request: ExecuteQualifiedRunRequest): Promise<ExecuteQualifiedRunResponse>;
  listQualifiedRuns(): Promise<QualifiedRunSummary[]>;
  loadQualifiedRun(request: LoadQualifiedRunRequest): Promise<ExecuteQualifiedRunResponse>;
  inspectRun(request: InspectRunRequest): Promise<InspectRunResponse>;
  readRunArtifact(request: ReadRunArtifactRequest): Promise<ReadRunArtifactResponse>;
  revealRunArtifact(request: RevealRunArtifactRequest): Promise<void>;
  openRunWorkspace(request: OpenRunWorkspaceRequest): Promise<void>;
}
