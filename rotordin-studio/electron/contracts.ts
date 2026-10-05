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
}

export interface LoadQualifiedRunRequest {
  runId: string;
}

export interface RotorDinDesktopApi {
  saveProject(request: ProjectSaveRequest): Promise<ProjectSaveResponse>;
  openProject(): Promise<ProjectOpenResponse>;
  prepareRun(request: PrepareRunRequest): Promise<PrepareRunResponse>;
  executeQualifiedRun(request: ExecuteQualifiedRunRequest): Promise<ExecuteQualifiedRunResponse>;
  listQualifiedRuns(): Promise<QualifiedRunSummary[]>;
  loadQualifiedRun(request: LoadQualifiedRunRequest): Promise<ExecuteQualifiedRunResponse>;
}
