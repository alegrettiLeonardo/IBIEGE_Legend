import {
  createContext,
  useContext,
  useEffect,
  useMemo,
  useState,
  type ReactNode,
} from 'react';
import { demoProject } from '../data/demoProject';
import type { ExecuteQualifiedRunResponse, InspectRunResponse, PrepareRunResponse, QualifiedRunSummary, ReadRunArtifactResponse } from '../../electron/contracts';
import type { SolverKind } from '../runs/model';
import {
  addSegmentAtomic,
  moveSegmentAtomic,
  removeSegmentAtomic,
  setRotorStackAtomic,
  updateBearingAtomic,
  updateLegacyLoadAtomic,
  updateSegmentAtomic,
  validateProject,
  type AnalysisKind,
  type BearingPatch,
  type LegacyLoadPatch,
  type RotorProject,
  type SegmentPatch,
  type ValidationIssue,
} from '../domain';

interface ProjectContextValue {
  project: RotorProject;
  saved: boolean;
  validation: ReturnType<typeof validateProject>;
  editIssues: ValidationIssue[];
  projectFilePath?: string;
  selectedRun?: ExecuteQualifiedRunResponse;
  runHistory: QualifiedRunSummary[];
  markSaved: () => void;
  markDirty: () => void;
  saveProject: () => Promise<boolean>;
  openProject: () => Promise<boolean>;
  prepareRun: (solver: SolverKind) => Promise<PrepareRunResponse | undefined>;
  executeQualifiedRun: (analyses: AnalysisKind[]) => Promise<ExecuteQualifiedRunResponse | undefined>;
  refreshRunHistory: () => Promise<void>;
  selectQualifiedRun: (runId: string) => Promise<boolean>;
  loadQualifiedRunData: (runId: string) => Promise<ExecuteQualifiedRunResponse | undefined>;
  inspectRun: (runId: string) => Promise<InspectRunResponse | undefined>;
  readRunArtifact: (runId: string, relativePath: string) => Promise<ReadRunArtifactResponse | undefined>;
  revealRunArtifact: (runId: string, relativePath: string) => Promise<boolean>;
  openRunWorkspace: (runId: string) => Promise<boolean>;
  updateSegment: (id: string, patch: SegmentPatch) => boolean;
  addSegment: (afterIndex?: number) => boolean;
  removeSegment: (id: string) => boolean;
  moveSegment: (id: string, direction: -1 | 1) => boolean;
  updateBearing: (id: string, patch: BearingPatch) => boolean;
  updateLegacyLoad: (id: string, patch: LegacyLoadPatch) => boolean;
  setRotorStack: (id: string) => boolean;
}

const ProjectContext = createContext<ProjectContextValue | null>(null);

export function ProjectProvider({ children }: { children: ReactNode }) {
  const [project, setProject] = useState<RotorProject>(() => structuredClone(demoProject));
  const [saved, setSaved] = useState(true);
  const [editIssues, setEditIssues] = useState<ValidationIssue[]>([]);
  const [projectFilePath, setProjectFilePath] = useState<string>();
  const [selectedRun, setSelectedRun] = useState<ExecuteQualifiedRunResponse>();
  const [runHistory, setRunHistory] = useState<QualifiedRunSummary[]>([]);

  const validation = useMemo(() => validateProject(project), [project]);

  const apply = (result: ReturnType<typeof updateSegmentAtomic>): boolean => {
    setEditIssues(result.issues);
    if (!result.accepted) return false;
    if (result.project !== project) {
      setProject(result.project);
      setSaved(false);
    }
    return true;
  };

  const desktopIssue = (code: string, error: unknown) => {
    setEditIssues([{
      code,
      severity: 'error',
      message: error instanceof Error ? error.message : String(error),
    }]);
  };

  const saveProject = async (): Promise<boolean> => {
    if (!window.rotorDinDesktop) {
      // Browser/Vite development mode has no privileged filesystem access.
      setSaved(true);
      return true;
    }

    try {
      const result = await window.rotorDinDesktop.saveProject({
        project,
        filePath: projectFilePath,
      });
      if (result.cancelled) return false;
      setProjectFilePath(result.filePath);
      setSaved(true);
      setEditIssues([]);
      return true;
    } catch (error) {
      desktopIssue('PROJECT-SAVE-001', error);
      return false;
    }
  };

  const openProject = async (): Promise<boolean> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('PROJECT-OPEN-DESKTOP', 'Opening project files requires the Electron desktop shell.');
      return false;
    }

    if (!saved && !window.confirm('This project has unsaved changes. Open another project and discard them?')) {
      return false;
    }

    try {
      const result = await window.rotorDinDesktop.openProject();
      if (result.cancelled || !result.project) return false;
      setProject(result.project);
      setProjectFilePath(result.sourceFormat === 'native-json' ? result.filePath : undefined);
      setSaved(result.sourceFormat === 'native-json');
      setEditIssues(result.issues ?? []);
      setSelectedRun(undefined);
      return true;
    } catch (error) {
      desktopIssue('PROJECT-OPEN-001', error);
      return false;
    }
  };

  const prepareRun = async (solver: SolverKind): Promise<PrepareRunResponse | undefined> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-PREPARE-DESKTOP', 'Run preparation requires the Electron desktop shell.');
      return undefined;
    }

    const currentValidation = validateProject(project);
    if (!currentValidation.valid) {
      setEditIssues(currentValidation.issues);
      return undefined;
    }

    try {
      const result = await window.rotorDinDesktop.prepareRun({ project, solver });
      setEditIssues([]);
      return result;
    } catch (error) {
      desktopIssue('RUN-PREPARE-001', error);
      return undefined;
    }
  };

  const executeQualifiedRun = async (
    analyses: AnalysisKind[],
  ): Promise<ExecuteQualifiedRunResponse | undefined> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-EXEC-DESKTOP', 'Qualified solver execution requires the Electron desktop shell.');
      return undefined;
    }

    const currentValidation = validateProject(project);
    if (!currentValidation.valid) {
      setEditIssues(currentValidation.issues);
      return undefined;
    }

    try {
      const result = await window.rotorDinDesktop.executeQualifiedRun({ project, analyses });
      setEditIssues([]);
      setSelectedRun(result);
      void refreshRunHistory();
      return result;
    } catch (error) {
      desktopIssue('RUN-EXEC-001', error);
      return undefined;
    }
  };

  const refreshRunHistory = async (): Promise<void> => {
    if (!window.rotorDinDesktop) {
      setRunHistory([]);
      return;
    }
    try {
      const rows = await window.rotorDinDesktop.listQualifiedRuns();
      setRunHistory(rows);
      const latestWithResults = rows.find((row) => row.hasResults);
      if (!selectedRun && latestWithResults) {
        const latest = await window.rotorDinDesktop.loadQualifiedRun({ runId: latestWithResults.runId });
        setSelectedRun(latest);
      }
    } catch (error) {
      desktopIssue('RUN-HISTORY-LIST-001', error);
    }
  };

  const selectQualifiedRun = async (runId: string): Promise<boolean> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-HISTORY-DESKTOP', 'Run history selection requires the Electron desktop shell.');
      return false;
    }
    try {
      const run = await window.rotorDinDesktop.loadQualifiedRun({ runId });
      setSelectedRun(run);
      setEditIssues([]);
      return true;
    } catch (error) {
      desktopIssue('RUN-HISTORY-LOAD-001', error);
      return false;
    }
  };

  useEffect(() => {
    void refreshRunHistory();
  }, []);

  const loadQualifiedRunData = async (
    runId: string,
  ): Promise<ExecuteQualifiedRunResponse | undefined> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-HISTORY-DESKTOP', 'Run history access requires the Electron desktop shell.');
      return undefined;
    }
    try {
      return await window.rotorDinDesktop.loadQualifiedRun({ runId });
    } catch (error) {
      desktopIssue('RUN-HISTORY-LOAD-001', error);
      return undefined;
    }
  };

  const inspectRun = async (runId: string): Promise<InspectRunResponse | undefined> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-HISTORY-DESKTOP', 'Run inspection requires the Electron desktop shell.');
      return undefined;
    }
    try {
      return await window.rotorDinDesktop.inspectRun({ runId });
    } catch (error) {
      desktopIssue('RUN-HISTORY-INSPECT-001', error);
      return undefined;
    }
  };

  const readRunArtifact = async (
    runId: string,
    relativePath: string,
  ): Promise<ReadRunArtifactResponse | undefined> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-HISTORY-DESKTOP', 'Artifact preview requires the Electron desktop shell.');
      return undefined;
    }
    try {
      return await window.rotorDinDesktop.readRunArtifact({ runId, relativePath });
    } catch (error) {
      desktopIssue('RUN-HISTORY-ARTIFACT-001', error);
      return undefined;
    }
  };

  const revealRunArtifact = async (runId: string, relativePath: string): Promise<boolean> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-HISTORY-DESKTOP', 'Artifact reveal requires the Electron desktop shell.');
      return false;
    }
    try {
      await window.rotorDinDesktop.revealRunArtifact({ runId, relativePath });
      return true;
    } catch (error) {
      desktopIssue('RUN-HISTORY-REVEAL-001', error);
      return false;
    }
  };

  const openRunWorkspace = async (runId: string): Promise<boolean> => {
    if (!window.rotorDinDesktop) {
      desktopIssue('RUN-HISTORY-DESKTOP', 'Opening a run workspace requires the Electron desktop shell.');
      return false;
    }
    try {
      await window.rotorDinDesktop.openRunWorkspace({ runId });
      return true;
    } catch (error) {
      desktopIssue('RUN-HISTORY-WORKSPACE-001', error);
      return false;
    }
  };

  const value: ProjectContextValue = {
    project,
    saved,
    validation,
    editIssues,
    projectFilePath,
    selectedRun,
    runHistory,
    markSaved: () => setSaved(true),
    markDirty: () => setSaved(false),
    saveProject,
    openProject,
    prepareRun,
    executeQualifiedRun,
    refreshRunHistory,
    selectQualifiedRun,
    loadQualifiedRunData,
    inspectRun,
    readRunArtifact,
    revealRunArtifact,
    openRunWorkspace,
    updateSegment: (id, patch) => apply(updateSegmentAtomic(project, id, patch)),
    addSegment: (afterIndex) => apply(addSegmentAtomic(project, afterIndex)),
    removeSegment: (id) => apply(removeSegmentAtomic(project, id)),
    moveSegment: (id, direction) => apply(moveSegmentAtomic(project, id, direction)),
    updateBearing: (id, patch) => apply(updateBearingAtomic(project, id, patch)),
    updateLegacyLoad: (id, patch) => apply(updateLegacyLoadAtomic(project, id, patch)),
    setRotorStack: (id) => apply(setRotorStackAtomic(project, id)),
  };

  return <ProjectContext.Provider value={value}>{children}</ProjectContext.Provider>;
}

export function useProject(): ProjectContextValue {
  const value = useContext(ProjectContext);
  if (!value) throw new Error('useProject must be used inside ProjectProvider');
  return value;
}
