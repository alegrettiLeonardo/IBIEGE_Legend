import {
  createContext,
  useContext,
  useMemo,
  useState,
  type ReactNode,
} from 'react';
import { demoProject } from '../data/demoProject';
import type { ExecuteQualifiedRunResponse, PrepareRunResponse } from '../../electron/contracts';
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
  markSaved: () => void;
  markDirty: () => void;
  saveProject: () => Promise<boolean>;
  openProject: () => Promise<boolean>;
  prepareRun: (solver: SolverKind) => Promise<PrepareRunResponse | undefined>;
  executeQualifiedRun: (analyses: AnalysisKind[]) => Promise<ExecuteQualifiedRunResponse | undefined>;
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
      return result;
    } catch (error) {
      desktopIssue('RUN-EXEC-001', error);
      return undefined;
    }
  };

  const value: ProjectContextValue = {
    project,
    saved,
    validation,
    editIssues,
    projectFilePath,
    markSaved: () => setSaved(true),
    markDirty: () => setSaved(false),
    saveProject,
    openProject,
    prepareRun,
    executeQualifiedRun,
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
