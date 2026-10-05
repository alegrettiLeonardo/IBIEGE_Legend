import {
  createContext,
  useContext,
  useMemo,
  useState,
  type ReactNode,
} from 'react';
import { demoProject } from '../data/demoProject';
import {
  addSegmentAtomic,
  moveSegmentAtomic,
  removeSegmentAtomic,
  setRotorStackAtomic,
  updateBearingAtomic,
  updateLegacyLoadAtomic,
  updateSegmentAtomic,
  validateProject,
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
  markSaved: () => void;
  markDirty: () => void;
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

  const value: ProjectContextValue = {
    project,
    saved,
    validation,
    editIssues,
    markSaved: () => setSaved(true),
    markDirty: () => setSaved(false),
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
