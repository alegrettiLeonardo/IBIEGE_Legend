import type { Bearing, LegacyMassLoad, RotorProject, ShaftSegment, ValidationIssue } from './model';
import { validateProject } from './validation';

export interface EditResult {
  accepted: boolean;
  project: RotorProject;
  issues: ValidationIssue[];
}

export type SegmentPatch = Partial<Omit<ShaftSegment, 'id' | 'ribbed'>> & {
  ribbed?: Partial<NonNullable<ShaftSegment['ribbed']>>;
};

function evaluate(original: RotorProject, candidate: RotorProject): EditResult {
  const validation = validateProject(candidate);
  if (!validation.valid) {
    return { accepted: false, project: original, issues: validation.issues };
  }
  return { accepted: true, project: candidate, issues: validation.issues };
}

export function updateSegmentAtomic(project: RotorProject, id: string, patch: SegmentPatch): EditResult {
  const current = project.segments.find((segment) => segment.id === id);
  if (!current) {
    return {
      accepted: false,
      project,
      issues: [{ code: 'EDIT-SEGMENT-404', severity: 'error', entityId: id, message: 'Segment not found.' }],
    };
  }

  const updated: ShaftSegment = {
    ...current,
    ...patch,
    ...(patch.ribbed
      ? {
          ribbed: {
            ...(current.ribbed ?? {
              packageDiameterMm: current.outerDiameterMm,
              ribThicknessMm: 0,
              hammerThicknessMm: 0,
              hammerWidthMm: 0,
              ribCount: 1,
            }),
            ...patch.ribbed,
          },
        }
      : {}),
  };

  const candidate: RotorProject = {
    ...project,
    segments: project.segments.map((segment) => segment.id === id ? updated : segment),
  };

  return evaluate(project, candidate);
}

export function addSegmentAtomic(project: RotorProject, afterIndex?: number): EditResult {
  const index = afterIndex == null ? project.segments.length : Math.max(0, Math.min(afterIndex + 1, project.segments.length));
  const newSegment: ShaftSegment = {
    id: `segment-${Date.now()}-${project.segments.length + 1}`,
    sectionType: 'solid',
    lengthMm: 100,
    outerDiameterMm: project.segments[Math.max(0, index - 1)]?.outerDiameterMm ?? 100,
  };
  const segments = [...project.segments];
  segments.splice(index, 0, newSegment);
  return evaluate(project, { ...project, segments });
}

export function removeSegmentAtomic(project: RotorProject, id: string): EditResult {
  if (project.segments.length <= 1) {
    return {
      accepted: false,
      project,
      issues: [{ code: 'EDIT-SEGMENT-MIN', severity: 'error', entityId: id, message: 'The shaft must contain at least one segment.' }],
    };
  }
  return evaluate(project, { ...project, segments: project.segments.filter((segment) => segment.id !== id) });
}

export function moveSegmentAtomic(project: RotorProject, id: string, direction: -1 | 1): EditResult {
  const from = project.segments.findIndex((segment) => segment.id === id);
  if (from < 0) return { accepted: false, project, issues: [{ code: 'EDIT-SEGMENT-404', severity: 'error', entityId: id, message: 'Segment not found.' }] };
  const to = from + direction;
  if (to < 0 || to >= project.segments.length) return { accepted: true, project, issues: [] };
  const segments = [...project.segments];
  const [item] = segments.splice(from, 1);
  segments.splice(to, 0, item);
  return evaluate(project, { ...project, segments });
}


export type BearingPatch = Partial<Omit<Bearing, 'id' | 'coefficients'>> & {
  coefficients?:
    | Bearing['coefficients']
    | {
        kind: 'legacy-scalar';
        stiffnessNPerM?: number;
        infinite?: boolean;
      };
};

export function updateBearingAtomic(project: RotorProject, id: string, patch: BearingPatch): EditResult {
  const current = project.bearings.find((bearing) => bearing.id === id);
  if (!current) {
    return {
      accepted: false,
      project,
      issues: [{ code: 'EDIT-BEARING-404', severity: 'error', entityId: id, message: 'Bearing not found.' }],
    };
  }

  let coefficients = current.coefficients;
  if (patch.coefficients) {
    if (patch.coefficients.kind === 'legacy-scalar') {
      const existing = current.coefficients.kind === 'legacy-scalar'
        ? current.coefficients
        : { kind: 'legacy-scalar' as const, stiffnessNPerM: 0, infinite: false };
      coefficients = {
        ...existing,
        ...patch.coefficients,
        kind: 'legacy-scalar',
      };
    } else {
      coefficients = patch.coefficients;
    }
  }

  const updated: Bearing = {
    ...current,
    ...patch,
    coefficients,
  };

  const candidate = {
    ...project,
    bearings: project.bearings.map((bearing) => bearing.id === id ? updated : bearing),
  };

  return evaluate(project, candidate);
}

export type LegacyLoadPatch = Partial<Omit<LegacyMassLoad, 'id'>>;

export function updateLegacyLoadAtomic(project: RotorProject, id: string, patch: LegacyLoadPatch): EditResult {
  const current = project.legacyLoads.find((load) => load.id === id);
  if (!current) {
    return {
      accepted: false,
      project,
      issues: [{ code: 'EDIT-LOAD-404', severity: 'error', entityId: id, message: 'Load/mass row not found.' }],
    };
  }

  const candidate = {
    ...project,
    legacyLoads: project.legacyLoads.map((load) => load.id === id ? { ...load, ...patch } : load),
  };

  return evaluate(project, candidate);
}

export function setRotorStackAtomic(project: RotorProject, id: string): EditResult {
  if (!project.legacyLoads.some((load) => load.id === id)) {
    return {
      accepted: false,
      project,
      issues: [{ code: 'EDIT-ROTOR-404', severity: 'error', entityId: id, message: 'Rotor stack load/mass row not found.' }],
    };
  }

  const candidate = {
    ...project,
    legacyLoads: project.legacyLoads.map((load) => ({
      ...load,
      isRotorStack: load.id === id,
    })),
  };

  return evaluate(project, candidate);
}
