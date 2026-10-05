import type {
  LegacyMassLoad,
  RotorProject,
  ShaftSegment,
  ValidationIssue,
  ValidationResult,
} from './model';

const push = (issues: ValidationIssue[], issue: ValidationIssue) => issues.push(issue);
const POSITION_EPS_MM = 1e-9;

export function segmentEndPositions(segments: ShaftSegment[]): Array<{ id: string; startMm: number; endMm: number }> {
  let cursor = 0;
  return segments.map((segment) => {
    const item = { id: segment.id, startMm: cursor, endMm: cursor + segment.lengthMm };
    cursor = item.endMm;
    return item;
  });
}

export function validateProject(project: RotorProject): ValidationResult {
  const issues: ValidationIssue[] = [];

  project.segments.forEach((segment) => {
    if (!(segment.lengthMm > 0)) {
      push(issues, { code: 'SHAFT-LENGTH-001', severity: 'error', entityId: segment.id, message: 'Segment length must be greater than zero.' });
    }
    if (!(segment.outerDiameterMm > 0)) {
      push(issues, { code: 'SHAFT-DIAM-001', severity: 'error', entityId: segment.id, message: 'Outer diameter must be greater than zero.' });
    }
    if (segment.endOuterDiameterMm != null && !(segment.endOuterDiameterMm > 0)) {
      push(issues, { code: 'SHAFT-TAPER-001', severity: 'error', entityId: segment.id, message: 'Taper end diameter must be greater than zero.' });
    }

    if (segment.innerDiameterMm != null && segment.innerDiameterMm !== 0) {
      const dInt = segment.innerDiameterMm;
      if (dInt <= 0 || dInt >= segment.outerDiameterMm) {
        push(issues, { code: 'SHAFT-HOLLOW-001', severity: 'error', entityId: segment.id, message: 'Inner diameter must satisfy 0 < Dint < D.' });
      }
    }

    if (segment.ribbed) {
      const rib = segment.ribbed;
      if (!rib) {
        push(issues, { code: 'SHAFT-RIB-001', severity: 'error', entityId: segment.id, message: 'Ribbed segment is missing rib geometry.' });
      } else {
        if (rib.packageDiameterMm < segment.outerDiameterMm) {
          push(issues, { code: 'SHAFT-RIB-002', severity: 'error', entityId: segment.id, message: 'Package diameter cannot be smaller than the shaft diameter.' });
        }
        if (!(rib.ribCount > 0)) {
          push(issues, { code: 'SHAFT-RIB-003', severity: 'error', entityId: segment.id, message: 'Ribbed segment must define at least one rib.' });
        }
      }
    }
  });

  const positions = segmentEndPositions(project.segments);

  project.legacyLoads.forEach((load) => {
    if (load.startMm < 0 || load.lengthMm < 0) {
      push(issues, { code: 'LOAD-RANGE-001', severity: 'error', entityId: load.id, message: 'Load start and length must be non-negative.' });
    }
    if (load.massKg < 0) {
      push(issues, { code: 'LOAD-MASS-001', severity: 'error', entityId: load.id, message: 'Load/mass value must be non-negative.' });
    }

    const end = load.startMm + load.lengthMm;
    const contained = positions.some((segment) => load.startMm >= segment.startMm - POSITION_EPS_MM && end <= segment.endMm + POSITION_EPS_MM);
    if (!contained) {
      push(issues, {
        code: 'LEGACY-LOAD-STEP-001',
        severity: 'error',
        entityId: load.id,
        legacySource: 'frmMain ultrapassaescalonamento',
        message: 'Legacy IBIEGE requires each load region to fit entirely inside one shaft segment.',
      });
    }
  });

  for (let i = 0; i < project.legacyLoads.length; i += 1) {
    for (let j = i + 1; j < project.legacyLoads.length; j += 1) {
      const a = project.legacyLoads[i];
      const b = project.legacyLoads[j];
      if (loadsOverlapInclusive(a, b)) {
        push(issues, {
          code: 'LOAD-OVERLAP-001',
          severity: 'error',
          entityId: a.id,
          legacySource: 'frmMain sobrepoecarga',
          message: `Load regions ${a.id} and ${b.id} overlap or touch. The React validator also catches the containment case missed by the VB6 endpoint-only test.`,
        });
      }
    }
  }

  project.bearings.forEach((bearing) => {
    if (bearing.positionMm < 0) {
      push(issues, { code: 'BRG-POS-001', severity: 'error', entityId: bearing.id, message: 'Bearing position cannot be negative.' });
    }
    if (bearing.coefficients.kind === 'legacy-scalar' && bearing.coefficients.stiffnessNPerM < 0) {
      push(issues, { code: 'BRG-K-001', severity: 'error', entityId: bearing.id, message: 'Legacy scalar bearing stiffness cannot be negative.' });
    }
  });

  return {
    valid: !issues.some((issue) => issue.severity === 'error'),
    issues,
  };
}

export function loadsOverlapInclusive(a: LegacyMassLoad, b: LegacyMassLoad): boolean {
  const aEnd = a.startMm + a.lengthMm;
  const bEnd = b.startMm + b.lengthMm;
  return a.startMm <= bEnd && b.startMm <= aEnd;
}
