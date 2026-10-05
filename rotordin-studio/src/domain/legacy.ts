import type {
  Bearing,
  LegacyImportResult,
  LegacyMassLoad,
  Matrix2x2,
  RotorProject,
  ShaftSegment,
  ValidationIssue,
} from './model';

export interface LegacyShaftGridRow {
  L: number | string | null | undefined;
  D: number | string | null | undefined;
  DPCT?: number | string | null;
  A?: number | string | null;
  B?: number | string | null;
  C?: number | string | null;
  NR_COST?: number | string | null;
  D_INT?: number | string | null;
  D_F?: number | string | null;
}

export interface LegacyLoadGridRow {
  Xi: number | string | null | undefined;
  LC: number | string | null | undefined;
  KG: number | string | null | undefined;
  Empuxo?: number | string | boolean | null;
}

const n = (value: unknown): number => {
  if (typeof value === 'number') return Number.isFinite(value) ? value : 0;
  if (typeof value === 'boolean') return value ? 1 : 0;
  if (value == null || value === '') return 0;
  const parsed = Number(String(value).trim().replace(',', '.'));
  return Number.isFinite(parsed) ? parsed : 0;
};

const positive = (value: unknown) => n(value) > 0;

export function importLegacyShaftGrid(rows: LegacyShaftGridRow[]): LegacyImportResult<ShaftSegment[]> {
  const issues: ValidationIssue[] = [];

  const segments = rows.map((row, index): ShaftSegment => {
    const id = `segment-${index + 1}`;
    const hasRibbedData = [row.DPCT, row.A, row.B, row.C, row.NR_COST].some(positive);
    const hasHollowData = positive(row.D_INT);

    if (hasRibbedData && hasHollowData) {
      issues.push({
        code: 'LEGACY-SHAFT-001',
        severity: 'error',
        entityId: id,
        legacySource: 'frmMain IncSgmnt/AltSgmnt',
        message: 'Legacy model contains ribbed and hollow data in the same segment; VB6 intends these representations to be mutually exclusive.',
      });
    }

    const outerDiameterMm = n(row.D);
    const end = n(row.D_F);

    const segment: ShaftSegment = {
      id,
      sectionType: hasRibbedData ? 'ribbed' : hasHollowData ? 'hollow' : 'solid',
      lengthMm: n(row.L),
      outerDiameterMm,
      ...(end > 0 && Math.abs(end - outerDiameterMm) > 1e-12 ? { endOuterDiameterMm: end } : {}),
    };

    if (hasHollowData) {
      segment.innerDiameterMm = n(row.D_INT);
    }

    if (hasRibbedData) {
      segment.ribbed = {
        packageDiameterMm: n(row.DPCT),
        ribThicknessMm: n(row.A),
        hammerThicknessMm: n(row.B),
        hammerWidthMm: n(row.C),
        ribCount: n(row.NR_COST),
      };
    }

    return segment;
  });

  return { value: segments, issues };
}

export function toLegacyShaftGrid(segments: ShaftSegment[]): LegacyShaftGridRow[] {
  return segments.map((segment) => ({
    L: segment.lengthMm,
    D: segment.outerDiameterMm,
    DPCT: segment.ribbed?.packageDiameterMm ?? 0,
    A: segment.ribbed?.ribThicknessMm ?? 0,
    B: segment.ribbed?.hammerThicknessMm ?? 0,
    C: segment.ribbed?.hammerWidthMm ?? 0,
    NR_COST: segment.ribbed?.ribCount ?? 0,
    D_INT: segment.innerDiameterMm ?? 0,
    D_F: segment.endOuterDiameterMm ?? 0,
  }));
}

export function importLegacyLoadGrid(
  rows: LegacyLoadGridRow[],
  rotorStackRowIndex?: number,
): LegacyImportResult<LegacyMassLoad[]> {
  const issues: ValidationIssue[] = [];

  const value = rows.map((row, index): LegacyMassLoad => ({
    id: `legacy-load-${index + 1}`,
    startMm: n(row.Xi),
    lengthMm: n(row.LC),
    massKg: n(row.KG),
    magneticThrust: n(row.Empuxo) !== 0,
    isRotorStack: rotorStackRowIndex === index,
  }));

  if (rotorStackRowIndex == null) {
    issues.push({
      code: 'LEGACY-FLEXA-001',
      severity: 'warning',
      legacySource: 'frmFlexa LdPc',
      message: 'Rotor stack selection is not persisted by the legacy database; VB6 derives it from MSFlexGrid CellBackColor.',
    });
  }

  return { value, issues };
}

export function toLegacyLoadGrid(loads: LegacyMassLoad[]): LegacyLoadGridRow[] {
  return loads.map((load) => ({
    Xi: load.startMm,
    LC: load.lengthMm,
    KG: load.massKg,
    Empuxo: load.magneticThrust ? 1 : 0,
  }));
}

export function importLegacyBearing(
  index: number,
  positionMm: number,
  scalarStiffnessNPerM: number,
): Bearing {
  return {
    id: `bearing-${index + 1}`,
    name: `BRG ${index + 1}`,
    positionMm,
    coefficients: {
      kind: 'legacy-scalar',
      stiffnessNPerM: scalarStiffnessNPerM,
      infinite: scalarStiffnessNPerM === 0,
    },
  };
}

export function matrixViewForBearing(bearing: Bearing): { stiffness: Matrix2x2; damping: Matrix2x2 } {
  if (bearing.coefficients.kind === 'matrix') {
    return {
      stiffness: bearing.coefficients.stiffness,
      damping: bearing.coefficients.damping,
    };
  }

  const k = bearing.coefficients.infinite ? 0 : bearing.coefficients.stiffnessNPerM;
  return {
    stiffness: { xx: k, xz: 0, zx: 0, zz: k },
    damping: { xx: 0, xz: 0, zx: 0, zz: 0 },
  };
}

export function legacyCompatibilityNotes(project: RotorProject): ValidationIssue[] {
  const issues: ValidationIssue[] = [];
  if (project.bearings.length > 2) {
    issues.push({
      code: 'LEGACY-BRG-001',
      severity: 'error',
      legacySource: 'frmMain exporta_biege / frmFlexa WrFl',
      message: 'Legacy BIEGE/FLECHA paths are defined for two bearings; additional bearings require the RotorDin adapter rather than the legacy adapter.',
    });
  }

  if (project.legacyLoads.filter((load) => load.isRotorStack).length !== 1) {
    issues.push({
      code: 'LEGACY-FLEXA-002',
      severity: 'warning',
      legacySource: 'frmFlexa Form_Activate/LdPc',
      message: 'FLECHA requires exactly one load region selected as the rotor stack.',
    });
  }

  return issues;
}
