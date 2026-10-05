import {
  importLegacyBearing,
  importLegacyLoadGrid,
  importLegacyShaftGrid,
  toLegacyLoadGrid,
  toLegacyShaftGrid,
  type LegacyLoadGridRow,
  type LegacyShaftGridRow,
  type LegacyImportResult,
  type RotorProject,
  type ValidationIssue,
} from '../domain';

export interface LegacyCalculationHeader {
  id: number;
  reference: string;
  description: string;
  user: string;
  updatedAt: string;
}

export interface LegacyCharacteristic {
  calculationId: number;
  line: number;
  name: string;
  value: number;
}

export interface LegacyEncodedCalculation {
  header: LegacyCalculationHeader;
  characteristics: LegacyCharacteristic[];
  issues: ValidationIssue[];
}

const shaftNames = ['L', 'D', 'DPCT', 'A', 'B', 'C', 'NR_COST', 'D_INT', 'D_F'] as const;
const loadNames = ['Xi', 'LC', 'KG', 'Empuxo'] as const;

const byLine = (rows: LegacyCharacteristic[], names: readonly string[]) => {
  const accepted = new Set(names);
  const map = new Map<number, Record<string, number>>();

  for (const row of rows) {
    if (row.line <= 0 || !accepted.has(row.name)) continue;
    const current = map.get(row.line) ?? {};
    current[row.name] = row.value;
    map.set(row.line, current);
  }

  return [...map.entries()]
    .sort(([a], [b]) => a - b)
    .map(([, values]) => values);
};

const scalarBearing = (
  rows: LegacyCharacteristic[],
  positionLine: number,
  stiffnessLine: number,
  index: number,
) => {
  const position = rows.find((row) => row.line === positionLine && row.name.toLowerCase() === 'mancal')?.value;
  const stiffness = rows.find((row) => row.line === stiffnessLine && row.name.toLowerCase() === 'constante')?.value;
  if (position == null || stiffness == null) return undefined;
  return importLegacyBearing(index, position, stiffness);
};

export function decodeLegacyEav(
  header: LegacyCalculationHeader,
  characteristics: LegacyCharacteristic[],
): LegacyImportResult<RotorProject> {
  const shaftRaw = byLine(characteristics, shaftNames) as unknown as LegacyShaftGridRow[];
  const loadRaw = byLine(characteristics, loadNames) as unknown as LegacyLoadGridRow[];

  const shaft = importLegacyShaftGrid(shaftRaw);
  const loads = importLegacyLoadGrid(loadRaw);
  const bearings = [
    scalarBearing(characteristics, -1, -2, 0),
    scalarBearing(characteristics, -3, -4, 1),
  ].filter((item): item is NonNullable<typeof item> => item != null);

  const issues: ValidationIssue[] = [
    ...shaft.issues,
    ...loads.issues,
  ];

  issues.push({
    code: 'LEGACY-DB-ROTOR-STACK',
    severity: 'warning',
    legacySource: 'd_carbiege / frmFlexa LdPc',
    message: 'Legacy EAV persistence has no rotor-stack characteristic; React requires explicit user confirmation after legacy import before FLECHA execution.',
  });

  return {
    value: {
      id: `legacy-${header.id}`,
      reference: header.reference,
      description: header.description,
      user: header.user,
      segments: shaft.value,
      bearings,
      legacyLoads: loads.value,
      concentratedMasses: [],
      forces: [],
      responsePoints: [],
      supports: [],
      analyses: [],
      generateBiegePlot: true,
    },
    issues,
  };
}

const add = (
  target: LegacyCharacteristic[],
  calculationId: number,
  line: number,
  name: string,
  value: number | undefined,
) => {
  if (value == null) return;
  target.push({ calculationId, line, name, value });
};

function bearingScalar(project: RotorProject, index: number, issues: ValidationIssue[]): number | undefined {
  const bearing = project.bearings[index];
  if (!bearing) return undefined;
  if (bearing.coefficients.kind !== 'legacy-scalar') {
    issues.push({
      code: 'LEGACY-DB-BEARING-MATRIX',
      severity: 'error',
      entityId: bearing.id,
      legacySource: 'frmMain salva_mancal',
      message: 'd_carbiege legacy persistence can store only one scalar bearing stiffness.',
    });
    return undefined;
  }
  return bearing.coefficients.infinite ? 0 : bearing.coefficients.stiffnessNPerM;
}

export function encodeLegacyEav(
  project: RotorProject,
  calculationId: number,
  updatedAt: string,
): LegacyEncodedCalculation {
  const issues: ValidationIssue[] = [];
  const characteristics: LegacyCharacteristic[] = [];

  const shaft = toLegacyShaftGrid(project.segments);
  shaft.forEach((row, index) => {
    const line = index + 1;
    add(characteristics, calculationId, line, 'L', Number(row.L ?? 0));
    add(characteristics, calculationId, line, 'D', Number(row.D ?? 0));

    const segment = project.segments[index];
    if (segment.ribbed) {
      add(characteristics, calculationId, line, 'DPCT', Number(row.DPCT ?? 0));
      add(characteristics, calculationId, line, 'A', Number(row.A ?? 0));
      add(characteristics, calculationId, line, 'B', Number(row.B ?? 0));
      add(characteristics, calculationId, line, 'C', Number(row.C ?? 0));
      add(characteristics, calculationId, line, 'NR_COST', Number(row.NR_COST ?? 0));
    }
    if ((segment.innerDiameterMm ?? 0) > 0) {
      add(characteristics, calculationId, line, 'D_INT', Number(row.D_INT ?? 0));
    }
    if (segment.endOuterDiameterMm != null) {
      add(characteristics, calculationId, line, 'D_F', Number(row.D_F ?? 0));
    }
  });

  const loads = toLegacyLoadGrid(project.legacyLoads);
  loads.forEach((row, index) => {
    const line = index + 1;
    add(characteristics, calculationId, line, 'Xi', Number(row.Xi ?? 0));
    add(characteristics, calculationId, line, 'LC', Number(row.LC ?? 0));
    add(characteristics, calculationId, line, 'KG', Number(row.KG ?? 0));
    add(characteristics, calculationId, line, 'Empuxo', Number(row.Empuxo ?? 0));
  });

  if (project.legacyLoads.some((load) => load.isRotorStack)) {
    issues.push({
      code: 'LEGACY-DB-ROTOR-STACK-DROP',
      severity: 'warning',
      legacySource: 'frmMain Salva / SlvGrd',
      message: 'Rotor-stack identity cannot be represented in d_carbiege and will be lost in a pure legacy save.',
    });
  }

  const first = project.bearings[0];
  const second = project.bearings[1];
  if (first) {
    add(characteristics, calculationId, -1, 'mancal', first.positionMm);
    const stiffness = bearingScalar(project, 0, issues);
    if (stiffness != null) add(characteristics, calculationId, -2, 'constante', stiffness);
  }
  if (second) {
    add(characteristics, calculationId, -3, 'mancal', second.positionMm);
    const stiffness = bearingScalar(project, 1, issues);
    if (stiffness != null) add(characteristics, calculationId, -4, 'constante', stiffness);
  }

  for (const row of characteristics) {
    const scaled = Math.round(row.value * 100);
    if (Math.abs(row.value * 100 - scaled) > 1e-8) {
      issues.push({
        code: 'LEGACY-DB-PRECISION',
        severity: 'warning',
        legacySource: 'DOCS/d_ibiege.sql VL_CARACT NUMBER(12,2)',
        message: `${row.name} at line ${row.line} has more than two decimal places and may lose precision in the legacy database.`,
      });
    }
  }

  return {
    header: {
      id: calculationId,
      reference: project.reference,
      description: project.description,
      user: project.user ?? 'SEMUSU',
      updatedAt,
    },
    characteristics,
    issues,
  };
}

export interface ProjectRepository {
  list(): Promise<LegacyCalculationHeader[]>;
  load(id: number): Promise<LegacyImportResult<RotorProject>>;
  save(project: RotorProject): Promise<void>;
  delete(id: number): Promise<void>;
}
