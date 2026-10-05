import type {
  Bearing,
  BearingCoefficientSample,
  ConcentratedMass,
  ForceExcitation,
  LegacyMassLoad,
  ResponsePoint,
  RotorProject,
  RotorProjectSettings,
  ShaftSegment,
  Support,
  ValidationIssue,
} from '../domain';

export interface IrDinRawDocument {
  sections: Record<string, Record<string, string>>;
}

export interface IrDinImportResult {
  project: RotorProject;
  raw: IrDinRawDocument;
  issues: ValidationIssue[];
}

const numberValue = (value: string | undefined): number => {
  if (value == null || value.trim() === '') return 0;
  const parsed = Number(value.trim().replace(',', '.'));
  return Number.isFinite(parsed) ? parsed : 0;
};

const boolValue = (value: string | undefined): boolean => numberValue(value) !== 0;

export function decodeIrDinBytes(bytes: Uint8Array): string {
  // The supplied production sample is ISO-8859-1 and uses byte A7 as the
  // bearing-table row delimiter. windows-1252 is a safe superset for these
  // characters and is available in modern Chromium/Node TextDecoder.
  return new TextDecoder('windows-1252').decode(bytes);
}

export function parseIrDinDocument(text: string): IrDinRawDocument {
  const sections: Record<string, Record<string, string>> = {};
  let section = '';

  for (const rawLine of text.replace(/^\uFEFF/, '').split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line) continue;

    const heading = line.match(/^\[([^\]]+)\]$/);
    if (heading) {
      section = heading[1];
      sections[section] ??= {};
      continue;
    }

    const equals = line.indexOf('=');
    if (equals < 0 || !section) continue;
    const key = line.slice(0, equals).trim();
    const value = line.slice(equals + 1).trim();
    sections[section] ??= {};
    sections[section][key] = value;
  }

  return { sections };
}

interface IndexedSection {
  rows: Map<number, Map<number, string>>;
}

function indexedSection(raw: Record<string, string> | undefined): IndexedSection {
  const rows = new Map<number, Map<number, string>>();

  for (const [key, value] of Object.entries(raw ?? {})) {
    const match = key.match(/^(\d+),(\d+)$/);
    if (!match) continue;
    const row = Number(match[1]);
    const column = Number(match[2]);
    const current = rows.get(row) ?? new Map<number, string>();
    current.set(column, value);
    rows.set(row, current);
  }

  return { rows };
}

const sortedRows = (section: IndexedSection) =>
  [...section.rows.entries()].sort(([a], [b]) => a - b);

const col = (row: Map<number, string>, index: number) => row.get(index);

function parseBearingTable(raw: string | undefined, issues: ValidationIssue[], bearingId: string): BearingCoefficientSample[] {
  if (!raw) return [];
  const normalized = raw.startsWith('TABLE§') ? raw.slice('TABLE§'.length) : raw;
  if (!normalized) return [];

  const result: BearingCoefficientSample[] = [];

  for (const record of normalized.split('§')) {
    if (!record.trim()) continue;
    const fields = record.split('|');
    if (fields.length !== 9) {
      issues.push({
        code: 'IRDIN-BRG-TABLE-001',
        severity: 'warning',
        entityId: bearingId,
        legacySource: '[Mancais] column 11',
        message: `Bearing coefficient table row has ${fields.length} fields; expected speed plus 8 coefficients.`,
      });
      continue;
    }

    const values = fields.map(numberValue);
    result.push({
      speedRpm: values[0],
      // Native TABLE order is authoritative from entrada.f90/beatab:
      // rpm,kxx,kxz,kzx,kzz,cxx,cxz,czx,czz[,kph,kth].
      // Note that this differs from the legacy GUI grid column order.
      stiffness: {
        xx: values[1],
        xz: values[2],
        zx: values[3],
        zz: values[4],
      },
      damping: {
        xx: values[5],
        xz: values[6],
        zx: values[7],
        zz: values[8],
      },
    });
  }

  return result;
}

function projectSettings(dados: Record<string, string>): RotorProjectSettings {
  return {
    material: {
      youngsModulusPa: numberValue(dados.s_melast),
      densityKgPerM3: numberValue(dados.s_masesp),
      poissonRatio: numberValue(dados.s_poisson),
      diskDensityKgPerM3: numberValue(dados.d_masesp),
    },
    speed: {
      nominalRpm: numberValue(dados.nnom),
      initialRpm: numberValue(dados.nini),
      finalRpm: numberValue(dados.nfin),
    },
    discretization: {
      divisions: numberValue(dados.s_div),
      maxDivisions: numberValue(dados.s_mxdiv),
      relativeLoadLimit: numberValue(dados.s_relld),
      packageDivisions: numberValue(dados.p_div),
    },
    campbell: {
      initialRpm: numberValue(dados.c_rpmi),
      finalRpm: numberValue(dados.c_rpmf),
      divisions: numberValue(dados.c_div),
      rotations: numberValue(dados.c_nrrot),
      interpolationPoints: numberValue(dados.c_interp),
    },
    unbalanceResponse: {
      initialRpm: numberValue(dados.d_rpmi),
      finalRpm: numberValue(dados.d_rpmf),
      divisions: numberValue(dados.d_div),
      modes: numberValue(dados.d_nrmodos),
    },
    modes: {
      modes: numberValue(dados.m_nrmodos),
    },
    elasticLine: {
      inclinationDeg: numberValue(dados.le_inc),
      gravityMPerS2: numberValue(dados.le_g),
    },
    criticalSpeedMap: {
      initialStiffnessNPerM: numberValue(dados.mr_rini),
      divisions: numberValue(dados.mr_nrdc),
    },
    graphics: {
      scale: numberValue(dados.gr_esc),
      x0: numberValue(dados.gr_x0),
      responseCurves: numberValue(dados.gr_rcp),
      modes: numberValue(dados.gr_mod),
      maps: numberValue(dados.gr_map),
      logDecrementCurves: numberValue(dados.gr_log),
    },
  };
}

function importSections(raw: Record<string, string> | undefined): ShaftSegment[] {
  const section = indexedSection(raw);
  return sortedRows(section).map(([rowNumber, row]) => {
    const lengthMm = numberValue(col(row, 0));
    const outerDiameterMm = numberValue(col(row, 1));
    const packageDiameterMm = numberValue(col(row, 2));
    const ribThicknessMm = numberValue(col(row, 3));
    const hammerThicknessMm = numberValue(col(row, 4));
    const hammerWidthMm = numberValue(col(row, 5));
    const ribCount = numberValue(col(row, 6));
    const innerDiameterMm = numberValue(col(row, 7));
    const endOuterDiameterMm = numberValue(col(row, 8));

    const hasRibbed = [packageDiameterMm, ribThicknessMm, hammerThicknessMm, hammerWidthMm, ribCount]
      .some((value) => value !== 0);

    return {
      id: `irdin-segment-${rowNumber}`,
      sectionType: hasRibbed ? 'ribbed' : innerDiameterMm > 0 ? 'hollow' : 'solid',
      lengthMm,
      outerDiameterMm,
      ...(innerDiameterMm > 0 ? { innerDiameterMm } : {}),
      ...(endOuterDiameterMm > 0 && endOuterDiameterMm !== outerDiameterMm ? { endOuterDiameterMm } : {}),
      ...(hasRibbed ? {
        ribbed: {
          packageDiameterMm,
          ribThicknessMm,
          hammerThicknessMm,
          hammerWidthMm,
          ribCount,
        },
      } : {}),
    };
  });
}

function importMasses(raw: Record<string, string> | undefined): LegacyMassLoad[] {
  return sortedRows(indexedSection(raw)).map(([rowNumber, row]) => ({
    id: `irdin-mass-${rowNumber}`,
    startMm: numberValue(col(row, 0)),
    lengthMm: numberValue(col(row, 1)),
    massKg: numberValue(col(row, 2)),
    outerDiameterMm: numberValue(col(row, 3)) || undefined,
    isRotorStack: boolValue(col(row, 4)),
    umpEnabled: boolValue(col(row, 5)),
    innerDiameterMm: numberValue(col(row, 6)) || undefined,
    magneticThrust: false,
  }));
}

function importBearings(raw: Record<string, string> | undefined, issues: ValidationIssue[]): Bearing[] {
  return sortedRows(indexedSection(raw)).map(([rowNumber, row]) => {
    const id = `irdin-bearing-${rowNumber}`;
    const table = parseBearingTable(col(row, 11), issues, id);
    const sourceFile = col(row, 10);

    const constantStiffness = {
      xx: numberValue(col(row, 1)),
      zz: numberValue(col(row, 2)),
      xz: numberValue(col(row, 3)),
      zx: numberValue(col(row, 4)),
    };
    const constantDamping = {
      xx: numberValue(col(row, 5)),
      zz: numberValue(col(row, 6)),
      xz: numberValue(col(row, 7)),
      zx: numberValue(col(row, 8)),
    };

    const representative = table[0];

    return {
      id,
      name: `BRG ${rowNumber}`,
      positionMm: numberValue(col(row, 0)),
      coefficients: {
        kind: 'matrix',
        source: table.length > 0 ? 'speed-dependent' : sourceFile ? 'file' : 'constant',
        sourceFile,
        stiffness: representative?.stiffness ?? constantStiffness,
        damping: representative?.damping ?? constantDamping,
        ...(table.length > 0 ? { table } : {}),
      },
    };
  });
}

function importSupports(
  raw: Record<string, string> | undefined,
  bearings: Bearing[],
): Support[] {
  return sortedRows(indexedSection(raw)).map(([rowNumber, row]) => {
    const bearingIndex = numberValue(col(row, 0));
    const linkedBearing = bearings[bearingIndex - 1];
    return {
      id: `irdin-support-${rowNumber}`,
      name: `SUP ${rowNumber}`,
      type: col(row, 10) || 'Support',
      locationMm: linkedBearing?.positionMm ?? 0,
      stiffness: {
        xx: numberValue(col(row, 1)),
        zz: numberValue(col(row, 2)),
        xz: numberValue(col(row, 3)),
        zx: numberValue(col(row, 4)),
      },
      damping: {
        xx: numberValue(col(row, 5)),
        zz: numberValue(col(row, 6)),
        xz: numberValue(col(row, 7)),
        zx: numberValue(col(row, 8)),
      },
      massKg: numberValue(col(row, 9)) || undefined,
    };
  });
}

function importUnbalance(raw: Record<string, string> | undefined): ForceExcitation[] {
  return sortedRows(indexedSection(raw)).map(([rowNumber, row]) => ({
    id: `irdin-unbalance-${rowNumber}`,
    positionMm: numberValue(col(row, 0)),
    phaseDeg: numberValue(col(row, 1)),
    magnitude: numberValue(col(row, 2)),
    unit: 'g-mm',
    type: 'unbalance',
    direction: 'radial',
  }));
}

function importResponses(
  raw: Record<string, string> | undefined,
  _issues: ValidationIssue[],
): ResponsePoint[] {
  const rows = sortedRows(indexedSection(raw));

  // RotorDin Fortran saidas.f90 documents desp=1 as horizontal and desp=2
  // as vertical; the native lateral DOF convention is x,z.  Therefore the
  // current contract is 1→X and 2→Z, while the original code is preserved.
  return rows.map(([rowNumber, row]) => {
    const code = numberValue(col(row, 1));
    return {
      id: `irdin-response-${rowNumber}`,
      positionMm: numberValue(col(row, 0)),
      coordinate: code === 2 ? 'Z' : 'X',
      sourceCoordinateCode: code,
      orientationDeg: numberValue(col(row, 2)),
      note: `irDin Coord ${code || '?'}`,
    };
  });
}

function importConcentrated(raw: Record<string, string> | undefined): ConcentratedMass[] {
  return sortedRows(indexedSection(raw)).map(([rowNumber, row]) => ({
    id: `irdin-concentrated-${rowNumber}`,
    positionMm: numberValue(col(row, 0)),
    massKg: numberValue(col(row, 1)),
    ix: numberValue(col(row, 2)) || undefined,
    iy: numberValue(col(row, 3)) || undefined,
    iz: numberValue(col(row, 4)) || undefined,
  }));
}

export function importIrDinText(text: string): IrDinImportResult {
  const raw = parseIrDinDocument(text);
  const issues: ValidationIssue[] = [];
  const meta = raw.sections.irDin ?? {};
  const dados = raw.sections.Dados ?? {};

  if (!raw.sections.irDin || !raw.sections.Dados || !raw.sections.Secoes) {
    issues.push({
      code: 'IRDIN-FORMAT-001',
      severity: 'error',
      legacySource: 'current irDin text format',
      message: 'File must contain [irDin], [Dados] and [Secoes] sections.',
    });
  }

  const segments = importSections(raw.sections.Secoes);
  const loads = importMasses(raw.sections.Massas);
  const bearings = importBearings(raw.sections.Mancais, issues);
  const supports = importSupports(raw.sections.Suporte, bearings);

  supports.forEach((support, index) => {
    const bearingIndex = numberValue(indexedSection(raw.sections.Suporte).rows.get(index + 1)?.get(0));
    const bearing = bearings[bearingIndex - 1];
    if (bearing) bearing.supportId = support.id;
  });

  const analyses = [
    'campbell',
    'modes',
    'unbalance-response',
    'elastic-line',
    'critical-speed-map',
    'orbit',
    'stability',
  ] as const;

  issues.push({
    code: 'IRDIN-ANALYSIS-FLAGS-001',
    severity: 'warning',
    legacySource: '[Dados]',
    message: 'The supplied irDin file contains analysis parameters but no explicit enabled/disabled flags. Parameters are imported, while analysis selections default to disabled pending source-code confirmation.',
  });

  const project: RotorProject = {
    id: `irdin-${dados.ref || 'project'}-${dados.comp || 'case'}`,
    reference: dados.ref || 'IRDIN',
    description: dados.comp || 'Imported irDin project',
    component: dados.comp,
    nominalSpeedRpm: numberValue(dados.nnom) || undefined,
    user: meta.Usuario,
    settings: projectSettings(dados),
    segments,
    bearings,
    legacyLoads: loads,
    concentratedMasses: importConcentrated(raw.sections.Concent),
    forces: importUnbalance(raw.sections.Desbal),
    responsePoints: importResponses(raw.sections.Respo, issues),
    supports,
    analyses: analyses.map((kind) => ({ kind, enabled: false })),
    generateBiegePlot: false,
  };

  if (segments.some((segment) => segment.ribbed && (segment.innerDiameterMm ?? 0) > 0)) {
    issues.push({
      code: 'IRDIN-GEOM-COMBINED-001',
      severity: 'warning',
      legacySource: '[Secoes]',
      message: 'Current irDin data contains geometry that is simultaneously ribbed and hollow. The native React domain preserves it; BIEGE14 legacy serialization is intentionally blocked for this case.',
    });
  }

  return { project, raw, issues };
}
