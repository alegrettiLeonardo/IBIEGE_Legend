import {
  importLegacyBearing,
  importLegacyLoadGrid,
  importLegacyShaftGrid,
  matrixViewForBearing,
  toLegacyShaftGrid,
  type LegacyLoadGridRow,
  type LegacyShaftGridRow,
  type RotorProject,
} from '../domain';

export const legacyDemoShaftRows: LegacyShaftGridRow[] = [
  { L: 350, D: 190 },
  { L: 303, D: 200 },
  { L: 136, D: 214 },
  { L: 118, D: 250 },
  { L: 132.47, D: 260 },
  { L: 1534, D: 260, DPCT: 580, A: 50, B: 0, C: 0, NR_COST: 6 },
  { L: 200, D: 260 },
  { L: 112, D: 260, D_F: 200 },
  { L: 118.53, D: 200 },
  { L: 116.5, D: 180 },
  { L: 110.5, D: 152 },
  { L: 99, D: 112 },
];

export const legacyDemoLoadRows: LegacyLoadGridRow[] = [
  { Xi: 0, LC: 350, KG: 75, Empuxo: 0 },
  { Xi: 827, LC: 80, KG: 125, Empuxo: 0 },
  { Xi: 1146, LC: 1321, KG: 2838, Empuxo: 1 },
  { Xi: 3231, LC: 80, KG: 147, Empuxo: 0 },
];

const shaftImport = importLegacyShaftGrid(legacyDemoShaftRows);
const loadImport = importLegacyLoadGrid(legacyDemoLoadRows, 2);

const brg1 = importLegacyBearing(0, 536.47, 5.7e7);
const brg2 = importLegacyBearing(1, 3027, 2.029e9);

export const demoProject: RotorProject = {
  id: 'project-0v',
  reference: '0V',
  description: '53546318/20-CER',
  component: '53546318/20-CER',
  line: 'MGF',
  frame: '630',
  poles: 16,
  frequencyHz: 50,
  nominalSpeedRpm: 375,
  user: 'DHOEFT',
  segments: shaftImport.value,
  bearings: [
    { ...brg1, supportId: 'support-1' },
    { ...brg2, supportId: 'support-2' },
  ],
  legacyLoads: loadImport.value.map((load, index) => ({
    ...load,
    outerDiameterMm: [235, 1120, 845, 1000][index],
    innerDiameterMm: index === 2 ? 580 : undefined,
    umpEnabled: index === 2,
  })),
  concentratedMasses: [],
  forces: [
    { id: 'force-1', positionMm: 1146, phaseDeg: 0, magnitude: 165608, unit: 'N', type: 'synchronous', direction: 'Y' },
    { id: 'force-2', positionMm: 2467, phaseDeg: 180, magnitude: 165608, unit: 'N', type: 'synchronous', direction: 'Y' },
  ],
  responsePoints: [
    { id: 'response-1', positionMm: 536.47, coordinate: 'Y', orientationDeg: 0, note: 'BRG 1' },
    { id: 'response-2', positionMm: 3026.97, coordinate: 'Y', orientationDeg: 0, note: 'BRG 2' },
  ],
  supports: [
    {
      id: 'support-1',
      name: 'SUP 1',
      type: 'Rigid Support',
      locationMm: 536.47,
      stiffness: { xx: 5.7e7, xz: 0, zx: 0, zz: 5.7e7 },
      damping: { xx: 0, xz: 0, zx: 0, zz: 0 },
    },
    {
      id: 'support-2',
      name: 'SUP 2',
      type: 'Rigid Support',
      locationMm: 3027,
      stiffness: { xx: 2.029e9, xz: 0, zx: 0, zz: 2.029e9 },
      damping: { xx: 0, xz: 0, zx: 0, zz: 0 },
    },
  ],
  analyses: [
    { kind: 'campbell', enabled: true },
    { kind: 'modes', enabled: true },
    { kind: 'unbalance-response', enabled: true },
    { kind: 'elastic-line', enabled: true },
    { kind: 'critical-speed-map', enabled: true },
    { kind: 'orbit', enabled: true },
    { kind: 'stability', enabled: true },
  ],
  generateBiegePlot: true,
};

export const demoLegacyImportIssues = [...shaftImport.issues, ...loadImport.issues];

export function legacySegmentTableRows(): string[][] {
  return toLegacyShaftGrid(demoProject.segments).map((row, index) => [
    String(index + 1),
    demoProject.segments[index].sectionType === 'ribbed'
      ? 'Ribbed'
      : demoProject.segments[index].sectionType === 'hollow'
        ? 'Hollow'
        : demoProject.segments[index].endOuterDiameterMm
          ? 'Tapered'
          : 'Shaft',
    String(row.L ?? 0),
    String(row.D ?? 0),
    String(row.DPCT ?? 0),
    String(row.A ?? 0),
    String(row.B ?? 0),
    String(row.C ?? 0),
    String(row.NR_COST ?? 0),
    String(row.D_INT ?? 0),
    String(row.D_F ?? 0),
  ]);
}

export function bearingTableRows(): string[][] {
  return demoProject.bearings.map((bearing, index) => {
    const matrix = matrixViewForBearing(bearing);
    return [
      String(index + 1),
      bearing.name,
      String(bearing.positionMm),
      'Radial',
      matrix.stiffness.xx.toExponential(3),
      matrix.stiffness.zz.toExponential(3),
      matrix.damping.xx.toExponential(3),
      matrix.damping.zz.toExponential(3),
      bearing.coefficients.kind === 'legacy-scalar' ? 'Legacy scalar' : bearing.coefficients.source,
      bearing.supportId ?? '—',
      'Ready',
    ];
  });
}

export function massTableRows(): string[][] {
  return demoProject.legacyLoads.map((load, index) => [
    String(index + 1),
    String(load.startMm),
    String(load.lengthMm),
    String(load.massKg),
    String(load.outerDiameterMm ?? '—'),
    String(load.innerDiameterMm ?? '—'),
    load.isRotorStack ? 'Yes' : 'No',
    load.umpEnabled ? 'Yes' : 'No',
  ]);
}
