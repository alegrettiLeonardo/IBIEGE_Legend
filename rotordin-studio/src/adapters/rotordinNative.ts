import type {
  Bearing,
  LegacyMassLoad,
  RotorProject,
  ShaftSegment,
  Support,
} from '../domain';

export type NativeQualifiedAnalysis =
  | 'modes'
  | 'campbell'
  | 'unbalance-response'
  | 'elastic-line'
  | 'critical-speed-map'
  | 'orbit'
  | 'stability';

export interface NativeSerializationResult {
  input: string;
  bearingTableRangeRpm?: [number, number];
  responseGrid: {
    initialRpm: number;
    finalRpm: number;
    stepRpm: number;
    points: number;
  };
}

interface NativeSection {
  endM: number;
  diameterM: number;
  innerDiameterM: number;
  finalDiameterM: number;
  finalInnerDiameterM: number;
  youngPa: number;
  poisson: number;
  densityKgM3: number;
  divisions: number;
  umpStiffnessNPerM2: number;
  yieldStrengthPa: number;
  elementType: 1 | 2 | 3 | 4;
  gyroscopicFactor: number;
}

interface NativeDisk {
  positionMm: number;
  lengthMm: number;
  massKg: number;
  outerDiameterMm: number;
  innerDiameterMm: number;
  offsetMm: number;
}

const DEFAULT_FIXED_STATIC_STIFFNESS = 5e20;
const DEFAULT_CONICAL_SUBDIVISIONS = 12;

function exponent(value: number, digits: number): string {
  if (!Number.isFinite(value)) throw new Error(`Non-finite native numeric value: ${value}`);
  const raw = value.toExponential(digits).toUpperCase();
  const match = raw.match(/^(.+)E([+-])(\d+)$/);
  if (!match) throw new Error(`Unable to format native numeric value: ${value}`);
  return `${match[1]}E${match[2]}${match[3].padStart(2, '0')}`;
}

export function f10(value: number): string {
  let body = exponent(value, value >= 0 ? 4 : 3);
  let out = body.padStart(10);
  if (out.length > 10) {
    body = exponent(value, 3);
    out = body.padStart(10);
  }
  if (out.length > 10) throw new Error(`Value does not fit native F10.0 field: ${value}`);
  return out;
}

export function i10(value: number): string {
  const text = String(Math.trunc(value)).padStart(10);
  if (text.length > 10) throw new Error(`Integer does not fit native I10 field: ${value}`);
  return text;
}

function axialF10M(value: number): string {
  const fixed = value.toFixed(6).padStart(10);
  return fixed.length <= 10 ? fixed : f10(value);
}

function fmt(value: number): string {
  const abs = Math.abs(value);
  if (abs >= 1e5 || (abs > 0 && abs < 1e-4)) {
    return exponent(value, 10);
  }
  // Python's .10g semantics for the small list-directed header fields used by
  // the audited contract. The qualified fields are simple 0/1 values.
  return Number(value.toPrecision(10)).toString();
}

function g6(value: number): string {
  const text = value.toPrecision(6);
  if (/e/i.test(text)) {
    const [mantissa, exp] = text.toLowerCase().split('e');
    return `${Number(mantissa)}e${Number(exp) >= 0 ? '+' : ''}${Number(exp)}`;
  }
  return String(Number(text));
}

function sumLength(project: RotorProject): number {
  return project.segments.reduce((sum, segment) => sum + segment.lengthMm, 0);
}

function localSegment(project: RotorProject, positionMm: number): {
  segment: ShaftSegment;
  startMm: number;
  ratio: number;
} {
  let start = 0;
  for (const segment of project.segments) {
    const end = start + segment.lengthMm;
    if (positionMm <= end + 1e-9) {
      const ratio = segment.lengthMm === 0
        ? 0
        : Math.max(0, Math.min(1, (positionMm - start) / segment.lengthMm));
      return { segment, startMm: start, ratio };
    }
    start = end;
  }
  throw new Error(`Position ${positionMm} mm lies outside the shaft.`);
}

function physicalOuterAt(project: RotorProject, positionMm: number): number {
  const { segment, ratio } = localSegment(project, positionMm);
  const end = segment.endOuterDiameterMm ?? segment.outerDiameterMm;
  return segment.outerDiameterMm + (end - segment.outerDiameterMm) * ratio;
}

function ribbedEquivalent(segment: ShaftSegment, density: number): {
  outerMm: number;
  density: number;
  gycof: number;
} {
  const rib = segment.ribbed;
  if (!rib) return { outerMm: segment.outerDiameterMm, density, gycof: 1 };

  const d = segment.outerDiameterMm / 1000;
  const di = (segment.innerDiameterMm ?? 0) / 1000;
  const dp = rib.packageDiameterMm / 1000;
  const ba = rib.ribThicknessMm / 1000;
  const hf = rib.hammerThicknessMm / 1000;
  const bf = rib.hammerWidthMm / 1000;
  const count = rib.ribCount;
  const ha = Math.max((dp - d) / 2 - hf, 0);
  const ra = (ha + d) / 2;
  const rf = ra + (ha + hf) / 2;
  const iRibs = count / 2 * (
    ba * ha ** 3 / 12 + ha * ba ** 3 / 12 + ra ** 2 * ha * ba
    + bf * hf ** 3 / 12 + hf * bf ** 3 / 12 + rf ** 2 * hf * bf
  );
  const iShaft = Math.PI * (d ** 4 - di ** 4) / 64;
  const iActual = iShaft + iRibs;
  const deq = (di ** 4 + 64 * iActual / Math.PI) ** 0.25;
  const areaReal = Math.PI * (d ** 2 - di ** 2) / 4 + count * (ba * ha + bf * hf);
  const areaEq = Math.PI * (deq ** 2 - di ** 2) / 4;
  if (!(areaReal > 0) || !(areaEq > 0)) throw new Error('Invalid ribbed equivalent area.');
  const densityEq = density * areaReal / areaEq;
  const gycof = areaEq / areaReal;
  if (!(gycof > 0 && gycof <= 5)) throw new Error(`Invalid GYCOF ${gycof}`);
  return { outerMm: deq * 1000, density: densityEq, gycof };
}

function elementType(segment: ShaftSegment): 1 | 2 | 3 | 4 {
  const hollow = (segment.innerDiameterMm ?? 0) > 0;
  const conical = segment.endOuterDiameterMm != null
    && Math.abs(segment.endOuterDiameterMm - segment.outerDiameterMm) > 1e-12;
  if (hollow && conical) return 4;
  if (hollow) return 3;
  if (conical) return 2;
  return 1;
}

function buildNativeSections(project: RotorProject): NativeSection[] {
  const material = project.settings?.material;
  const density = material?.densityKgPerM3 ?? 7850;
  const young = material?.youngsModulusPa ?? 2.07e11;
  const poisson = material?.poissonRatio ?? 0.3;
  const cuts = new Set<number>([0, sumLength(project)]);

  let cursor = 0;
  for (const segment of project.segments) {
    cuts.add(cursor);
    cursor += segment.lengthMm;
    cuts.add(cursor);
  }

  if (project.legacyLoads.some((load) => load.umpEnabled)) {
    throw new Error('NATIVE-UMP-QUAL-001: UMP execution is not yet qualified in the React runner.');
  }

  const positions = [...cuts].filter((x) => x >= 0 && x <= sumLength(project)).sort((a, b) => a - b);
  const sections: NativeSection[] = [];

  for (let index = 0; index < positions.length - 1; index += 1) {
    const xa = positions[index];
    const xb = positions[index + 1];
    if (xb - xa <= 1e-9) continue;
    const mid = (xa + xb) / 2;
    const { segment, startMm } = localSegment(project, mid);
    const r0 = Math.max(0, Math.min(1, (xa - startMm) / segment.lengthMm));
    const r1 = Math.max(0, Math.min(1, (xb - startMm) / segment.lengthMm));

    let type = elementType(segment);
    const endOuter = segment.endOuterDiameterMm ?? segment.outerDiameterMm;
    let outer0 = segment.outerDiameterMm + (endOuter - segment.outerDiameterMm) * r0;
    let outer1 = segment.outerDiameterMm + (endOuter - segment.outerDiameterMm) * r1;
    let inner0 = segment.innerDiameterMm ?? 0;
    let inner1 = segment.innerDiameterMm ?? 0;
    let rho = density;
    let gycof = 1;

    if (segment.ribbed) {
      const eq = ribbedEquivalent(segment, density);
      outer0 = eq.outerMm;
      outer1 = eq.outerMm;
      rho = eq.density;
      gycof = eq.gycof;
      if ((segment.innerDiameterMm ?? 0) > 0) {
        type = 3;
        inner0 = segment.innerDiameterMm ?? 0;
        inner1 = inner0;
      } else {
        type = 1;
        inner0 = 0;
        inner1 = 0;
      }
    }

    let divisions = 0;
    if (type === 2 || type === 4) {
      divisions = Math.max(1, Math.round(DEFAULT_CONICAL_SUBDIVISIONS * ((xb - xa) / segment.lengthMm)));
    }

    sections.push({
      endM: xb / 1000,
      diameterM: outer0 / 1000,
      innerDiameterM: inner0 / 1000,
      finalDiameterM: outer1 / 1000,
      finalInnerDiameterM: inner1 / 1000,
      youngPa: young,
      poisson,
      densityKgM3: rho,
      divisions,
      umpStiffnessNPerM2: 0,
      yieldStrengthPa: 0,
      elementType: type,
      gyroscopicFactor: gycof,
    });
  }

  if (sections.length > 99) throw new Error(`RotorDin accepts at most 99 SECTIONS; got ${sections.length}`);
  return sections;
}

function effectiveMassInnerDiameter(project: RotorProject, mass: LegacyMassLoad, positionMm: number): number {
  if ((mass.innerDiameterMm ?? 0) > 0) return mass.innerDiameterMm!;
  if ((mass.outerDiameterMm ?? 0) <= 0) return 0;
  const { segment } = localSegment(project, positionMm);
  if (segment.ribbed && segment.ribbed.packageDiameterMm > 0) return segment.ribbed.packageDiameterMm;
  return physicalOuterAt(project, positionMm);
}

function effectiveMassOuterDiameter(project: RotorProject, mass: LegacyMassLoad, positionMm: number): number {
  return (mass.outerDiameterMm ?? 0) > 0 ? mass.outerDiameterMm! : physicalOuterAt(project, positionMm);
}

function buildNativeDisks(project: RotorProject): NativeDisk[] {
  const divisions = project.settings?.discretization?.packageDivisions ?? 4;
  if (!(divisions >= 1 && divisions <= 99)) throw new Error('Package divisions must be 1..99.');

  const disks: NativeDisk[] = [];
  for (const mass of project.legacyLoads) {
    if (!(mass.lengthMm > 0)) continue;
    const count = mass.isRotorStack ? divisions : 1;
    const length = mass.lengthMm / count;
    const weight = mass.massKg / count;
    for (let piece = 0; piece < count; piece += 1) {
      const xi = mass.startMm + piece * length;
      const center = xi + length / 2;
      const outer = effectiveMassOuterDiameter(project, mass, center);
      const inner = effectiveMassInnerDiameter(project, mass, center);
      if (!(outer > 0) || inner < 0 || inner >= outer) {
        throw new Error(`Invalid native disk geometry at ${center} mm: Dext=${outer}, Dint=${inner}`);
      }
      disks.push({
        positionMm: center,
        lengthMm: length,
        massKg: weight,
        outerDiameterMm: outer,
        innerDiameterMm: inner,
        offsetMm: 0,
      });
    }
  }
  if (disks.length > 99) throw new Error(`RotorDin accepts at most 99 DISK records; got ${disks.length}`);
  return disks;
}

function rawBearingPayload(bearing: Bearing, index: number): string {
  if (bearing.coefficients.kind !== 'matrix' || !bearing.coefficients.rawTableContent) {
    throw new Error(`NATIVE-BRG-TABLE-001: Bearing #${index} lacks the qualified embedded TABLE payload.`);
  }
  const chunks = bearing.coefficients.rawTableContent.split('§').map((x) => x.trim()).filter(Boolean);
  if (!chunks[0]?.toUpperCase().startsWith('TABLE')) {
    throw new Error(`NATIVE-BRG-TABLE-002: Bearing #${index} is not a qualified TABLE source.`);
  }
  const rows = chunks.slice(1);
  if (rows.length === 0) throw new Error(`Bearing #${index} TABLE contains no rows.`);
  return [
    `TABLE:LEGACY_IMPORT_${index}`,
    `${rows.length} 1.0`,
    ...rows.map((row) => row.replace(/\|/g, ' ')),
    'EOF',
  ].join('\n') + '\n';
}

function commonBearingRange(project: RotorProject): [number, number] | undefined {
  const ranges = project.bearings.map((bearing) => {
    if (bearing.coefficients.kind !== 'matrix' || !bearing.coefficients.table?.length) return undefined;
    const speeds = bearing.coefficients.table.map((item) => item.speedRpm);
    return [Math.min(...speeds), Math.max(...speeds)] as [number, number];
  }).filter((item): item is [number, number] => item != null);

  if (ranges.length === 0) return undefined;
  const lower = Math.max(...ranges.map((item) => item[0]));
  const upper = Math.min(...ranges.map((item) => item[1]));
  if (upper < lower) throw new Error('Bearing TABLEs have no common speed range.');
  return [lower, upper];
}

function responseGrid(project: RotorProject, tableRange?: [number, number]) {
  const source = project.settings?.unbalanceResponse;
  const rawI = source?.initialRpm ?? 500;
  const rawF = source?.finalRpm ?? 12000;
  const rawDiv = source?.divisions ?? 400;
  let initial = rawI;
  let final = rawF;
  if (tableRange) {
    initial = Math.max(initial, tableRange[0]);
    final = Math.min(final, tableRange[1]);
  }
  if (!(final > initial) || rawDiv < 0) throw new Error('Invalid qualified response grid.');
  const step = (final - initial) / (rawDiv + 1);
  const points = Math.floor((final - initial) / step + 1e-9) + 1;
  return { initialRpm: initial, finalRpm: final, stepRpm: step, points };
}

function responseStepField(grid: ReturnType<typeof responseGrid>): string {
  // The authoritative Python contract searches neighbouring F10.0 tokens only
  // when native REAL32 accumulation would alter cardinality. EST-ST41's direct
  // F10 representation already preserves the frozen 402-point contract.
  return f10(grid.stepRpm);
}

function supportFor(project: RotorProject, bearing: Bearing): Support | undefined {
  return project.supports.find((support) => support.id === bearing.supportId);
}

function analysisCampbellRange(project: RotorProject, analysis: NativeQualifiedAnalysis): [number, number] {
  const raw: [number, number] = [
    project.settings?.campbell?.initialRpm ?? 0,
    project.settings?.campbell?.finalRpm ?? Math.max(project.nominalSpeedRpm ?? 0, 6000),
  ];
  if (analysis === 'campbell' || analysis === 'stability') {
    const range = commonBearingRange(project);
    if (!range) return raw;
    const clipped: [number, number] = [Math.max(raw[0], range[0]), Math.min(raw[1], range[1])];
    if (!(clipped[1] > clipped[0])) throw new Error('Campbell range does not intersect bearing TABLE domain.');
    return clipped;
  }
  return raw;
}

export function nativeFlags(analysis: NativeQualifiedAnalysis, hasSupports: boolean): string[] {
  const flag = {
    modes: '-m',
    campbell: '-c',
    stability: '-c',
    'unbalance-response': '-f',
    'elastic-line': '-s',
    'critical-speed-map': '-k',
    orbit: '-t',
  }[analysis];
  return ['-std', flag, ...(hasSupports ? ['-b'] : [])];
}

export function serializeRotorDinNative(project: RotorProject, analysis: NativeQualifiedAnalysis): NativeSerializationResult {
  const sections = buildNativeSections(project);
  const disks = buildNativeDisks(project);
  const tableRange = commonBearingRange(project);
  if (!tableRange) throw new Error('NATIVE-BRG-TABLE-003: Qualified EST-ST41 runner requires speed-dependent TABLE bearings.');

  const nominalRpm = project.nominalSpeedRpm ?? project.settings?.speed?.nominalRpm ?? 0;
  if (!(nominalRpm > 0)) throw new Error('Nominal speed must be positive for the qualified TABLE-bearing contract.');
  if (nominalRpm < tableRange[0] || nominalRpm > tableRange[1]) {
    throw new Error(`Nominal speed ${nominalRpm} rpm lies outside TABLE range ${tableRange[0]}–${tableRange[1]} rpm.`);
  }

  const grid = responseGrid(project, tableRange);
  const [campbellI, campbellF] = analysisCampbellRange(project, analysis);
  const config = project.settings;
  const lines: string[] = [];
  const emit = (value = '') => lines.push(value);
  const fw = (...parts: string[]) => parts.join('');

  const user = (project.user || project.reference || 'ROTORDIN').slice(0, 12);
  const poles = project.poles ?? 2;
  const frequency = project.frequencyHz ?? 60;
  const description = (
    project.description && project.description !== project.component
      ? project.description
      : [project.component, project.line, project.frame, `${poles}P`, `${frequency}Hz`].filter(Boolean).join(' ')
  ).slice(0, 50);

  emit('NAME');
  emit(user);
  emit('');
  emit('DESCRIPTION');
  emit(description || project.reference || 'RotorDin calculation');
  emit('');

  emit('SECTIONS  LENGTH    DIV       MXDIV     LDR       HANGLE    ACCG      HANUN     SHMFWD');
  emit(
    fw(
      i10(sections.length),
      f10(sumLength(project) / 1000),
      i10(config?.discretization?.divisions ?? 1),
      i10(config?.discretization?.maxDivisions ?? 3),
      // Current authoritative legacy importer does not map s_relld to native LDR.
      f10(0),
      f10(0),
      f10(config?.elasticLine?.gravityMPerS2 ?? 9.81),
    )
    + 'D'.padStart(10)
    + ' 1,1'
  );
  emit('');
  emit('POSITION  EXTDIAM   INTDIAM   YOUNG     POISSON   DENSITY   DIV       UMP       YSTR      ETYPE     EXTDIAM2  INTDIAM2');
  for (const section of sections) {
    emit(fw(
      f10(section.endM), f10(section.diameterM), f10(section.innerDiameterM),
      f10(section.youngPa), f10(section.poisson), f10(section.densityKgM3),
      i10(section.divisions), f10(section.umpStiffnessNPerM2),
      f10(section.yieldStrengthPa), i10(section.elementType),
      f10(section.finalDiameterM), f10(section.finalInnerDiameterM),
    ));
  }

  emit('');
  emit('DISK      NRAT      GYCOF');
  const gycof = sections
    .map((section, index) => ({ index: index + 1, value: section.gyroscopicFactor }))
    .filter((item) => Math.abs(item.value - 1) > 1e-10);
  if (gycof.length > 5) throw new Error(`RotorDin accepts at most 5 GYCOF pairs; got ${gycof.length}`);
  const gycofText = gycof.map((item) => `${item.index}-${g6(item.value)}`).join(',');
  emit(i10(disks.length) + f10(nominalRpm) + gycofText);
  emit('');
  emit('POSITION  DIAMETER  LENGTH    DENSITY   INTDIAM   IX        IY        WEIGHT    OFFSET');
  for (const disk of disks) {
    emit(fw(
      axialF10M(disk.positionMm / 1000),
      f10(disk.outerDiameterMm / 1000),
      f10(disk.lengthMm / 1000),
      f10(0),
      f10(disk.innerDiameterMm / 1000),
      f10(0), f10(0),
      f10(disk.massKg),
      f10(disk.offsetMm / 1000),
    ));
  }

  emit('');
  emit('BEARING   SCALE     RKS       FXSTF');
  emit(`${project.bearings.length} ${fmt(1)} ${fmt(0)} ${fmt(DEFAULT_FIXED_STATIC_STIFFNESS)}`);
  emit('');
  emit('POSITION  KXX       KXZ       KZZ       KZX       CXX       CXZ       CZZ       CZX       WEIGTH    KPH       KTH');
  for (const bearing of project.bearings) {
    const matrix = bearing.coefficients.kind === 'matrix'
      ? bearing.coefficients
      : undefined;
    const k = matrix?.stiffness ?? { xx: 0, xz: 0, zx: 0, zz: 0 };
    const d = matrix?.damping ?? { xx: 0, xz: 0, zx: 0, zz: 0 };
    // The production irDin constant grid is zero when Arq/TABLE is authoritative.
    const tableAuthoritative = Boolean(matrix?.rawTableContent);
    emit(fw(
      f10(bearing.positionMm / 1000),
      f10(tableAuthoritative ? 0 : k.xx), f10(tableAuthoritative ? 0 : k.xz),
      f10(tableAuthoritative ? 0 : k.zz), f10(tableAuthoritative ? 0 : k.zx),
      f10(tableAuthoritative ? 0 : d.xx), f10(tableAuthoritative ? 0 : d.xz),
      f10(tableAuthoritative ? 0 : d.zz), f10(tableAuthoritative ? 0 : d.zx),
      f10(0),
    ));
  }

  emit('');
  emit('CPBIS     CPBFS     CPBSPD    NCRSPD    INTSPD    SMIST     SMNDC     SMSPD     IMDTRK');
  emit(fw(
    f10(campbellI), f10(campbellF),
    f10(config?.campbell?.divisions ?? 25),
    i10(config?.campbell?.rotations ?? 4),
    i10(config?.campbell?.interpolationPoints ?? 50),
    f10(config?.criticalSpeedMap?.initialStiffnessNPerM ?? 1e7),
    i10(config?.criticalSpeedMap?.divisions ?? 4),
    f10(nominalRpm),
  ) + i10(1));

  emit('');
  emit('EXCT      UNBIF     UNBFF     UNBFD     UNBMD     MINAMP    MDRPM     UANUN');
  emit(fw(
    i10(project.forces.length),
    f10(grid.initialRpm),
    f10(grid.finalRpm),
    responseStepField(grid),
    i10(config?.unbalanceResponse?.modes ?? 10),
    f10(0),
    f10(nominalRpm),
  ) + 'D'.padStart(10));
  emit('');
  emit('POSITION  VALUE     PHASE     KIND      THFR/OFFSET');
  for (const force of project.forces) {
    if (force.type !== 'unbalance') {
      throw new Error('NATIVE-FORCE-QUAL-001: Only qualified unbalance excitations are enabled in the current React runner.');
    }
    emit(fw(
      f10(force.positionMm / 1000),
      f10(force.magnitude),
      f10(force.phaseDeg),
      i10(0),
    ));
  }

  emit('');
  emit('FRESPPOS  NBRMOD    TIMOEP    TIMSPD    TANGLE    RANUN     TAUN');
  emit(fw(
    i10(project.responsePoints.length),
    i10(config?.modes?.modes ?? 10),
    f10(0),
    f10(nominalRpm),
    f10(0),
  ) + 'D'.padStart(10) + 'D'.padStart(10));
  emit('');
  emit('POSITION  DISPL     ORIENT');
  for (const probe of project.responsePoints) {
    const code = probe.sourceCoordinateCode ?? (probe.coordinate === 'Z' ? 2 : 1);
    emit(fw(f10(probe.positionMm / 1000), f10(code), f10(probe.orientationDeg)));
  }

  emit('');
  emit('OPTIONS');
  emit('opt');

  emit('');
  emit('CONCENT');
  emit(i10(project.concentratedMasses.length));
  emit('');
  emit('CMASSPOS  CMASS     IX        IY        IZ');
  for (const mass of project.concentratedMasses) {
    emit(fw(
      f10(mass.positionMm / 1000), f10(mass.massKg),
      f10(mass.ix ?? 0), f10(mass.iy ?? 0), f10(mass.iz ?? 0),
    ));
  }

  emit('KIND      MODES');
  emit(i10(2));
  emit('');
  emit('BEAFILES  MINAMP    MDRPM     CPBCRX');
  emit(fw(i10(project.bearings.length), f10(0), f10(nominalRpm)));
  emit('');
  emit('BEANBR    BEAFILES');
  project.bearings.forEach((_bearing, index) => {
    emit(f10(index + 1) + ` bearing_${index + 1}`);
  });
  emit('');
  emit('OPTIONS');
  emit('');
  project.bearings.forEach((bearing, index) => {
    lines.push(...rawBearingPayload(bearing, index + 1).trimEnd().split('\n'));
  });

  if (project.supports.length) {
    emit('SUPPORT   SCALE');
    emit(`${project.supports.length} 1.0`);
    emit('');
    emit('BEANBR    KXX       KXZ       KZZ       KZX       CXX       CXZ       CZZ       CZX       WEIGTH    DESC');
    project.supports.forEach((support) => {
      const bearingIndex = project.bearings.findIndex((bearing) => bearing.supportId === support.id);
      if (bearingIndex < 0) throw new Error(`Support ${support.id} is not linked to a bearing.`);
      const desc = (support.type || 'PYTHON').slice(0, 10);
      emit(fw(
        i10(bearingIndex + 1),
        f10(support.stiffness.xx), f10(support.stiffness.xz),
        f10(support.stiffness.zz), f10(support.stiffness.zx),
        f10(support.damping.xx), f10(support.damping.xz),
        f10(support.damping.zz), f10(support.damping.zx),
        f10(support.massKg ?? 0),
      ) + desc);
    });
  }

  emit('');
  return {
    input: lines.join('\n') + '\n',
    bearingTableRangeRpm: tableRange,
    responseGrid: grid,
  };
}
