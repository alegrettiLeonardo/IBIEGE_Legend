import type {
  LegacyMassLoad,
  RotorProject,
  ShaftSegment,
  ValidationIssue,
} from '../domain';
import { segmentEndPositions } from '../domain';

export interface FlechaCase {
  frame: string;
  powerKwOrKva: number;
  speedRpm: number;
  poles: number;
  airGapMm: number;
  meanAirGapInductionT: number;
  rotorLamDiameterMm: number;
  rotorType: 'rings' | 'cage';
  rotorStackLengthMm: number;
  shaftInclinationDeg: number;
  amplification: 'vibration' | 'impact';
  shearDeformation: boolean;
  forceY_N: number;
  forceZ_N: number;
  forcePositionMm: number;
  maxSections?: number;
  splitThreshold?: number;
}

export interface FlechaSection {
  outerDiameterMm: number;
  innerDiameterMm: number;
  lengthMm: number;
  distributedMassKg: number;
  airGapMm: number;
  webWidthMm: number;
  webHeightMm: number;
  flangeWidthMm: number;
  flangeHeightMm: number;
  ribCount: number;
}

export interface FlechaPrepared {
  sections: FlechaSection[];
  issues: ValidationIssue[];
}

export interface FlechaSerializationResult {
  text?: string;
  prepared: FlechaPrepared;
  issues: ValidationIssue[];
}

export interface FlechaResult {
  frontBearingReactionY_kN: number;
  frontBearingReactionZ_kN: number;
  rearBearingReactionY_kN: number;
  rearBearingReactionZ_kN: number;
  maxDeflectionPercentOfAirGap: number;
  maxDeflectionPositionMm: number;
  frontShaftDeflectionMm: number;
  rearShaftDeflectionMm: number;
}

const zeroSection = (segment: ShaftSegment): Omit<FlechaSection, 'lengthMm' | 'distributedMassKg' | 'airGapMm'> => {
  if (segment.sectionType === 'ribbed' && segment.ribbed) {
    return {
      outerDiameterMm: segment.outerDiameterMm,
      innerDiameterMm: segment.innerDiameterMm ?? 0,
      webWidthMm: segment.ribbed.ribThicknessMm,
      webHeightMm: segment.ribbed.packageDiameterMm
        - segment.outerDiameterMm
        - (2 * segment.ribbed.hammerThicknessMm),
      flangeWidthMm: segment.ribbed.hammerWidthMm,
      flangeHeightMm: segment.ribbed.hammerThicknessMm,
      ribCount: segment.ribbed.ribCount,
    };
  }

  return {
    outerDiameterMm: segment.outerDiameterMm,
    innerDiameterMm: segment.innerDiameterMm ?? 0,
    webWidthMm: 0,
    webHeightMm: 0,
    flangeWidthMm: 0,
    flangeHeightMm: 0,
    ribCount: 0,
  };
};

const addSection = (
  target: FlechaSection[],
  segment: ShaftSegment,
  lengthMm: number,
  distributedMassKg: number,
  airGapMm: number,
) => {
  target.push({
    ...zeroSection(segment),
    lengthMm,
    distributedMassKg,
    airGapMm,
  });
};

const inSegment = (load: LegacyMassLoad, startMm: number, endMm: number) =>
  load.startMm >= startMm && load.startMm + load.lengthMm <= endMm;

export function prepareFlechaSections(
  project: RotorProject,
  airGapMm: number,
  splitThreshold = 0.95,
): FlechaPrepared {
  const issues: ValidationIssue[] = [];
  const sections: FlechaSection[] = [];
  const positions = segmentEndPositions(project.segments);
  const loads = [...project.legacyLoads].sort((a, b) => a.startMm - b.startMm);
  const rotorStack = project.legacyLoads.find((load) => load.isRotorStack);

  if (!rotorStack) {
    issues.push({
      code: 'FLECHA-ROTOR-001',
      severity: 'error',
      legacySource: 'frmFlexa Form_Activate/LdPc',
      message: 'FLECHA requires a rotor-stack load region.',
    });
  }

  project.segments.forEach((segment, segmentIndex) => {
    const range = positions[segmentIndex];
    const segmentLoads = loads.filter((load) => inSegment(load, range.startMm, range.endMm));

    if (segment.endOuterDiameterMm != null) {
      issues.push({
        code: 'FLECHA-TAPER-001',
        severity: 'warning',
        entityId: segment.id,
        legacySource: 'frmFlexa GtAxDt',
        message: 'Legacy FLECHA preparation uses grid column D only and ignores D_F; taper is reduced to the start/outer diameter.',
      });
    }

    if (segmentLoads.length === 0) {
      addSection(sections, segment, segment.lengthMm, 0, 0);
      return;
    }

    const divide = segmentLoads.some((load) => load.lengthMm < segment.lengthMm * splitThreshold);

    if (!divide) {
      const first = segmentLoads[0];
      const stackInside = rotorStack != null
        && rotorStack.startMm >= range.startMm
        && rotorStack.startMm <= range.endMm;
      addSection(sections, segment, segment.lengthMm, first.massKg, stackInside ? airGapMm : 0);
      return;
    }

    let cursor = range.startMm;
    for (const load of segmentLoads) {
      if (cursor < load.startMm) {
        addSection(sections, segment, load.startMm - cursor, 0, 0);
        cursor = load.startMm;
      }

      const loadEnd = load.startMm + load.lengthMm;
      const loadAirGap = rotorStack?.id === load.id && cursor === rotorStack.startMm ? airGapMm : 0;
      addSection(sections, segment, loadEnd - cursor, load.massKg, loadAirGap);
      cursor = loadEnd;
    }

    if (cursor < range.endMm) {
      addSection(sections, segment, range.endMm - cursor, 0, 0);
    }
  });

  return { sections, issues };
}

const line = (value: number | string) => String(value);

export function serializeFlecha(
  project: RotorProject,
  config: FlechaCase,
): FlechaSerializationResult {
  const splitThreshold = config.splitThreshold ?? 0.95;
  const maxSections = config.maxSections ?? 40;
  const prepared = prepareFlechaSections(project, config.airGapMm, splitThreshold);
  const issues = [...prepared.issues];

  const rotorStack = project.legacyLoads.find((load) => load.isRotorStack);
  if (!rotorStack) return { prepared, issues };

  if (project.bearings.length !== 2) {
    issues.push({
      code: 'FLECHA-BRG-001',
      severity: 'error',
      legacySource: 'frmFlexa Vld/WrFl',
      message: 'Legacy FLECHA input requires exactly two bearing positions.',
    });
  }

  if (prepared.sections.length > maxSections) {
    issues.push({
      code: 'FLECHA-SECTIONS-001',
      severity: 'error',
      legacySource: 'frmFlexa WrFl NRESC',
      message: `Prepared FLECHA model has ${prepared.sections.length} sections, exceeding legacy NRESC=${maxSections}.`,
    });
  }

  if (issues.some((issue) => issue.severity === 'error')) {
    return { prepared, issues };
  }

  const bearings = [...project.bearings].sort((a, b) => a.positionMm - b.positionMm);
  const lines: string[] = [];

  lines.push(config.frame);
  lines.push(line(config.powerKwOrKva));
  lines.push(line(config.speedRpm));
  lines.push(line(config.poles));
  lines.push('2');
  lines.push(line(bearings[0].positionMm));
  lines.push(line(bearings[1].positionMm));
  lines.push('0');
  lines.push('0');
  lines.push('7850');
  lines.push('207');
  lines.push('0.3');
  lines.push(line(prepared.sections.length));

  for (let i = 0; i < maxSections; i += 1) {
    const section = prepared.sections[i];
    lines.push(line(section?.outerDiameterMm ?? 0));
    lines.push(line(section?.innerDiameterMm ?? 0));
    lines.push(line(section?.lengthMm ?? 0));
    lines.push(line(section?.distributedMassKg ?? 0));
    lines.push(line(section?.airGapMm ?? 0));
  }

  for (let i = 0; i < maxSections; i += 1) {
    const section = prepared.sections[i];
    lines.push(line(section?.webWidthMm ?? 0));
    lines.push(line(section?.webHeightMm ?? 0));
    lines.push(line(section?.flangeWidthMm ?? 0));
    lines.push(line(section?.flangeHeightMm ?? 0));
    lines.push(line(section?.ribCount ?? 0));
  }

  lines.push('0');
  for (let i = 0; i < 10; i += 1) {
    lines.push('0');
    lines.push('0');
  }

  lines.push(line(config.airGapMm));
  lines.push(line(config.meanAirGapInductionT));
  lines.push(line(config.rotorLamDiameterMm));
  lines.push(config.rotorType === 'rings' ? '1' : '2');
  lines.push(line(config.rotorStackLengthMm));
  lines.push(line(rotorStack.startMm + rotorStack.lengthMm / 2));
  lines.push(line(config.shaftInclinationDeg));
  lines.push('0');
  lines.push('0');
  lines.push('0');
  lines.push(config.amplification === 'vibration' ? '1' : '2');

  lines.push('0');
  lines.push(config.forceY_N !== 0 ? '1' : '0');
  lines.push(config.forceZ_N !== 0 ? '1' : '0');

  for (let i = 0; i < 10; i += 1) {
    lines.push('0');
    lines.push('0');
  }

  lines.push(line(config.forceY_N));
  lines.push(config.forceY_N !== 0 ? line(config.forcePositionMm) : '0');
  for (let i = 0; i < 9; i += 1) {
    lines.push('0');
    lines.push('0');
  }

  lines.push(line(config.forceZ_N));
  lines.push(config.forceZ_N !== 0 ? line(config.forcePositionMm) : '0');
  for (let i = 0; i < 9; i += 1) {
    lines.push('0');
    lines.push('0');
  }

  lines.push(config.shearDeformation ? '1' : '0');

  return {
    text: lines.join('\r\n') + '\r\n',
    prepared,
    issues,
  };
}

export function parseFlechaResult(text: string): FlechaResult {
  const values = text
    .split(/\r?\n/)
    .map((item) => item.trim())
    .filter(Boolean)
    .map((item) => Number(item.replace(',', '.')));

  if (values.length !== 8 || values.some((value) => !Number.isFinite(value))) {
    throw new Error(`FLECHA output must contain exactly 8 numeric lines; received ${values.length}.`);
  }

  return {
    frontBearingReactionY_kN: values[0],
    frontBearingReactionZ_kN: values[1],
    rearBearingReactionY_kN: values[2],
    rearBearingReactionZ_kN: values[3],
    maxDeflectionPercentOfAirGap: values[4],
    maxDeflectionPositionMm: values[5],
    frontShaftDeflectionMm: values[6],
    rearShaftDeflectionMm: values[7],
  };
}
