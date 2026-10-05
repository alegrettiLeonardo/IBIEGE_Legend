import type { CriticalSpeedPoint } from './rotordinResults';

export interface XYSeries {
  name: string;
  x: number[];
  y: number[];
}

export interface CampbellData {
  naturalFrequency: XYSeries[];
  harmonics: XYSeries[];
  logarithmicDecrement: XYSeries[];
  criticalSpeeds: CriticalSpeedPoint[];
  frequencyKind: string;
}

export interface CriticalMapData {
  title: string;
  stiffnessNPerM: number[];
  modesRpm: XYSeries[];
  ratedSpeedRpm: number;
}

export interface UnbalancePeak {
  responseIndex: number;
  sequence: number;
  rpm: number;
  amplitudeM: number;
  amplificationFactor: number;
  status: string;
}

export interface UnbalanceData {
  responsePositionsM: number[];
  amplitude: XYSeries[];
  phase: XYSeries[];
  peaks: UnbalancePeak[];
  amplificationLowLimit?: number;
}

export interface ModeShape {
  mode: number;
  frequencyHz: number;
  direction: string;
  positionM: number[];
  xM: number[];
  zM: number[];
}

export interface ModeShapeData {
  rpm: number;
  modes: ModeShape[];
}

export interface ElasticLineData {
  positionM: number[];
  xM: number[];
  zM: number[];
  gxRad: number[];
  gzRad: number[];
}

export interface StaticLateralData {
  bearingForce: XYSeries[];
  bearingMoment: XYSeries[];
  bendingMoment: XYSeries[];
  shearForce: XYSeries[];
  bendingStress: XYSeries[];
}

export interface OrbitShape {
  section: number;
  positionM: number;
  direction: string;
  xM: number[];
  zM: number[];
}

export interface OrbitSpeedCase {
  speedRpm: number;
  sections: OrbitShape[];
}

export interface OrbitData {
  speedRpm: number;
  points: number;
  sections: OrbitShape[];
  speedCases: OrbitSpeedCase[];
}

export interface RotorDinParsedResults {
  campbell?: CampbellData;
  criticalMap?: CriticalMapData;
  unbalance?: UnbalanceData;
  modes?: ModeShapeData;
  elasticLine?: ElasticLineData;
  staticLateral?: StaticLateralData;
  orbit?: OrbitData;
}

const FLOAT_RE = /(?<![A-Za-z0-9_])[-+]?(?:nan|inf(?:inity)?)(?![A-Za-z0-9_])|[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][-+]?\d+)?/gi;
const MODE_RE = /mode\s*=\s*(\d+)\s+f\s*=\s*([-+0-9.EeDd]+)\s*\(Hz\)\s*dir\s*=\s*(\w+)/i;
const ORBIT_SECTION_RE = /sec\.\s*(\d+)\s+y\s*=\s*([-+0-9.EeDd]+)\s*\(m\)\s+dir\s*=\s*(\w+)/i;
const SPEED_RE = /speed\s*=\s*([-+0-9.EeDd]+)\s*\(rpm\)/i;

function numberToken(token: string): number {
  return Number(token.replace(/[Dd]/g, 'E'));
}

function numbers(line: string): number[] {
  return [...line.matchAll(FLOAT_RE)].map((match) => numberToken(match[0]));
}

function findLine(lines: string[], text: string, start = 0): number {
  const needle = text.toLowerCase();
  for (let index = start; index < lines.length; index += 1) {
    if (lines[index].toLowerCase().includes(needle)) return index;
  }
  return -1;
}

function numericTableAfter(
  lines: string[],
  marker: string,
  options: { start?: number; graphData?: boolean; minCols?: number } = {},
): number[][] {
  const start = options.start ?? 0;
  const minCols = options.minCols ?? 2;
  let position = findLine(lines, marker, start);
  if (position < 0) return [];
  if (options.graphData) {
    const graph = findLine(lines, 'Graph Data', position + 1);
    if (graph >= 0) position = graph;
  }

  const rows: number[][] = [];
  let started = false;
  for (let index = position + 1; index < lines.length; index += 1) {
    const line = lines[index];
    const values = numbers(line);
    const beginsNumeric = /^\s*[-+]?(?:\d|\.)/.test(line);
    if (beginsNumeric && values.length >= minCols) {
      rows.push(values);
      started = true;
      continue;
    }
    if (started && (!line.trim() || !beginsNumeric)) break;
  }
  return rows;
}

function seriesFromRows(rows: number[][], prefix: string): XYSeries[] {
  if (!rows.length) return [];
  const columns = Math.min(...rows.map((row) => row.length));
  const x = rows.map((row) => row[0]);
  return Array.from({ length: Math.max(0, columns - 1) }, (_, index) => ({
    name: `${prefix} ${index + 1}`,
    x: [...x],
    y: rows.map((row) => row[index + 1]),
  }));
}

function excitationOrders(lines: string[]): number[] {
  const line = lines.find((candidate) => candidate.trim().startsWith('EXCITATION_ORDERS '));
  if (!line) return [1, 2, 0.5];
  return line.trim().split(/\s+/).slice(1).map(numberToken).filter(Number.isFinite);
}

export function parseCampbellData(text: string, criticalSpeeds: CriticalSpeedPoint[] = []): CampbellData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  const orders = excitationOrders(lines);
  const offset = 1 + orders.length;
  const rows = numericTableAfter(lines, 'Natural frequencies (rpm) x (Hz)', { graphData: true, minCols: offset + 1 });
  const naturalFrequency: XYSeries[] = [];
  const harmonics: XYSeries[] = [];

  if (rows.length) {
    const columns = Math.min(...rows.map((row) => row.length));
    const x = rows.map((row) => row[0]);
    orders.forEach((order, index) => {
      harmonics.push({
        name: `${order.toFixed(1)}x`,
        x: [...x],
        y: rows.map((row) => row[index + 1]),
      });
    });
    for (let column = offset; column < columns; column += 1) {
      naturalFrequency.push({
        name: `Mode ${column - offset + 1}`,
        x: [...x],
        y: rows.map((row) => row[column]),
      });
    }
  }

  const decrementMarker = findLine(lines, 'Logarithmic Decrement raw') >= 0
    ? 'Logarithmic Decrement raw'
    : 'Logarithmic Decrement';
  const decrementRows = numericTableAfter(lines, decrementMarker, { minCols: 2 });
  const logarithmicDecrement = seriesFromRows(decrementRows, 'Mode');

  return {
    naturalFrequency,
    harmonics,
    logarithmicDecrement,
    criticalSpeeds,
    frequencyKind: text.includes('FREQUENCY_KIND DAMPED_IMAGINARY_PART')
      ? 'damped_imaginary_part'
      : 'legacy_unspecified',
  };
}

export function parseCriticalMapData(text: string, title = 'Critical speed map'): CriticalMapData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  const rows = numericTableAfter(lines, 'Critical speed (N/m) x (rpm)', { graphData: true, minCols: 2 });
  if (!rows.length) return undefined;

  const columns = Math.min(...rows.map((row) => row.length));
  const stiffness = rows.map((row) => row[0]);
  const ratedIndex = findLine(lines, 'rated speed (rpm)');
  const ratedValues = ratedIndex >= 0 ? numbers(lines[ratedIndex]) : [];
  const ratedSpeedRpm = ratedValues.at(-1) ?? 0;

  return {
    title,
    stiffnessNPerM: stiffness,
    modesRpm: Array.from({ length: Math.max(0, columns - 1) }, (_, index) => ({
      name: `Mode ${index + 1}`,
      x: [...stiffness],
      y: rows.map((row) => row[index + 1]),
    })),
    ratedSpeedRpm,
  };
}

function responsePositions(lines: string[]): number[] {
  const index = findLine(lines, 'point');
  if (index < 0) return [];
  for (const line of lines.slice(index + 1, index + 6)) {
    if (line.toLowerCase().includes('y (m)')) return numbers(line);
  }
  return [];
}

function parseUnbalancePeaks(lines: string[]): { peaks: UnbalancePeak[]; lowLimit?: number } {
  const start = findLine(lines, 'Automatic Response Analysis');
  if (start < 0) return { peaks: [] };
  const foundStop = findLine(lines, 'Amplitude (rpm) x (m)', start + 1);
  const stop = foundStop < 0 ? lines.length : foundStop;

  let lowLimit: number | undefined;
  const lowMarker = findLine(lines, 'amplif. low limit', start + 1);
  if (lowMarker >= start && lowMarker < stop) {
    for (const line of lines.slice(lowMarker + 1, Math.min(stop, lowMarker + 5))) {
      if (!/^\s*[-+]?(?:\d|\.)/.test(line)) continue;
      const values = numbers(line);
      if (values.length) {
        lowLimit = values[0];
        break;
      }
    }
  }

  let header = -1;
  for (let index = start + 1; index < stop; index += 1) {
    const value = lines[index].toLowerCase();
    if (value.includes('max. disp.') && value.includes('amplif. factor')) {
      header = index;
      break;
    }
  }
  if (header < 0) return { peaks: [], lowLimit };

  const raw: Array<{ responseIndex: number; rpm: number; amplitudeM: number; amplificationFactor: number; status: string }> = [];
  for (const line of lines.slice(header + 1, stop)) {
    if (!/^\s*[-+]?(?:\d|\.)/.test(line)) continue;
    const values = numbers(line);
    if (values.length < 4) continue;
    const responseIndex = Math.round(values[3]);
    if (responseIndex <= 0) continue;
    raw.push({
      responseIndex,
      rpm: values[0],
      amplitudeM: values[1],
      amplificationFactor: values[2],
      status: line.match(/([nifF])\s*$/)?.[1] ?? '',
    });
  }

  raw.sort((a, b) => a.responseIndex - b.responseIndex || a.rpm - b.rpm);
  const counters = new Map<number, number>();
  const peaks = raw.map((item) => {
    const sequence = (counters.get(item.responseIndex) ?? 0) + 1;
    counters.set(item.responseIndex, sequence);
    return { ...item, sequence };
  });

  return { peaks, lowLimit };
}

export function parseUnbalanceData(text: string): UnbalanceData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  const amplitudeRows = numericTableAfter(lines, 'Amplitude (rpm) x (m)', { graphData: true, minCols: 2 });
  const phaseRows = numericTableAfter(lines, 'Phase (rpm) x (rad)', { minCols: 2 });
  const { peaks, lowLimit } = parseUnbalancePeaks(lines);
  return {
    responsePositionsM: responsePositions(lines),
    amplitude: seriesFromRows(amplitudeRows, 'Point'),
    phase: seriesFromRows(phaseRows, 'Point'),
    peaks,
    ...(lowLimit == null ? {} : { amplificationLowLimit: lowLimit }),
  };
}

export function parseModeShapeData(text: string): ModeShapeData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  const rpmIndex = findLine(lines, 'Mode Shape - rpm');
  const rpm = rpmIndex >= 0 ? numbers(lines[rpmIndex]).at(-1) ?? 0 : 0;

  const metadata: Array<{ mode: number; frequencyHz: number; direction: string }> = [];
  for (const line of lines) {
    const match = line.match(MODE_RE);
    if (!match) continue;
    metadata.push({
      mode: Number(match[1]),
      frequencyHz: numberToken(match[2]),
      direction: match[3].toUpperCase(),
    });
  }

  const rows = numericTableAfter(lines, 'Graph Data', { minCols: 3 });
  if (!rows.length || !metadata.length) return { rpm, modes: [] };

  const columns = Math.min(...rows.map((row) => row.length));
  const modeCount = Math.min(metadata.length, Math.floor((columns - 1) / 2));
  const positionM = rows.map((row) => row[0]);
  const modes = Array.from({ length: modeCount }, (_, index): ModeShape => ({
    ...metadata[index],
    positionM: [...positionM],
    xM: rows.map((row) => row[1 + index]),
    zM: rows.map((row) => row[1 + modeCount + index]),
  }));

  return { rpm, modes };
}

export function parseElasticLineData(text: string): ElasticLineData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  const rows = numericTableAfter(lines, 'Displacements', { minCols: 5 }).filter((row) => row.length >= 5);
  if (!rows.length) return undefined;
  return {
    positionM: rows.map((row) => row[0]),
    xM: rows.map((row) => row[1]),
    zM: rows.map((row) => row[2]),
    gxRad: rows.map((row) => row[3]),
    gzRad: rows.map((row) => row[4]),
  };
}

export function parseStaticLateralData(text: string): StaticLateralData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  const result: StaticLateralData = {
    bearingForce: [],
    bearingMoment: [],
    bendingMoment: [],
    shearForce: [],
    bendingStress: [],
  };

  const reactionRows = numericTableAfter(lines, 'Bearing Reaction (N, Nm)', { minCols: 5 });
  if (reactionRows.length) {
    const bearing = reactionRows.map((row) => row[0]);
    result.bearingForce = [
      { name: 'X', x: [...bearing], y: reactionRows.map((row) => row[1]) },
      { name: 'Z', x: [...bearing], y: reactionRows.map((row) => row[2]) },
    ];
    result.bearingMoment = [
      { name: 'X', x: [...bearing], y: reactionRows.map((row) => row[3]) },
      { name: 'Z', x: [...bearing], y: reactionRows.map((row) => row[4]) },
    ];
  }

  const momentRows = numericTableAfter(lines, 'Bending moment at division (Nm)', { minCols: 3 });
  if (momentRows.length) {
    const position = momentRows.map((row) => row[0]);
    result.bendingMoment = [
      { name: 'X', x: [...position], y: momentRows.map((row) => row[1]) },
      { name: 'Z', x: [...position], y: momentRows.map((row) => row[2]) },
    ];
  }

  const shearRows = numericTableAfter(lines, 'Shear Force constant on Element (N)', { minCols: 4 });
  if (shearRows.length) {
    const position: number[] = [];
    const shearX: number[] = [];
    const shearZ: number[] = [];
    for (const row of shearRows) {
      position.push(row[0], row[1]);
      shearX.push(row[2], row[2]);
      shearZ.push(row[3], row[3]);
    }
    result.shearForce = [
      { name: 'X', x: [...position], y: shearX },
      { name: 'Z', x: [...position], y: shearZ },
    ];
  }

  const stressRows = numericTableAfter(lines, 'bending stress', { minCols: 2 });
  if (stressRows.length) {
    result.bendingStress = [{
      name: 'Bending stress',
      x: stressRows.map((row) => row[0]),
      y: stressRows.map((row) => row[1]),
    }];
  }

  const hasData = Object.values(result).some((series) => series.length > 0);
  return hasData ? result : undefined;
}

function parseOrbitSections(lines: string[], start: number, stop: number): OrbitShape[] {
  const shapes: OrbitShape[] = [];
  let index = start;
  while (index < stop) {
    const match = lines[index].match(ORBIT_SECTION_RE);
    if (!match) {
      index += 1;
      continue;
    }

    const section = Number(match[1]);
    const positionM = numberToken(match[2]);
    const direction = match[3].toUpperCase();
    index += 1;
    const xM: number[] = [];
    const zM: number[] = [];
    let started = false;

    while (index < stop) {
      const line = lines[index];
      const values = numbers(line);
      const beginsNumeric = /^\s*[-+]?(?:\d|\.)/.test(line);
      if (beginsNumeric && values.length >= 2) {
        xM.push(values[0]);
        zM.push(values[1]);
        started = true;
        index += 1;
        continue;
      }
      if (started) break;
      index += 1;
    }

    shapes.push({ section, positionM, direction, xM, zM });
  }
  return shapes;
}

export function parseOrbitData(text: string): OrbitData | undefined {
  if (!text.trim()) return undefined;
  const lines = text.split(/\r?\n/);
  let points = 0;
  for (const line of lines) {
    if (!line.includes('Orbit points')) continue;
    points = Math.round(numbers(line).at(-1) ?? 0);
    break;
  }

  const markers: Array<{ index: number; speedRpm: number }> = [];
  lines.forEach((line, index) => {
    const match = line.match(SPEED_RE);
    if (match) markers.push({ index, speedRpm: numberToken(match[1]) });
  });

  const speedCases: OrbitSpeedCase[] = [];
  markers.forEach((marker, item) => {
    const stop = item + 1 < markers.length ? markers[item + 1].index : lines.length;
    const sections = parseOrbitSections(lines, marker.index + 1, stop);
    if (sections.length) speedCases.push({ speedRpm: marker.speedRpm, sections });
  });

  if (speedCases.length) {
    return {
      speedRpm: speedCases[0].speedRpm,
      points,
      sections: [...speedCases[0].sections],
      speedCases,
    };
  }

  const sections = parseOrbitSections(lines, 0, lines.length);
  return { speedRpm: 0, points, sections, speedCases: [] };
}

export function parseRotorDinResultData(
  sections: Record<string, string>,
  criticalSpeeds: CriticalSpeedPoint[] = [],
): RotorDinParsedResults {
  const elasticText = sections['selaline.out'] ?? '';
  return {
    ...(sections['campbell.out']
      ? { campbell: parseCampbellData(sections['campbell.out'], criticalSpeeds) }
      : {}),
    ...(sections['ucspdmap.out']
      ? { criticalMap: parseCriticalMapData(sections['ucspdmap.out'], 'Critical speed map') }
      : {}),
    ...(sections['unblresp.out']
      ? { unbalance: parseUnbalanceData(sections['unblresp.out']) }
      : {}),
    ...(sections['modeshp.out']
      ? { modes: parseModeShapeData(sections['modeshp.out']) }
      : {}),
    ...(elasticText
      ? {
          elasticLine: parseElasticLineData(elasticText),
          staticLateral: parseStaticLateralData(elasticText),
        }
      : {}),
    ...(sections['timeresp.out']
      ? { orbit: parseOrbitData(sections['timeresp.out']) }
      : {}),
  };
}

export function mergeRotorDinParsedResults(results: RotorDinParsedResults[]): RotorDinParsedResults {
  const merged: RotorDinParsedResults = {};
  for (const result of results) {
    if (result.campbell) merged.campbell = result.campbell;
    if (result.criticalMap) merged.criticalMap = result.criticalMap;
    if (result.unbalance) merged.unbalance = result.unbalance;
    if (result.modes) merged.modes = result.modes;
    if (result.elasticLine) merged.elasticLine = result.elasticLine;
    if (result.staticLateral) merged.staticLateral = result.staticLateral;
    if (result.orbit) merged.orbit = result.orbit;
  }
  return merged;
}
