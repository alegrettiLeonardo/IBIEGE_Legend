import type { ExecuteQualifiedRunResponse } from '../../electron/contracts';
import { mergeRotorDinParsedResults } from '../adapters/rotordinResultData';

export interface RunComparisonMetric {
  group: string;
  metric: string;
  baseline: number;
  candidate: number;
  delta: number;
  relativePercent?: number;
  unit: string;
}

export interface RunComparison {
  baselineRunId: string;
  candidateRunId: string;
  solverHashSame: boolean;
  inputHashSame: boolean;
  projectHashSame: boolean;
  commonAnalyses: string[];
  baselineOnlyAnalyses: string[];
  candidateOnlyAnalyses: string[];
  metrics: RunComparisonMetric[];
}

function relativePercent(baseline: number, candidate: number): number | undefined {
  if (!Number.isFinite(baseline) || !Number.isFinite(candidate) || Math.abs(baseline) < 1e-30) return undefined;
  return ((candidate - baseline) / Math.abs(baseline)) * 100;
}

function metric(
  group: string,
  name: string,
  baseline: number,
  candidate: number,
  unit: string,
): RunComparisonMetric {
  return {
    group,
    metric: name,
    baseline,
    candidate,
    delta: candidate - baseline,
    relativePercent: relativePercent(baseline, candidate),
    unit,
  };
}

function maxFinite(values: number[]): number {
  const finite = values.filter(Number.isFinite);
  return finite.length ? Math.max(...finite) : 0;
}

function maxAbsFinite(values: number[]): number {
  const finite = values.filter(Number.isFinite).map(Math.abs);
  return finite.length ? Math.max(...finite) : 0;
}

export function compareQualifiedRuns(
  baseline: ExecuteQualifiedRunResponse,
  candidate: ExecuteQualifiedRunResponse,
): RunComparison {
  const a = mergeRotorDinParsedResults(baseline.jobs.map((job) => job.results));
  const b = mergeRotorDinParsedResults(candidate.jobs.map((job) => job.results));
  const metrics: RunComparisonMetric[] = [];

  const aAnalyses = new Set(baseline.jobs.map((job) => job.analysis));
  const bAnalyses = new Set(candidate.jobs.map((job) => job.analysis));
  const commonAnalyses = [...aAnalyses].filter((item) => bAnalyses.has(item)).sort();
  const baselineOnlyAnalyses = [...aAnalyses].filter((item) => !bAnalyses.has(item)).sort();
  const candidateOnlyAnalyses = [...bAnalyses].filter((item) => !aAnalyses.has(item)).sort();

  if (a.modes && b.modes) {
    const count = Math.min(a.modes.modes.length, b.modes.modes.length);
    for (let index = 0; index < count; index += 1) {
      metrics.push(metric(
        'Modes',
        `Mode ${index + 1} frequency`,
        a.modes.modes[index].frequencyHz,
        b.modes.modes[index].frequencyHz,
        'Hz',
      ));
    }
  }

  if (a.campbell && b.campbell) {
    for (const harmonic of new Set([
      ...a.campbell.criticalSpeeds.map((point) => point.harmonic),
      ...b.campbell.criticalSpeeds.map((point) => point.harmonic),
    ])) {
      const aa = a.campbell.criticalSpeeds.filter((point) => point.harmonic === harmonic).map((point) => point.rpm).sort((x, y) => x - y);
      const bb = b.campbell.criticalSpeeds.filter((point) => point.harmonic === harmonic).map((point) => point.rpm).sort((x, y) => x - y);
      const count = Math.min(aa.length, bb.length);
      const deltas = Array.from({ length: count }, (_, index) => Math.abs(bb[index] - aa[index]));
      metrics.push(metric('Campbell', `${harmonic} crossing count`, aa.length, bb.length, 'count'));
      if (count) metrics.push(metric('Campbell', `${harmonic} max crossing shift`, 0, maxFinite(deltas), 'rpm'));
    }

    const branchCount = Math.min(a.campbell.logarithmicDecrement.length, b.campbell.logarithmicDecrement.length);
    for (let index = 0; index < branchCount; index += 1) {
      metrics.push(metric(
        'Stability',
        `Mode ${index + 1} minimum log decrement`,
        Math.min(...a.campbell.logarithmicDecrement[index].y.filter(Number.isFinite)),
        Math.min(...b.campbell.logarithmicDecrement[index].y.filter(Number.isFinite)),
        '',
      ));
    }
  }

  if (a.unbalance && b.unbalance) {
    const count = Math.min(a.unbalance.amplitude.length, b.unbalance.amplitude.length);
    for (let index = 0; index < count; index += 1) {
      metrics.push(metric(
        'Unbalance',
        `Response ${index + 1} max amplitude`,
        maxAbsFinite(a.unbalance.amplitude[index].y) * 1e6,
        maxAbsFinite(b.unbalance.amplitude[index].y) * 1e6,
        'µm',
      ));
    }
    metrics.push(metric('Unbalance', 'Native peak count', a.unbalance.peaks.length, b.unbalance.peaks.length, 'count'));
  }

  if (a.elasticLine && b.elasticLine) {
    const maxA = maxFinite(a.elasticLine.positionM.map((_, index) => Math.hypot(a.elasticLine!.xM[index], a.elasticLine!.zM[index]))) * 1e6;
    const maxB = maxFinite(b.elasticLine.positionM.map((_, index) => Math.hypot(b.elasticLine!.xM[index], b.elasticLine!.zM[index]))) * 1e6;
    metrics.push(metric('Elastic line', 'Maximum resultant displacement', maxA, maxB, 'µm'));
  }

  if (a.criticalMap && b.criticalMap) {
    const branches = Math.min(a.criticalMap.modesRpm.length, b.criticalMap.modesRpm.length);
    for (let index = 0; index < branches; index += 1) {
      const valuesA = a.criticalMap.modesRpm[index].y;
      const valuesB = b.criticalMap.modesRpm[index].y;
      const count = Math.min(valuesA.length, valuesB.length);
      const deltas = Array.from({ length: count }, (_, item) => Math.abs(valuesB[item] - valuesA[item]));
      metrics.push(metric('Critical speed map', `Mode ${index + 1} max shift`, 0, maxFinite(deltas), 'rpm'));
    }
  }

  if (a.orbit && b.orbit) {
    const maxRadius = (run: NonNullable<typeof a.orbit>) =>
      maxFinite(run.sections.flatMap((shape) => shape.xM.map((x, index) => Math.hypot(x, shape.zM[index]))));
    metrics.push(metric('Orbit', 'Maximum orbit radius', maxRadius(a.orbit), maxRadius(b.orbit), 'm'));
  }

  return {
    baselineRunId: baseline.manifest.runId,
    candidateRunId: candidate.manifest.runId,
    solverHashSame: baseline.manifest.solver.executableHash === candidate.manifest.solver.executableHash,
    inputHashSame: baseline.manifest.inputHash === candidate.manifest.inputHash,
    projectHashSame: baseline.manifest.projectHash === candidate.manifest.projectHash,
    commonAnalyses,
    baselineOnlyAnalyses,
    candidateOnlyAnalyses,
    metrics,
  };
}
