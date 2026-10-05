import type { Bearing, RotorProject, ValidationIssue } from '../domain';
import { validateProject } from '../domain';

export interface BiegeSerializationResult {
  text?: string;
  issues: ValidationIssue[];
}

export interface ParsedNkrit {
  valuesRpm: number[];
  primaryRpm?: number;
  secondaryRpm?: number;
}

const f6 = (value: number) => value.toFixed(6);
const numeric = (value: number) => Number.isInteger(value) ? String(value) : String(value);

function bearingLine(bearing: Bearing): string {
  if (bearing.coefficients.kind === 'legacy-scalar') {
    if (bearing.coefficients.infinite || bearing.coefficients.stiffnessNPerM === 0) {
      return `${f6(bearing.positionMm)}\t1\t0.0`;
    }
    return `${f6(bearing.positionMm)}\t2\t${numeric(bearing.coefficients.stiffnessNPerM)}`;
  }

  const kxx = bearing.coefficients.stiffness.xx;
  const kzz = bearing.coefficients.stiffness.zz;
  const crossTerms = Math.abs(bearing.coefficients.stiffness.xz)
    + Math.abs(bearing.coefficients.stiffness.zx)
    + Math.abs(bearing.coefficients.damping.xx)
    + Math.abs(bearing.coefficients.damping.zz)
    + Math.abs(bearing.coefficients.damping.xz)
    + Math.abs(bearing.coefficients.damping.zx);

  if (crossTerms > 0 || Math.abs(kxx - kzz) > 1e-9 * Math.max(1, Math.abs(kxx), Math.abs(kzz))) {
    throw new Error('BIEGE legacy bearing format cannot represent a general K/C matrix.');
  }

  if (kxx === 0) return `${f6(bearing.positionMm)}\t1\t0.0`;
  return `${f6(bearing.positionMm)}\t2\t${numeric(kxx)}`;
}

/**
 * Serialize the React domain to the current VB6 BIEGE14 contract.
 *
 * This is a semantic-compatibility serializer. The VB6 code uses a mixture of
 * Format(..., "######.000000") and Str$ for different sections, so whitespace
 * byte parity must be frozen with production Golden Masters before this adapter
 * is promoted to solver authority.
 */
export function serializeBiege14(project: RotorProject): BiegeSerializationResult {
  const validation = validateProject(project);
  const issues = [...validation.issues];

  if (project.bearings.length > 2) {
    return { issues };
  }

  if (issues.some((issue) => issue.severity === 'error')) {
    return { issues };
  }

  const lines: string[] = [];
  lines.push('BIEGE14');
  lines.push("''");
  lines.push("''");
  lines.push(`'MM'  2 2.06E+011 7850 ${project.generateBiegePlot ? "'JA'" : "'NEIN'"}`);

  let cursor = 0;
  for (const segment of project.segments) {
    cursor += segment.lengthMm;
    lines.push(
      `${f6(cursor)}\t${f6(segment.outerDiameterMm)}\t${segment.endOuterDiameterMm && segment.endOuterDiameterMm > 0 ? numeric(segment.endOuterDiameterMm) : '0.0'}`,
    );
  }
  lines.push('0.0\t0.0\t0.0');

  const hasRibbed = project.segments.some((segment) => segment.sectionType === 'ribbed');
  const hasHollow = project.segments.some((segment) => segment.sectionType === 'hollow');

  if (hasRibbed) {
    lines.push("'ST'");
    for (const segment of project.segments) {
      if (segment.sectionType === 'ribbed' && segment.ribbed) {
        const ribHeight = (segment.ribbed.packageDiameterMm - segment.outerDiameterMm) / 2 - segment.ribbed.hammerThicknessMm;
        lines.push([
          numeric(ribHeight),
          numeric(segment.ribbed.ribThicknessMm),
          numeric(segment.ribbed.hammerThicknessMm),
          numeric(segment.ribbed.hammerWidthMm),
          numeric(segment.ribbed.ribCount),
          '0.0',
          '0.0',
        ].join(' '));
      } else {
        lines.push('0.0\t0.0\t0.0\t0.0\t0.0\t0.0');
      }
    }
  } else if (hasHollow) {
    lines.push("'HL'");
    for (const segment of project.segments) {
      lines.push(segment.sectionType === 'hollow' ? numeric(segment.innerDiameterMm ?? 0) : '0.0');
    }
  } else {
    lines.push("'KEIN'");
  }

  const sortedLoads = [...project.legacyLoads].sort((a, b) => a.startMm - b.startMm);

  for (const load of sortedLoads) {
    if (load.lengthMm === 0) {
      if (load.startMm <= 0) {
        issues.push({
          code: 'BIEGE-POINT-ORIGIN-001',
          severity: 'warning',
          entityId: load.id,
          legacySource: 'frmMain exporta_biege',
          message: 'VB6 omits concentrated loads at Xi=0 because exporta_biege requires Xi > 0. The serializer preserves this legacy behavior.',
        });
        continue;
      }
      lines.push(`${f6(load.startMm)}\t${f6(load.massKg)}`);
    }
  }
  lines.push('0.0 0.0');

  for (const load of sortedLoads) {
    if (load.lengthMm <= 0) continue;
    lines.push(
      [
        f6(load.startMm),
        f6(load.startMm + load.lengthMm),
        f6(load.massKg),
        '0.0',
        load.magneticThrust ? '1' : '0',
      ].join('\t'),
    );
  }
  lines.push('0.0\t0.0\t0.0\t0.0\t0');

  const bearings = [...project.bearings]
    .filter((bearing) => {
      if (bearing.positionMm > 0) return true;
      issues.push({
        code: 'BIEGE-BRG-ORIGIN-001',
        severity: 'warning',
        entityId: bearing.id,
        legacySource: 'frmMain exporta_biege',
        message: 'VB6 does not emit a bearing at position zero; the serializer preserves that legacy behavior.',
      });
      return false;
    })
    .sort((a, b) => a.positionMm - b.positionMm);

  try {
    for (const bearing of bearings) lines.push(bearingLine(bearing));
  } catch (error) {
    issues.push({
      code: 'BIEGE-BRG-MATRIX-001',
      severity: 'error',
      legacySource: 'frmMain exporta_biege',
      message: error instanceof Error ? error.message : 'Unsupported bearing coefficient model.',
    });
    return { issues };
  }

  lines.push('0.0\t0\t0.0');
  return { text: lines.join('\r\n') + '\r\n', issues };
}

export function requiresMagneticThrustFile(project: RotorProject): boolean {
  return project.legacyLoads.some((load) => load.magneticThrust);
}

/**
 * Equivalent to the information extracted by legacy le_NKRIT, but without
 * attaching physical meaning to first/second occurrence prematurely.
 */
export function parseNkrit(output: string): ParsedNkrit {
  const valuesRpm: number[] = [];

  for (const line of output.split(/\r?\n/)) {
    const match = line.match(/^\s*NKRIT\s*=\s*([-+]?\d+(?:[.,]\d+)?(?:[Ee][-+]?\d+)?)/i);
    if (!match) continue;
    const value = Number(match[1].replace(',', '.'));
    if (Number.isFinite(value)) valuesRpm.push(value);
  }

  return {
    valuesRpm,
    primaryRpm: valuesRpm[0],
    secondaryRpm: valuesRpm[1],
  };
}

export function interpretNkrit(
  parsed: ParsedNkrit,
  hasMagneticThrust: boolean,
): {
  criticalRpm?: number;
  criticalHz?: number;
  magneticCriticalRpm?: number;
  magneticCriticalHz?: number;
} {
  if (hasMagneticThrust && parsed.valuesRpm.length > 1) {
    return {
      magneticCriticalRpm: parsed.valuesRpm[0],
      magneticCriticalHz: parsed.valuesRpm[0] / 60,
      criticalRpm: parsed.valuesRpm[1],
      criticalHz: parsed.valuesRpm[1] / 60,
    };
  }

  const rpm = parsed.valuesRpm[0];
  return {
    criticalRpm: rpm,
    criticalHz: rpm == null ? undefined : rpm / 60,
  };
}
