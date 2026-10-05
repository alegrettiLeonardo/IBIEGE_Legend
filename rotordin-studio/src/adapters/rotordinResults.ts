export function parseRotorDinMarkedSections(stdout: string): Record<string, string> {
  const result = new Map<string, string[]>();
  let current: string | undefined;

  for (const raw of stdout.split(/\r?\n/)) {
    const line = raw.trim();

    if (line.startsWith('RD_AUDIT_V1 ')) {
      const rows = result.get('rdaudit.out') ?? [];
      rows.push(raw);
      result.set('rdaudit.out', rows);
      continue;
    }

    const begin = line.match(/^#BEGIN\s+(.+?)\s*$/);
    if (begin) {
      current = begin[1];
      if (!result.has(current)) result.set(current, []);
      continue;
    }

    if (/^#END\s+/.test(line)) {
      current = undefined;
      continue;
    }

    if (current) {
      result.get(current)!.push(raw);
    }
  }

  return Object.fromEntries(
    [...result.entries()].map(([name, lines]) => [
      name,
      lines.join('\n').replace(/\s+$/u, '') + '\n',
    ]),
  );
}

export interface CriticalSpeedPoint {
  rpm: number;
  hz: number;
  harmonic: string;
}

function numericMatrixAfter(lines: string[], title: string): number[][] {
  const index = lines.findIndex((line) => line.toLowerCase().includes(title.toLowerCase()));
  if (index < 0) return [];
  const rows: number[][] = [];
  for (const line of lines.slice(index + 2)) {
    if (!line.trim()) break;
    const values = line.trim().split(/\s+/).map((token) => Number(token.replace(/[Dd]/g, 'E')));
    if (!values.length || values.some((value) => !Number.isFinite(value))) break;
    rows.push(values);
  }
  return rows;
}

export function parseCampbellCriticalSpeeds(campbell: string): CriticalSpeedPoint[] {
  if (!campbell.trim()) return [];
  const lines = campbell.split(/\r?\n/);
  const metadata = lines.find((line) => line.trim().startsWith('EXCITATION_ORDERS '));
  const orders = metadata
    ? metadata.trim().split(/\s+/).slice(1).map((value) => Number(value))
    : [1, 2, 0.5];
  const labels = orders.map((order) => `${order.toFixed(1)}x`);

  const rpm = numericMatrixAfter(lines, 'Critical Speeds (rpm)');
  const hz = numericMatrixAfter(lines, 'Critical Frequencies (Hz)');
  const points: CriticalSpeedPoint[] = [];

  for (let row = 0; row < Math.min(rpm.length, hz.length); row += 1) {
    for (let column = 0; column < labels.length; column += 1) {
      const speed = rpm[row]?.[column] ?? 0;
      const frequency = hz[row]?.[column] ?? 0;
      if (speed > 0 && frequency > 0) {
        points.push({ rpm: speed, hz: frequency, harmonic: labels[column] });
      }
    }
  }

  return points.sort((a, b) => a.rpm - b.rpm || a.harmonic.localeCompare(b.harmonic));
}
