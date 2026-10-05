import { useEffect, useMemo, useState, type ReactNode } from 'react';
import {
  Activity,
  BarChart3,
  CircleDot,
  FileText,
  Gauge,
  Orbit,
  ShieldCheck,
  Target,
} from 'lucide-react';
import type {
  CampbellData,
  ElasticLineData,
  ModeShape,
  OrbitShape,
  RotorDinParsedResults,
  UnbalanceData,
  XYSeries,
} from '../../adapters/rotordinResultData';
import { mergeRotorDinParsedResults } from '../../adapters/rotordinResultData';
import { useProject } from '../../state/ProjectContext';

type ResultTab =
  | 'campbell'
  | 'stability'
  | 'response'
  | 'modes'
  | 'line'
  | 'map'
  | 'orbit';

const TAB_LABELS: Record<ResultTab, string> = {
  campbell: 'Campbell',
  stability: 'Stability / Log Dec',
  response: 'Unbalance Response',
  modes: 'Modes',
  line: 'Elastic Line',
  map: 'Critical Speed Map',
  orbit: 'Orbit',
};

const CHART_COLORS = [
  '#126bc5',
  '#14a6a8',
  '#7d57c2',
  '#d66b1f',
  '#d23f57',
  '#4f7d35',
  '#7a5b32',
  '#2f8ab8',
  '#8f4f8f',
  '#637a8a',
  '#e04f43',
  '#5d8fcb',
];

function availableTabs(results: RotorDinParsedResults): ResultTab[] {
  const tabs: ResultTab[] = [];
  if (results.campbell) {
    tabs.push('campbell');
    if (results.campbell.logarithmicDecrement.length) tabs.push('stability');
  }
  if (results.unbalance) tabs.push('response');
  if (results.modes) tabs.push('modes');
  if (results.elasticLine) tabs.push('line');
  if (results.criticalMap) tabs.push('map');
  if (results.orbit) tabs.push('orbit');
  return tabs;
}

function formatNumber(value: number, digits = 4): string {
  if (!Number.isFinite(value)) return '—';
  const abs = Math.abs(value);
  if ((abs > 0 && abs < 1e-3) || abs >= 1e6) return value.toExponential(3);
  return value.toLocaleString('en-US', { maximumFractionDigits: digits });
}

function resultIcon(tab: ResultTab) {
  switch (tab) {
    case 'campbell': return BarChart3;
    case 'stability': return ShieldCheck;
    case 'response': return Target;
    case 'modes': return Activity;
    case 'line': return Gauge;
    case 'map': return BarChart3;
    case 'orbit': return Orbit;
  }
}

interface PlotSeries {
  name: string;
  x: number[];
  y: number[];
  dashed?: boolean;
}

function LineSeriesChart({
  series,
  xLabel,
  yLabel,
  xTransform = (value) => value,
  yTransform = (value) => value,
}: {
  series: PlotSeries[];
  xLabel: string;
  yLabel: string;
  xTransform?: (value: number) => number;
  yTransform?: (value: number) => number;
}) {
  const width = 820;
  const height = 330;
  const left = 68;
  const right = 22;
  const top = 20;
  const bottom = 52;

  const transformed = series.map((item) => ({
    ...item,
    points: item.x.map((x, index) => ({
      x: xTransform(x),
      y: yTransform(item.y[index]),
    })).filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y)),
  }));
  const points = transformed.flatMap((item) => item.points);
  if (!points.length) return <div className="result-empty-small">No plottable native data.</div>;

  const rawMinX = Math.min(...points.map((point) => point.x));
  const rawMaxX = Math.max(...points.map((point) => point.x));
  const rawMinY = Math.min(...points.map((point) => point.y));
  const rawMaxY = Math.max(...points.map((point) => point.y));
  const padX = Math.max((rawMaxX - rawMinX) * 0.02, Math.abs(rawMaxX || 1) * 1e-9);
  const padY = Math.max((rawMaxY - rawMinY) * 0.06, Math.abs(rawMaxY || 1) * 1e-9);
  const minX = rawMinX - padX;
  const maxX = rawMaxX + padX;
  const minY = rawMinY - padY;
  const maxY = rawMaxY + padY;

  const sx = (value: number) =>
    left + ((value - minX) / Math.max(maxX - minX, Number.EPSILON)) * (width - left - right);
  const sy = (value: number) =>
    top + (1 - (value - minY) / Math.max(maxY - minY, Number.EPSILON)) * (height - top - bottom);

  const xTicks = Array.from({ length: 6 }, (_, index) => minX + (index / 5) * (maxX - minX));
  const yTicks = Array.from({ length: 6 }, (_, index) => minY + (index / 5) * (maxY - minY));

  return (
    <div className="native-chart">
      <svg viewBox={`0 0 ${width} ${height}`} role="img" aria-label={`${yLabel} versus ${xLabel}`}>
        <g className="chart-grid">
          {xTicks.map((tick) => <line key={`x-${tick}`} x1={sx(tick)} y1={top} x2={sx(tick)} y2={height - bottom}/>)}
          {yTicks.map((tick) => <line key={`y-${tick}`} x1={left} y1={sy(tick)} x2={width - right} y2={sy(tick)}/>)}
        </g>
        <line className="axis" x1={left} y1={height - bottom} x2={width - right} y2={height - bottom}/>
        <line className="axis" x1={left} y1={top} x2={left} y2={height - bottom}/>
        {transformed.map((item, seriesIndex) => {
          const path = item.points.map((point, index) =>
            `${index === 0 ? 'M' : 'L'} ${sx(point.x).toFixed(2)} ${sy(point.y).toFixed(2)}`).join(' ');
          return <path key={item.name} d={path} fill="none" stroke={CHART_COLORS[seriesIndex % CHART_COLORS.length]} strokeWidth="2" strokeDasharray={item.dashed ? '7 5' : undefined}/>;
        })}
        {xTicks.map((tick) => <text key={`xt-${tick}`} className="native-chart-tick" x={sx(tick)} y={height - 28} textAnchor="middle">{formatNumber(tick, 2)}</text>)}
        {yTicks.map((tick) => <text key={`yt-${tick}`} className="native-chart-tick" x={left - 8} y={sy(tick) + 4} textAnchor="end">{formatNumber(tick, 3)}</text>)}
        <text className="native-chart-label" x={(left + width - right) / 2} y={height - 7} textAnchor="middle">{xLabel}</text>
        <text className="native-chart-label" transform={`translate(15 ${(top + height - bottom) / 2}) rotate(-90)`} textAnchor="middle">{yLabel}</text>
      </svg>
      <div className="native-chart-legend">
        {series.map((item, index) => <span key={item.name}><i style={{ backgroundColor: CHART_COLORS[index % CHART_COLORS.length] }}/>{item.name}</span>)}
      </div>
    </div>
  );
}

function OrbitPlot({ shape }: { shape: OrbitShape }) {
  const width = 420;
  const height = 330;
  const pad = 35;
  const xs = shape.xM.filter(Number.isFinite);
  const zs = shape.zM.filter(Number.isFinite);
  if (!xs.length || !zs.length) return <div className="result-empty-small">No orbit coordinates.</div>;

  const maxAbsX = Math.max(...xs.map(Math.abs), 1e-20);
  const maxAbsZ = Math.max(...zs.map(Math.abs), 1e-20);
  const scale = Math.min((width - 2 * pad) / (2 * maxAbsX), (height - 2 * pad) / (2 * maxAbsZ));
  const cx = width / 2;
  const cy = height / 2;
  const path = shape.xM.map((x, index) => {
    const z = shape.zM[index];
    return `${index === 0 ? 'M' : 'L'} ${(cx + x * scale).toFixed(2)} ${(cy - z * scale).toFixed(2)}`;
  }).join(' ');

  return (
    <div className="native-orbit">
      <svg viewBox={`0 0 ${width} ${height}`}>
        <g className="chart-grid">
          <line x1={pad} y1={cy} x2={width - pad} y2={cy}/>
          <line x1={cx} y1={pad} x2={cx} y2={height - pad}/>
        </g>
        <path d={path} fill="none" stroke="#126bc5" strokeWidth="2.5"/>
        <circle cx={cx} cy={cy} r="3.5" fill="#263d51"/>
        <text className="native-chart-label" x={width - 20} y={cy - 7} textAnchor="end">X</text>
        <text className="native-chart-label" x={cx + 7} y={20}>Z</text>
      </svg>
      <div className="orbit-meta">
        <span>Section {shape.section}</span>
        <span>y = {formatNumber(shape.positionM, 5)} m</span>
        <span>{shape.direction}</span>
      </div>
    </div>
  );
}

function ResultCard({ title, children }: { title: string; children: ReactNode }) {
  return <section className="result-card"><header><h3>{title}</h3></header><div className="result-card-body">{children}</div></section>;
}

function CampbellPanel({ data }: { data: CampbellData }) {
  const series: PlotSeries[] = [
    ...data.naturalFrequency.map((item) => ({ ...item })),
    ...data.harmonics.map((item) => ({ ...item, dashed: true })),
  ];
  return (
    <div className="result-panel-grid">
      <ResultCard title="Campbell diagram — native campbell.out">
        <LineSeriesChart series={series} xLabel="Speed [rpm]" yLabel="Frequency [Hz]"/>
      </ResultCard>
      <ResultCard title="Critical-speed crossings">
        <div className="result-table-wrap">
          <table>
            <thead><tr><th>#</th><th>Speed [rpm]</th><th>Frequency [Hz]</th><th>Order</th></tr></thead>
            <tbody>{data.criticalSpeeds.map((point, index) => <tr key={`${point.rpm}-${point.harmonic}`}><td>{index + 1}</td><td>{formatNumber(point.rpm, 3)}</td><td>{formatNumber(point.hz, 4)}</td><td>{point.harmonic}</td></tr>)}</tbody>
          </table>
        </div>
      </ResultCard>
    </div>
  );
}

function StabilityPanel({ data }: { data: CampbellData }) {
  const minima = data.logarithmicDecrement.map((series) => ({
    mode: series.name,
    minimum: Math.min(...series.y.filter(Number.isFinite)),
  }));
  return (
    <div className="result-panel-grid">
      <ResultCard title="Logarithmic decrement — native Campbell computation">
        <LineSeriesChart series={data.logarithmicDecrement} xLabel="Speed [rpm]" yLabel="Log decrement"/>
      </ResultCard>
      <ResultCard title="Branch minima">
        <div className="result-table-wrap">
          <table><thead><tr><th>Branch</th><th>Minimum log dec</th><th>Status</th></tr></thead>
            <tbody>{minima.map((item) => <tr key={item.mode}><td>{item.mode}</td><td>{formatNumber(item.minimum, 5)}</td><td><span className={item.minimum < 0 ? 'result-status danger' : 'result-status success'}>{item.minimum < 0 ? 'Unstable region' : 'Stable'}</span></td></tr>)}</tbody>
          </table>
        </div>
      </ResultCard>
    </div>
  );
}

function ResponsePanel({ data }: { data: UnbalanceData }) {
  const amplitudes = data.amplitude.map((series) => ({ ...series, y: series.y.map((value) => value * 1e6) }));
  return (
    <div className="result-panel-stack">
      <div className="result-panel-grid two-equal">
        <ResultCard title="Amplitude — native unblresp.out">
          <LineSeriesChart series={amplitudes} xLabel="Speed [rpm]" yLabel="Amplitude [µm]"/>
        </ResultCard>
        <ResultCard title="Phase — native unblresp.out">
          <LineSeriesChart series={data.phase} xLabel="Speed [rpm]" yLabel="Phase [rad]"/>
        </ResultCard>
      </div>
      <ResultCard title="Solver-native automatic peaks">
        <div className="result-table-wrap compact">
          <table><thead><tr><th>Response</th><th>Peak</th><th>rpm</th><th>Amplitude [µm]</th><th>AF</th><th>Status</th></tr></thead>
            <tbody>{data.peaks.map((peak) => <tr key={`${peak.responseIndex}-${peak.sequence}`}><td>{peak.responseIndex}</td><td>{peak.sequence}</td><td>{formatNumber(peak.rpm, 3)}</td><td>{formatNumber(peak.amplitudeM * 1e6, 4)}</td><td>{formatNumber(peak.amplificationFactor, 4)}</td><td>{peak.status || '—'}</td></tr>)}</tbody>
          </table>
        </div>
      </ResultCard>
    </div>
  );
}

function ModesPanel({ modes, selected, onSelect }: { modes: ModeShape[]; selected: number; onSelect: (index: number) => void }) {
  const index = Math.min(selected, Math.max(0, modes.length - 1));
  const mode = modes[index];
  if (!mode) return <div className="result-empty-small">No mode-shape data.</div>;
  const series: PlotSeries[] = [
    { name: 'X', x: mode.positionM, y: mode.xM.map((value) => value * 1e6) },
    { name: 'Z', x: mode.positionM, y: mode.zM.map((value) => value * 1e6) },
  ];
  return (
    <div className="result-panel-grid">
      <ResultCard title={`Mode ${mode.mode} shape — native modeshp.out`}>
        <LineSeriesChart series={series} xLabel="Shaft position [m]" yLabel="Projected displacement [µm]"/>
      </ResultCard>
      <ResultCard title="Mode selector">
        <div className="mode-selector">
          {modes.map((item, itemIndex) => <button className={itemIndex === index ? 'active' : ''} key={item.mode} onClick={() => onSelect(itemIndex)}>
            <span>Mode {item.mode}</span><strong>{formatNumber(item.frequencyHz, 4)} Hz</strong><em>{item.direction}</em>
          </button>)}
        </div>
        <div className="result-kpis">
          <div><span>Frequency</span><strong>{formatNumber(mode.frequencyHz, 5)} Hz</strong></div>
          <div><span>Direction</span><strong>{mode.direction}</strong></div>
          <div><span>Stations</span><strong>{mode.positionM.length}</strong></div>
        </div>
      </ResultCard>
    </div>
  );
}

function ElasticLinePanel({ data, staticData }: { data: ElasticLineData; staticData?: RotorDinParsedResults['staticLateral'] }) {
  const series: PlotSeries[] = [
    { name: 'X', x: data.positionM, y: data.xM.map((value) => value * 1e6) },
    { name: 'Z', x: data.positionM, y: data.zM.map((value) => value * 1e6) },
  ];
  const resultants = data.positionM.map((position, index) => ({
    position,
    value: Math.hypot(data.xM[index], data.zM[index]),
  }));
  const maximum = resultants.reduce((best, item) => item.value > best.value ? item : best, resultants[0] ?? { position: 0, value: 0 });
  const reactionX = staticData?.bearingForce.find((item) => item.name === 'X');
  const reactionZ = staticData?.bearingForce.find((item) => item.name === 'Z');

  return (
    <div className="result-panel-grid">
      <ResultCard title="Elastic line — native selaline.out">
        <LineSeriesChart series={series} xLabel="Shaft position [m]" yLabel="Displacement [µm]"/>
        <div className="result-kpis">
          <div><span>Max resultant</span><strong>{formatNumber(maximum.value * 1e6, 4)} µm</strong></div>
          <div><span>At position</span><strong>{formatNumber(maximum.position, 6)} m</strong></div>
          <div><span>Stations</span><strong>{data.positionM.length}</strong></div>
        </div>
      </ResultCard>
      <ResultCard title="Bearing reactions — solver-native static lateral">
        <div className="result-table-wrap compact">
          <table><thead><tr><th>Bearing</th><th>Fx [N]</th><th>Fz [N]</th></tr></thead>
            <tbody>{(reactionX?.x ?? []).map((bearing, index) => <tr key={bearing}><td>{bearing}</td><td>{formatNumber(reactionX?.y[index] ?? 0, 5)}</td><td>{formatNumber(reactionZ?.y[index] ?? 0, 5)}</td></tr>)}</tbody>
          </table>
        </div>
      </ResultCard>
    </div>
  );
}

function CriticalMapPanel({ data }: { data: NonNullable<RotorDinParsedResults['criticalMap']> }) {
  return (
    <div className="result-panel-stack">
      <ResultCard title="Critical-speed stiffness map — native ucspdmap.out">
        <LineSeriesChart series={data.modesRpm} xLabel="log10(K [N/m])" yLabel="Critical speed [rpm]" xTransform={(value) => Math.log10(value)}/>
      </ResultCard>
      <div className="result-kpis">
        <div><span>Stiffness points</span><strong>{data.stiffnessNPerM.length}</strong></div>
        <div><span>Mode branches</span><strong>{data.modesRpm.length}</strong></div>
        <div><span>Rated speed</span><strong>{formatNumber(data.ratedSpeedRpm, 2)} rpm</strong></div>
        <div><span>K range</span><strong>{formatNumber(data.stiffnessNPerM[0])} – {formatNumber(data.stiffnessNPerM.at(-1) ?? 0)} N/m</strong></div>
      </div>
    </div>
  );
}

function OrbitPanel({ data, selected, onSelect }: { data: NonNullable<RotorDinParsedResults['orbit']>; selected: number; onSelect: (index: number) => void }) {
  const sections = data.speedCases[0]?.sections ?? data.sections;
  const index = Math.min(selected, Math.max(0, sections.length - 1));
  const shape = sections[index];
  if (!shape) return <div className="result-empty-small">No orbit data.</div>;
  return (
    <div className="result-panel-grid">
      <ResultCard title={`Orbit — native timeresp.out @ ${formatNumber(data.speedRpm, 2)} rpm`}>
        <OrbitPlot shape={shape}/>
      </ResultCard>
      <ResultCard title="Native section selector">
        <div className="orbit-section-list">
          {sections.map((item, itemIndex) => <button className={itemIndex === index ? 'active' : ''} key={item.section} onClick={() => onSelect(itemIndex)}>
            <span>Sec. {item.section}</span><strong>{formatNumber(item.positionM, 5)} m</strong><em>{item.direction}</em>
          </button>)}
        </div>
        <div className="result-kpis">
          <div><span>Orbit points</span><strong>{data.points}</strong></div>
          <div><span>Native sections</span><strong>{sections.length}</strong></div>
          <div><span>Speed cases</span><strong>{data.speedCases.length || 1}</strong></div>
        </div>
      </ResultCard>
    </div>
  );
}

export default function ResultsWorkspace() {
  const { selectedRun } = useProject();
  const merged = useMemo(
    () => mergeRotorDinParsedResults(selectedRun?.jobs.map((job) => job.results) ?? []),
    [selectedRun],
  );
  const tabs = useMemo(() => availableTabs(merged), [merged]);
  const [activeTab, setActiveTab] = useState<ResultTab>('campbell');
  const [selectedMode, setSelectedMode] = useState(0);
  const [selectedOrbitSection, setSelectedOrbitSection] = useState(0);

  useEffect(() => {
    if (!tabs.length) return;
    if (!tabs.includes(activeTab)) setActiveTab(tabs[0]);
  }, [tabs, activeTab]);

  useEffect(() => {
    setSelectedMode(0);
    setSelectedOrbitSection(0);
  }, [selectedRun?.manifest.runId]);

  if (!selectedRun) {
    return (
      <div className="results-native-empty">
        <FileText size={44}/>
        <h2>No qualified run selected</h2>
        <p>Execute a qualified RotorDin analysis from Analysis Plan. The Results Workspace will display only native data stored by that run manifest.</p>
      </div>
    );
  }

  const manifest = selectedRun.manifest;
  const analyses = selectedRun.jobs.map((job) => job.analysis).join(', ');
  const solverHash = manifest.solver.executableHash?.slice(0, 16) ?? '—';

  return (
    <div className="results-native-layout">
      <div className="run-context native">
        <div><span>Run ID</span><strong>{manifest.runId}</strong></div>
        <div><span>Native analyses</span><strong>{analyses || '—'}</strong></div>
        <div><span>Solver hash</span><strong>{solverHash}</strong></div>
        <div><span>Finished</span><strong>{manifest.finishedAt ? new Date(manifest.finishedAt).toLocaleString() : '—'}</strong></div>
        <div><span>Status</span><span className={`result-status ${manifest.status === 'success' ? 'success' : 'danger'}`}>{manifest.status.toUpperCase()}</span></div>
      </div>

      <div className="results-native-heading">
        <div>
          <h1><BarChart3 size={22}/> Results Workspace</h1>
          <p>Native RotorDin outputs parsed from the selected run manifest. No demonstration curves are used.</p>
        </div>
        <div className="result-run-summary">
          <span>{manifest.artifacts.length} artifacts</span>
          <span>{selectedRun.jobs.length} native jobs</span>
          <span>{Object.keys(manifest.qualifications ?? {}).length} qualifications</span>
        </div>
      </div>

      <div className="result-tabs">
        {tabs.map((tab) => {
          const Icon = resultIcon(tab);
          return <button key={tab} className={activeTab === tab ? 'active' : ''} onClick={() => setActiveTab(tab)}><Icon size={16}/>{TAB_LABELS[tab]}</button>;
        })}
      </div>

      <div className="results-native-body">
        {activeTab === 'campbell' && merged.campbell && <CampbellPanel data={merged.campbell}/>}
        {activeTab === 'stability' && merged.campbell && <StabilityPanel data={merged.campbell}/>}
        {activeTab === 'response' && merged.unbalance && <ResponsePanel data={merged.unbalance}/>}
        {activeTab === 'modes' && merged.modes && <ModesPanel modes={merged.modes.modes} selected={selectedMode} onSelect={setSelectedMode}/>}
        {activeTab === 'line' && merged.elasticLine && <ElasticLinePanel data={merged.elasticLine} staticData={merged.staticLateral}/>}
        {activeTab === 'map' && merged.criticalMap && <CriticalMapPanel data={merged.criticalMap}/>}
        {activeTab === 'orbit' && merged.orbit && <OrbitPanel data={merged.orbit} selected={selectedOrbitSection} onSelect={setSelectedOrbitSection}/>}
      </div>

      <div className="result-source-strip">
        {selectedRun.jobs.map((job) => <span key={job.qualificationId}><CircleDot size={12}/>{job.analysis}: {job.sectionNames.join(', ')}</span>)}
      </div>
    </div>
  );
}
