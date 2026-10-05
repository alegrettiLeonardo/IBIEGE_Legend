import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { matrixViewForBearing, toLegacyShaftGrid, type AnalysisKind } from './domain';
import { qualificationFor } from './runs/qualification';
import { useProject } from './state/ProjectContext';
import {
  Activity,
  BarChart3,
  Box,
  Check,
  CheckCircle2,
  ChevronDown,
  ChevronLeft,
  ChevronRight,
  CircleDot,
  ClipboardList,
  Copy,
  Database,
  Download,
  Eye,
  FileBarChart2,
  FileCode2,
  FileDown,
  FileText,
  Folder,
  FolderOpen,
  Gauge,
  Home,
  Layers3,
  Link2,
  Lock,
  Maximize2,
  Minus,
  MousePointer2,
  MoveDown,
  MoveUp,
  Orbit,
  PanelLeftClose,
  Play,
  Plus,
  RefreshCcw,
  Ruler,
  Save,
  Search,
  Settings,
  ShieldCheck,
  SlidersHorizontal,
  Sparkles,
  Target,
  Trash2,
  Triangle,
  Upload,
  Weight,
  Wrench,
  X,
  ZoomIn,
  ZoomOut,
  type LucideIcon,
} from 'lucide-react';

type Page =
  | 'Overview'
  | 'Shaft'
  | 'Bearings'
  | 'Masses'
  | 'Excitations'
  | 'Analysis'
  | 'Results'
  | 'Runs'
  | 'Reports'
  | 'Settings';

type Tone = 'success' | 'info' | 'warning' | 'danger' | 'neutral';

const navItems: Array<{ page: Page; icon: LucideIcon }> = [
  { page: 'Overview', icon: Home },
  { page: 'Shaft', icon: Gauge },
  { page: 'Bearings', icon: CircleDot },
  { page: 'Masses', icon: Weight },
  { page: 'Excitations', icon: Activity },
  { page: 'Analysis', icon: BarChart3 },
  { page: 'Results', icon: FileBarChart2 },
  { page: 'Runs', icon: Play },
  { page: 'Reports', icon: FileText },
  { page: 'Settings', icon: Settings },
];


const analysisCards = [
  { key: 'campbell', title: 'Campbell Diagram', subtitle: 'Natural frequencies vs. speed', icon: BarChart3, rows: [['Speed Range', '0 – 6,000 rpm'], ['Speed Divisions', '200'], ['Modes', '1 – 10'], ['Damping', 'Included']] },
  { key: 'modes', title: 'Modes', subtitle: 'Mode shapes and frequencies', icon: Activity, rows: [['Modes to Compute', '1 – 20'], ['Frequency Range', '0 – 6,000 rpm'], ['Normalization', 'Mass normalized'], ['Include Damping', 'Yes']] },
  { key: 'response', title: 'Unbalance Response', subtitle: 'Synchronous response analysis', icon: Target, rows: [['Speed Range', '0 – 6,000 rpm'], ['Speed Divisions', '200'], ['Unbalance Position', 'All masses'], ['Response Type', 'Displacement']] },
  { key: 'line', title: 'Elastic Line', subtitle: 'Shaft deflection shape', icon: Activity, rows: [['Speed', '375 rpm'], ['Load Case', 'Synchronous'], ['Display', 'X, Y (2D)'], ['Scaling', 'Automatic']] },
  { key: 'map', title: 'Critical Speed Map', subtitle: 'Critical speeds identification', icon: BarChart3, rows: [['Speed Range', '0 – 6,000 rpm'], ['Speed Divisions', '100'], ['Modes', '1 – 20'], ['Identification', 'Automatic']] },
  { key: 'orbit', title: 'Orbit Analysis', subtitle: 'Shaft orbit at bearings', icon: Orbit, rows: [['Speed Range', '0 – 6,000 rpm'], ['Speed Divisions', '200'], ['Bearing Positions', 'BRG 1, BRG 2'], ['Display', 'X–Y Orbit']] },
  { key: 'stability', title: 'Stability / Log Decrement', subtitle: 'Stability analysis and damping', icon: ShieldCheck, rows: [['Speed Range', '0 – 6,000 rpm'], ['Speed Divisions', '100'], ['Perturbation', 'Modal'], ['Output', 'Log Decrement']] },
];

const runs = [
  ['#007', '03/09/2026 13:15', 'Critical Speed + Response', 'RD-Solver v2026.1', 'Success', '12.4 s', 'a3f9c7e1', 'Final design check'],
  ['#006', '03/09/2026 11:42', 'Unbalance Response', 'RD-Solver v2026.1', 'Qualified', '28.7 s', 'd4b1e9a5', 'Tolerance study'],
  ['#005', '02/09/2026 16:03', 'Modal Analysis', 'RD-Solver v2026.0', 'Success', '7.1 s', '9c2d4f8a', 'Updated bearing data'],
  ['#004', '02/09/2026 14:21', 'Critical Speed', 'RD-Solver v2026.0', 'Warning', '10.6 s', 'f7a9b2c3', 'Coarse mesh'],
  ['#003', '01/09/2026 10:17', 'Transient Response', 'RD-Solver v2026.0', 'Success', '41.3 s', '3e6d1a90', 'Startup simulation'],
  ['#002', '31/08/2026 15:08', 'Unbalance Response', 'RD-Solver v2025.3', 'Failed', '6.8 s', 'b8d4e2f1', 'Numerical divergence'],
  ['#001', '31/08/2026 09:54', 'Modal Analysis', 'RD-Solver v2025.3', 'Success', '5.2 s', 'c1a7d9e0', 'Initial model'],
];

function App() {
  const { project: currentProject, saved, validation, markDirty, saveProject, openProject } = useProject();
  const [page, setPage] = useState<Page>('Overview');
  const [activeSegment, setActiveSegment] = useState(6);
  const [activeBearing, setActiveBearing] = useState(0);
  const [activeRun, setActiveRun] = useState(0);
  const [enabledAnalyses, setEnabledAnalyses] = useState<Record<string, boolean>>(
    Object.fromEntries(analysisCards.map((item) => [item.key, true])),
  );

  useEffect(() => {
    const imported = Object.fromEntries(currentProject.analyses.map((item) => [item.kind, item.enabled]));
    setEnabledAnalyses(Object.fromEntries(analysisCards.map((item) => [
      item.key,
      item.key === 'response'
        ? Boolean(imported['unbalance-response'])
        : item.key === 'line'
          ? Boolean(imported['elastic-line'])
          : item.key === 'map'
            ? Boolean(imported['critical-speed-map'])
            : Boolean(imported[item.key]),
    ])));
  }, [currentProject.id, currentProject.analyses]);

  const pageContent = useMemo(() => {
    switch (page) {
      case 'Shaft': return <ShaftPage selected={activeSegment} onSelect={setActiveSegment} />;
      case 'Bearings': return <BearingsPage selected={activeBearing} onSelect={setActiveBearing} />;
      case 'Masses':
      case 'Excitations': return <MassesPage />;
      case 'Analysis': return <AnalysisPage enabled={enabledAnalyses} onToggle={(key) => setEnabledAnalyses((prev) => ({ ...prev, [key]: !prev[key] }))} />;
      case 'Results': return <ResultsPage />;
      case 'Runs':
      case 'Reports': return <RunsPage selected={activeRun} onSelect={setActiveRun} />;
      case 'Settings': return <SettingsPage />;
      default: return <OverviewPage onNavigate={setPage} />;
    }
  }, [page, activeSegment, activeBearing, activeRun, enabledAnalyses]);

  return (
    <div className="app-shell">
      <Sidebar page={page} onNavigate={setPage} />
      <div className="app-main">
        <TopBar saved={saved} valid={validation.valid} onSave={() => { void saveProject(); }} onOpen={() => { void openProject(); }} onValidate={() => undefined} />
        <ProjectStrip />
        <main className="page-content" onInput={markDirty}>{pageContent}</main>
        <StatusBar />
      </div>
    </div>
  );
}

function Sidebar({ page, onNavigate }: { page: Page; onNavigate: (page: Page) => void }) {
  return (
    <aside className="sidebar">
      <div className="brand">
        <div className="brand-mark"><Orbit size={24} /></div>
        <div><strong>RotorDin Studio</strong><span>Rotordynamic Analysis & Design</span></div>
      </div>
      <nav className="side-nav">
        {navItems.map(({ page: item, icon: Icon }) => (
          <button key={item} className={page === item ? 'nav-item active' : 'nav-item'} onClick={() => onNavigate(item)}>
            <Icon size={19} /><span>{item}</span>
          </button>
        ))}
      </nav>
      <div className="sidebar-footer">
        <div className="mini-brand"><Orbit size={22} /><div><strong>RotorDin Studio</strong><span>v1.0.0</span></div></div>
        <PanelLeftClose size={18} />
      </div>
    </aside>
  );
}

function TopBar({ saved, valid, onSave, onOpen, onValidate }: { saved: boolean; valid: boolean; onSave: () => void; onOpen: () => void; onValidate: () => void }) {
  const { project: currentProject } = useProject();
  return (
    <header className="topbar">
      <button className="project-select" onClick={onOpen}><Folder size={18} /> Project: <strong>{currentProject.reference}</strong><ChevronDown size={16} /></button>
      <div className="topbar-actions">
        <Button icon={Save} onClick={onSave}>Save</Button>
        <Badge tone={saved ? 'success' : 'warning'}>{saved ? 'Saved' : 'Unsaved'}</Badge>
        <Button icon={Check} onClick={onValidate}>Validate Model</Button>
        <Badge tone={valid ? 'success' : 'danger'}>{valid ? 'Valid' : 'Invalid'}</Badge>
        <div className="window-actions"><Minus size={16} /><Maximize2 size={15} /><X size={17} /></div>
      </div>
    </header>
  );
}

function ProjectStrip() {
  const { project: currentProject } = useProject();
  const items = [
    ['Ref.', currentProject.reference],
    ['Component', currentProject.component ?? currentProject.description],
    ['Line', currentProject.line ?? '—'],
    ['Frame', currentProject.frame ?? '—'],
    ['Poles', String(currentProject.poles ?? '—')],
    ['Frequency', currentProject.frequencyHz != null ? `${currentProject.frequencyHz} Hz` : '—'],
    ['Nominal Speed', currentProject.nominalSpeedRpm != null ? `${currentProject.nominalSpeedRpm} rpm` : '—'],
  ];
  return <div className="project-strip">{items.map(([label, value]) => <div key={label}><span>{label}</span><strong>{value}</strong></div>)}</div>;
}

function StatusBar() {
  const { project: currentProject, projectFilePath, saved } = useProject();
  const fileLabel = projectFilePath?.split(/[\\/]/).pop() ?? `${currentProject.reference}.rdin.json`;
  return (
    <footer className="statusbar">
      <div className="status-left"><span className="status-dot" />Ready<span className="sep" />Project: {currentProject.reference}<span className="sep" />File: {fileLabel}<span className="sep" />{saved ? 'Saved' : 'Unsaved changes'}</div>
      <div className="status-right"><Database size={15} /><span className="status-dot small" />Connected<span className="sep" />Units: metric (mm, kg, N)<ChevronDown size={14} /></div>
    </footer>
  );
}

function Card({ title, subtitle, icon: Icon, className = '', actions, children }: { title?: string; subtitle?: string; icon?: LucideIcon; className?: string; actions?: ReactNode; children: ReactNode }) {
  return (
    <section className={'card ' + className}>
      {(title || actions) && <header className="card-header"><div className="card-title-wrap">{Icon && <Icon size={21} />}<div>{title && <h2>{title}</h2>}{subtitle && <p>{subtitle}</p>}</div></div>{actions && <div className="card-actions">{actions}</div>}</header>}
      <div className="card-body">{children}</div>
    </section>
  );
}

function Button({ icon: Icon, children, primary = false, danger = false, disabled = false, onClick }: { icon?: LucideIcon; children?: ReactNode; primary?: boolean; danger?: boolean; disabled?: boolean; onClick?: () => void }) {
  return <button disabled={disabled} onClick={onClick} className={'button ' + (primary ? 'primary ' : '') + (danger ? 'danger' : '')}>{Icon && <Icon size={15} />}{children}</button>;
}

function Badge({ tone = 'neutral', children }: { tone?: Tone; children: ReactNode }) {
  return <span className={'badge ' + tone}>{tone === 'success' && <CheckCircle2 size={13} />}{tone === 'warning' && <Triangle size={12} />}{children}</span>;
}

function Metric({ icon: Icon, label, value, tone }: { icon: LucideIcon; label: string; value: string; tone?: Tone }) {
  return <div className="metric"><div className={'metric-icon ' + (tone ?? '')}><Icon size={22} /></div><div><span>{label}</span><strong>{value}</strong></div></div>;
}

function ShaftViewer({ compact = false, selected = false }: { compact?: boolean; selected?: boolean }) {
  return (
    <div className={'shaft-viewer ' + (compact ? 'compact' : '')}>
      <svg viewBox="0 0 1000 350" role="img" aria-label="Rotor shaft engineering viewer">
        <defs>
          <linearGradient id="metal" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stopColor="#f9fbfc"/><stop offset="0.45" stopColor="#bfc7ce"/><stop offset="0.55" stopColor="#7e8991"/><stop offset="1" stopColor="#eef1f4"/></linearGradient>
          <linearGradient id="cyan" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stopColor="#67e8f9"/><stop offset="1" stopColor="#14b8c9"/></linearGradient>
        </defs>
        <line x1="75" y1="170" x2="925" y2="170" stroke="#738392" strokeDasharray="8 6" strokeWidth="1" />
        <g fill="url(#metal)" stroke="#263640" strokeWidth="1.7">
          <rect x="85" y="145" width="95" height="50" /><rect x="180" y="150" width="95" height="40" /><rect x="275" y="140" width="52" height="60" /><rect x="327" y="132" width="68" height="76" /><rect x="395" y="125" width="365" height="90" /><path d="M760 125 L810 142 L810 198 L760 215 Z" /><rect x="810" y="145" width="55" height="50" /><rect x="865" y="150" width="45" height="40" /><rect x="910" y="155" width="35" height="30" />
        </g>
        <rect x="390" y="95" width="370" height="150" fill="#cfd5d9" stroke="#55636d" strokeWidth="1.2" opacity="0.92" />
        <rect x="420" y="68" width="310" height="25" fill="url(#cyan)" stroke="#057b85" /><rect x="420" y="247" width="310" height="25" fill="url(#cyan)" stroke="#057b85" /><rect x="330" y="72" width="18" height="196" fill="url(#cyan)" stroke="#057b85" /><rect x="910" y="88" width="18" height="164" fill="url(#cyan)" stroke="#057b85" />
        {selected && <rect x="725" y="122" width="92" height="96" fill="#2d8bd7" opacity="0.15" stroke="#1677c8" strokeWidth="3" strokeDasharray="7 6" />}
        <g fill="none" stroke="#1f2d38" strokeWidth="2"><path d="M200 215 L185 245 L215 245 Z" /><path d="M850 215 L835 245 L865 245 Z" /></g>
        {!compact && <><g stroke="#e53935" fill="#fff" strokeWidth="2"><circle cx="420" cy="37" r="12"/><circle cx="730" cy="37" r="12"/><line x1="420" y1="49" x2="420" y2="68"/><line x1="730" y1="49" x2="730" y2="68"/></g><g fill="#e53935" fontSize="14" fontFamily="sans-serif"><text x="440" y="40">F1</text><text x="750" y="40">F2</text></g><g stroke="#5c6f7e" fill="none" strokeWidth="1.2"><line x1="85" y1="305" x2="330" y2="305"/><line x1="330" y1="305" x2="760" y2="305"/><line x1="760" y1="305" x2="928" y2="305"/><line x1="85" y1="295" x2="85" y2="315"/><line x1="330" y1="295" x2="330" y2="315"/><line x1="760" y1="295" x2="760" y2="315"/><line x1="928" y1="295" x2="928" y2="315"/></g><g fill="#334657" fontSize="14" fontWeight="600" fontFamily="sans-serif"><text x="175" y="329">536.47</text><text x="515" y="329">2,490.53</text><text x="828" y="329">303.0</text></g></>}
        <g fill="#172936" fontSize="13" fontWeight="600" fontFamily="sans-serif"><text x="165" y="265">BRG 1</text><text x="828" y="265">BRG 2</text></g>
      </svg>
    </div>
  );
}

function OverviewPage({ onNavigate }: { onNavigate: (page: Page) => void }) {
  const { project: currentProject, validation } = useProject();
  const totalLength = currentProject.segments.reduce((sum, segment) => sum + segment.lengthMm, 0);
  const analysisTools = [
    ['Campbell Diagram', 'Natural frequencies vs. speed', BarChart3, 'Analysis'],
    ['Modes', 'Mode shapes and frequencies', Activity, 'Analysis'],
    ['Unbalance Response', 'Synchronous response analysis', Target, 'Analysis'],
    ['Elastic Line', 'Shaft deflection shape', Activity, 'Results'],
    ['Critical Speed Map', 'Critical speeds identification', BarChart3, 'Results'],
    ['Orbit Analysis', 'Shaft orbit at bearings', Orbit, 'Results'],
    ['Stability / Log Decrement', 'Stability analysis and damping', ShieldCheck, 'Analysis'],
  ] as const;
  return (
    <div className="overview-grid">
      <Card title="Rotor / Shaft Geometry" icon={Box} className="overview-viewer" actions={<><Button icon={Maximize2}>Fit</Button><Button icon={ZoomIn} /><Button icon={ZoomOut} /><Button primary>2D</Button><Button>3D</Button><Button icon={Settings} /></>}><ShaftViewer /></Card>
      <Card title="Analysis Tools" icon={BarChart3} className="analysis-tools"><div className="tool-list">{analysisTools.map(([name, sub, Icon, dest]) => <button key={name} className="tool-row" onClick={() => onNavigate(dest as Page)}><Icon size={21}/><div><strong>{name}</strong><span>{sub}</span></div><ChevronRight size={17}/></button>)}</div></Card>
      <Card title="Model Summary" icon={ClipboardList} className="summary-card"><div className="metric-grid"><Metric icon={Ruler} label="Shaft Length" value={`${totalLength.toLocaleString('en-US', { maximumFractionDigits: 2 })} mm`}/><Metric icon={Weight} label="Shaft Mass" value="1,705.0 kg"/><Metric icon={CircleDot} label="Bearings" value={String(currentProject.bearings.length)}/><Metric icon={Database} label="Distributed Masses" value={String(currentProject.legacyLoads.filter((load) => load.lengthMm > 0).length)}/><Metric icon={Triangle} label="Concentrated Masses" value={String(currentProject.concentratedMasses.length)}/><Metric icon={Activity} label="Forces / Excitations" value={String(currentProject.forces.length)}/><Metric icon={Triangle} label="Supports" value={String(currentProject.supports.length)}/><Metric icon={Layers3} label="Shaft Segments" value={String(currentProject.segments.length)}/><Metric icon={CheckCircle2} label="Model Status" value={validation.valid ? "Valid" : "Review"} tone={validation.valid ? "success" : "warning"}/></div></Card>
      <Card title="Latest Run" icon={Play} className="latest-run" actions={<Button primary icon={Play} onClick={() => onNavigate('Analysis')}>Run Analysis</Button>}><KeyValue rows={[["Run ID", "#007"],["Analysis Type","Critical Speed + Response"],["Speed Range","100 – 6,000 rpm"],["Frequency Points","50"],["Computation Time","12.4 s"],["Date","03/09/2026 13:15:33"]]} /><div className="latest-status"><span>Status</span><Badge tone="success">Completed</Badge></div><div className="split-actions"><Button icon={FolderOpen} onClick={() => onNavigate('Results')}>View Results</Button><Button icon={FileText}>Open Log</Button></div></Card>
      <Card title="Quick Actions" icon={Sparkles} className="quick-actions"><div className="quick-list"><Button icon={Box} onClick={() => onNavigate('Shaft')}>Edit Shaft Geometry</Button><Button icon={CircleDot} onClick={() => onNavigate('Bearings')}>Configure Bearings</Button><Button icon={Weight} onClick={() => onNavigate('Masses')}>Define Masses</Button><Button icon={Activity} onClick={() => onNavigate('Excitations')}>Set Excitations</Button><Button icon={Settings} onClick={() => onNavigate('Analysis')}>Analysis Settings</Button></div></Card>
    </div>
  );
}

function ShaftPage({ selected, onSelect }: { selected: number; onSelect: (index: number) => void }) {
  const { project: currentProject, updateSegment, addSegment, removeSegment, moveSegment, editIssues } = useProject();
  const rows = toLegacyShaftGrid(currentProject.segments).map((row, index) => {
    const segment = currentProject.segments[index];
    const type = segment.ribbed && (segment.innerDiameterMm ?? 0) > 0
      ? 'Ribbed + Hollow'
      : segment.ribbed
        ? 'Ribbed'
        : (segment.innerDiameterMm ?? 0) > 0
          ? 'Hollow'
          : segment.endOuterDiameterMm
            ? 'Tapered'
            : 'Shaft';
    return [
      String(index + 1), type, String(row.L ?? 0), String(row.D ?? 0), String(row.DPCT ?? 0),
      String(row.A ?? 0), String(row.B ?? 0), String(row.C ?? 0), String(row.NR_COST ?? 0),
      String(row.D_INT ?? 0), String(row.D_F ?? 0),
    ];
  });
  const safeSelected = Math.min(selected, Math.max(0, currentProject.segments.length - 1));
  const segment = currentProject.segments[safeSelected];
  const s = rows[safeSelected];
  const totalLength = currentProject.segments.reduce((sum, item) => sum + item.lengthMm, 0);
  const segmentIssues = editIssues.filter((issue) => !issue.entityId || issue.entityId === segment?.id);
  const numberValue = (value: string) => {
    const parsed = Number(value.replace(',', '.'));
    return Number.isFinite(parsed) ? parsed : 0;
  };
  return (
    <div className="shaft-page-grid">
      <Card title="Shaft Modeler" icon={Box} className="shaft-main" actions={<><Button icon={Maximize2}>Fit</Button><Button icon={ZoomIn}/><Button icon={ZoomOut}/><Button icon={MousePointer2}/><Button icon={Ruler}>Measure</Button><Button icon={Eye}>Layers</Button><Button primary>2D</Button><Button>3D</Button><Button icon={Upload}>Export</Button></>}><ShaftViewer selected /></Card>
      <Card title="Segment Properties" icon={SlidersHorizontal} className="segment-inspector"><div className="inspector-nav"><Button icon={ChevronLeft} onClick={() => onSelect(Math.max(0, safeSelected - 1))}/><strong>Segment {safeSelected + 1} of {rows.length}</strong><Button icon={ChevronRight} onClick={() => onSelect(Math.min(rows.length - 1, safeSelected + 1))}/></div><FormField label="Type" value={s[1]} /><FormField label="Length (L)" value={s[2]} unit="mm" onChange={(value) => updateSegment(segment.id, { lengthMm: numberValue(value) })} /><FormField label="Outer Diameter (D)" value={s[3]} unit="mm" onChange={(value) => updateSegment(segment.id, { outerDiameterMm: numberValue(value) })} /><FormField label="Package Diameter (DPCT)" value={s[4]} unit="mm" onChange={segment.ribbed ? (value) => updateSegment(segment.id, { ribbed: { packageDiameterMm: numberValue(value) } }) : undefined} /><FormField label="Inner Diameter (Dint)" value={s[9]} unit="mm" onChange={(segment.innerDiameterMm ?? 0) > 0 ? (value) => updateSegment(segment.id, { innerDiameterMm: numberValue(value) }) : undefined} /><FormField label="End Diameter (Df)" value={s[10]} unit="mm" onChange={(value) => updateSegment(segment.id, { endOuterDiameterMm: numberValue(value) || undefined })} /><FormField label="Rib Count" value={s[8]} onChange={segment.sectionType === 'ribbed' ? (value) => updateSegment(segment.id, { ribbed: { ribCount: numberValue(value) } }) : undefined} />{segmentIssues.length > 0 ? <div className="inspector-note error-note"><Triangle size={16}/><div><strong>Edit rejected</strong><span>{segmentIssues[0].message}</span></div></div> : <div className="inspector-note"><CheckCircle2 size={16}/><div><strong>Geometry valid</strong><span>Selected segment is physically consistent.</span></div></div>}</Card>
      <div className="shaft-metrics"><Metric icon={Ruler} label="Total Length" value={`${totalLength.toLocaleString('en-US', { maximumFractionDigits: 2 })} mm`}/><Metric icon={Weight} label="Shaft Mass" value="1,705.0 kg"/><Metric icon={Layers3} label="Number of Segments" value={String(rows.length)}/><Metric icon={CheckCircle2} label="Geometry Validation" value={segmentIssues.length ? "Rejected edit" : "Valid"} tone={segmentIssues.length ? "warning" : "success"}/></div>
      <Card title="Shaft Segments" icon={ClipboardList} className="segments-card" actions={<><Button primary icon={Plus} onClick={() => { if (addSegment(safeSelected)) onSelect(safeSelected + 1); }}>Add Segment</Button><Button icon={FileText} onClick={() => { if (addSegment(Math.max(-1, safeSelected - 1))) onSelect(safeSelected); }}>Insert</Button><Button icon={Copy} onClick={() => { if (addSegment(safeSelected)) onSelect(safeSelected + 1); }}>Duplicate</Button><Button icon={Trash2} danger onClick={() => { if (removeSegment(segment.id)) onSelect(Math.max(0, safeSelected - 1)); }}>Delete</Button><Button icon={MoveUp} onClick={() => { if (moveSegment(segment.id, -1)) onSelect(Math.max(0, safeSelected - 1)); }}>Move Up</Button><Button icon={MoveDown} onClick={() => { if (moveSegment(segment.id, 1)) onSelect(Math.min(rows.length - 1, safeSelected + 1)); }}>Move Down</Button></>}><DataTable headers={['#','Type','L [mm]','D [mm]','DPCT','A','B','C','Ribs','Dint [mm]','Df [mm]']} rows={rows} selected={safeSelected} onSelect={onSelect} /></Card>
      <Card title="Visual Options" icon={Eye} className="visual-options">{['Show Bearings','Show Supports','Show Masses / Rotors','Show Loads','Show Dimensions','Show Segment Numbers','Show Centerline'].map((label, i) => <label className="check-row" key={label}><input type="checkbox" defaultChecked={i !== 5}/><span>{label}</span></label>)}<FormField label="View Preset" value="Standard" /></Card>
    </div>
  );
}

function BearingsPage({ selected, onSelect }: { selected: number; onSelect: (index: number) => void }) {
  const { project: currentProject, updateBearing, editIssues } = useProject();
  const safeSelected = Math.min(selected, Math.max(0, currentProject.bearings.length - 1));
  const bearing = currentProject.bearings[safeSelected];
  const matrix = matrixViewForBearing(bearing);
  const tableRows = currentProject.bearings.map((item, index) => {
    const view = matrixViewForBearing(item);
    return [
      String(index + 1),
      item.name,
      String(item.positionMm),
      'Radial',
      view.stiffness.xx.toExponential(3),
      view.stiffness.zz.toExponential(3),
      view.damping.xx.toExponential(3),
      view.damping.zz.toExponential(3),
      item.coefficients.kind === 'legacy-scalar' ? 'Legacy scalar' : item.coefficients.source,
      item.supportId ?? '—',
      'Ready',
    ];
  });
  const bearingIssues = editIssues.filter((issue) => !issue.entityId || issue.entityId === bearing.id);
  const parse = (value: string) => {
    const parsed = Number(value.replace(',', '.'));
    return Number.isFinite(parsed) ? parsed : 0;
  };
  const updateLegacyScalar = (value: string) => {
    if (bearing.coefficients.kind !== 'legacy-scalar') return;
    const stiffness = parse(value);
    updateBearing(bearing.id, {
      coefficients: {
        kind: 'legacy-scalar',
        stiffnessNPerM: stiffness,
        infinite: stiffness === 0,
      },
    });
  };

  return (
    <div className="bearings-grid">
      <Card title="Bearings & Supports" subtitle="Define bearing coefficients, support connections and boundary conditions" icon={Box} className="bearing-main" actions={<><Button primary icon={Plus}>Add Bearing</Button><Button icon={Trash2} danger>Delete</Button><Button icon={Download}>Import Coefficients</Button><Button icon={Link2}>Link Support</Button></>}><div className="subsection-label">Shaft Overview</div><ShaftViewer compact /><div className="section-toolbar"><h3><Database size={20}/> Bearings</h3><div className="search-box"><Search size={15}/><input placeholder="Search bearings..."/></div></div><DataTable headers={['#','Name','Position [mm]','Type','Kxx [N/m]','Kzz [N/m]','Cxx [N·s/m]','Czz [N·s/m]','Source','Support','Status']} rows={tableRows} selected={safeSelected} onSelect={onSelect} /></Card>
      <Card title="Bearing Properties" icon={Database} className="bearing-inspector"><div className="inline-fields"><FormField label="Name" value={bearing.name}/><FormField label="Position" value={String(bearing.positionMm)} unit="mm" onChange={(value) => updateBearing(bearing.id, { positionMm: parse(value) })}/></div><FormField label="Bearing Type" value="Radial (Journal)" /><h3 className="minor-title">Coefficient Source</h3><div className="source-grid"><SourceTile icon={Database} title="Legacy scalar" subtitle="Exact IBIEGE coefficient" active={bearing.coefficients.kind === 'legacy-scalar'}/><SourceTile icon={FileText} title="File-based" subtitle="Target RotorDin extension"/><SourceTile icon={Activity} title="Speed-dependent" subtitle="Target RotorDin extension" active={bearing.coefficients.kind === 'matrix' && bearing.coefficients.source === 'speed-dependent'}/><SourceTile icon={Link2} title="Linked Support" subtitle="From support entity"/></div>{bearing.coefficients.kind === 'matrix' && bearing.coefficients.table?.length ? <div className="info-box"><Activity size={18}/><div><strong>Speed-dependent table</strong><span>{bearing.coefficients.table.length} points • {bearing.coefficients.table[0].speedRpm}–{bearing.coefficients.table[bearing.coefficients.table.length - 1].speedRpm} rpm • source {bearing.coefficients.sourceFile ?? 'embedded'}</span></div></div> : null}<MatrixEditor title="Stiffness Matrix K (N/m)" values={[matrix.stiffness.xx.toExponential(3),matrix.stiffness.xz.toExponential(3),matrix.stiffness.zx.toExponential(3),matrix.stiffness.zz.toExponential(3)]} editable={[bearing.coefficients.kind === 'legacy-scalar',false,false,bearing.coefficients.kind === 'legacy-scalar']} onChange={(_, value) => updateLegacyScalar(value)} /><MatrixEditor title="Damping Matrix C (N·s/m)" values={[matrix.damping.xx.toExponential(3),matrix.damping.xz.toExponential(3),matrix.damping.zx.toExponential(3),matrix.damping.zz.toExponential(3)]} /><h3 className="minor-title">Validation</h3>{bearingIssues.length ? <div className="inspector-note error-note"><Triangle size={16}/><div><strong>Edit rejected</strong><span>{bearingIssues[0].message}</span></div></div> : <div className="validation-grid">{['Finite values','Legacy scalar preserved','Linked to support','Ready for analysis'].map((item) => <span key={item}><CheckCircle2 size={15}/>{item}</span>)}</div>}</Card>
      <Card title="Supports" subtitle="Define support entities and link them to bearings" icon={Triangle} className="supports-card" actions={<><Button primary icon={Plus}>Add Support</Button><Button icon={Trash2} danger>Delete</Button><Button icon={Wrench}>Edit Support</Button></>}><DataTable headers={['#','Name','Type','Location [mm]','Linked Bearings','Description','Status']} rows={currentProject.supports.map((support, index) => [String(index + 1), support.name, support.type, String(support.locationMm), currentProject.bearings.filter((item) => item.supportId === support.id).map((item) => item.name).join(', ') || '—', support.name === 'SUP 1' ? 'Housing, left side' : 'Housing, right side', 'Valid'])} /></Card>
    </div>
  );
}

function MassesPage() {
  const { project: currentProject, updateLegacyLoad, setRotorStack, editIssues } = useProject();
  const [selectedMass, setSelectedMass] = useState(2);
  const safeSelected = Math.min(selectedMass, Math.max(0, currentProject.legacyLoads.length - 1));
  const selectedLoad = currentProject.legacyLoads[safeSelected];
  const rows = currentProject.legacyLoads.map((load, index) => [
    String(index + 1),
    String(load.startMm),
    String(load.lengthMm),
    String(load.massKg),
    String(load.outerDiameterMm ?? '—'),
    String(load.innerDiameterMm ?? '—'),
    load.isRotorStack ? 'Yes' : 'No',
    load.umpEnabled ? 'Yes' : 'No',
  ]);
  const loadIssues = editIssues.filter((issue) => !issue.entityId || issue.entityId === selectedLoad.id);
  const parse = (value: string) => {
    const parsed = Number(value.replace(',', '.'));
    return Number.isFinite(parsed) ? parsed : 0;
  };
  const totalMass = currentProject.legacyLoads.reduce((sum, load) => sum + load.massKg, 0);
  const forceRows = currentProject.forces.map((force, index) => [
    String(index + 1),
    String(force.positionMm),
    force.type === 'synchronous' ? 'Synchronous' : force.type === 'unbalance' ? 'Unbalance' : 'Custom',
    `${force.magnitude} ${force.unit}`,
    String(force.phaseDeg),
    force.direction,
  ]);
  const responseRows = currentProject.responsePoints.map((point, index) => [
    String(index + 1),
    String(point.positionMm),
    'Displacement',
    `Radial (${point.coordinate})`,
    point.note ?? '—',
  ]);

  return (
    <div className="masses-grid">
      <Card title="Masses & Excitations" subtitle="Define distributed masses, rotor packages, forces and response points" icon={Weight} className="mass-header" actions={<><Button icon={Download}>Import</Button><Button icon={Copy}>Duplicate</Button><Button icon={Trash2} danger>Delete</Button><Button primary icon={Plus}>Add Mass</Button><Button primary icon={Plus}>Add Force</Button><Button primary icon={Plus}>Add Response Point</Button></>}><ShaftViewer compact /></Card>
      <Card title="Distributed Masses / Rotor Package" icon={Weight} className="distributed-masses" actions={<><Button primary icon={Plus}>Add Mass</Button><Button icon={Copy}>Duplicate</Button><Button icon={Trash2} danger>Delete</Button></>}><DataTable headers={['#','Xi [mm]','LC [mm]','Mass [kg]','Dext [mm]','Dint [mm]','Package','UMP']} rows={rows} selected={safeSelected} onSelect={setSelectedMass} /></Card>
      <Card title="Selected Mass" icon={Settings} className="mass-inspector"><FormField label="Position Xi" value={String(selectedLoad.startMm)} unit="mm" onChange={(value) => updateLegacyLoad(selectedLoad.id, { startMm: parse(value) })}/><FormField label="Length (LC)" value={String(selectedLoad.lengthMm)} unit="mm" onChange={(value) => updateLegacyLoad(selectedLoad.id, { lengthMm: parse(value) })}/><FormField label="Mass" value={String(selectedLoad.massKg)} unit="kg" onChange={(value) => updateLegacyLoad(selectedLoad.id, { massKg: parse(value) })}/><FormField label="Outer Diameter" value={String(selectedLoad.outerDiameterMm ?? 0)} unit="mm" onChange={(value) => updateLegacyLoad(selectedLoad.id, { outerDiameterMm: parse(value) })}/><FormField label="Inner Diameter" value={String(selectedLoad.innerDiameterMm ?? 0)} unit="mm" onChange={(value) => updateLegacyLoad(selectedLoad.id, { innerDiameterMm: parse(value) || undefined })}/><label className="toggle-line"><span>Rotor Package</span><input type="radio" name="rotor-stack" checked={selectedLoad.isRotorStack} onChange={() => setRotorStack(selectedLoad.id)}/></label><label className="toggle-line"><span>Unbalance Mass Prop.</span><input type="checkbox" checked={Boolean(selectedLoad.umpEnabled)} onChange={(event) => updateLegacyLoad(selectedLoad.id, { umpEnabled: event.target.checked })}/></label>{loadIssues.length ? <div className="inspector-note error-note"><Triangle size={16}/><div><strong>Edit rejected</strong><span>{loadIssues[0].message}</span></div></div> : <div className="info-box"><Settings size={18}/><div><strong>{selectedLoad.isRotorStack ? 'Rotor Package' : 'Distributed Mass'}</strong><span>{selectedLoad.isRotorStack ? 'Explicitly persisted in the React domain; no longer inferred from MSFlexGrid cell color.' : 'Legacy Xi/LC/KG semantics preserved for BIEGE/FLECHA compatibility.'}</span></div></div>}</Card>
      <Card title="Forces & Response Points" icon={Activity} className="force-response"><div className="dual-section"><div><div className="section-toolbar"><h3><Activity size={18}/> Forces / Excitations</h3><Button icon={Plus}>Add Force</Button></div><DataTable headers={['#','Xi [mm]','Type','Magnitude','Phase [°]','Direction']} rows={forceRows} /></div><div><div className="section-toolbar"><h3><Triangle size={18}/> Response Points</h3><Button icon={Plus}>Add Response Point</Button></div><DataTable headers={['#','Xi [mm]','Type','Direction','Note']} rows={responseRows} /></div></div></Card>
      <div className="mass-summary"><Metric icon={Weight} label="Total Distributed Mass" value={`${totalMass.toLocaleString('en-US')} kg`}/><Metric icon={Settings} label="Rotor Package Count" value={String(currentProject.legacyLoads.filter((load) => load.isRotorStack).length)}/><Metric icon={Database} label="Concentrated Masses" value={String(currentProject.concentratedMasses.length)}/><Metric icon={Activity} label="Number of Forces" value={String(currentProject.forces.length)}/><Metric icon={Triangle} label="Response Points" value={String(currentProject.responsePoints.length)}/></div>
    </div>
  );
}

function AnalysisPage({ enabled, onToggle }: { enabled: Record<string, boolean>; onToggle: (key: string) => void }) {
  const { project: currentProject, executeQualifiedRun, validation } = useProject();
  const settings = currentProject.settings;
  const formatRange = (a?: number, b?: number) => a != null && b != null ? `${a.toLocaleString()} – ${b.toLocaleString()} rpm` : 'Not configured';
  const displayRows = (key: string, fallback: string[][]): string[][] => {
    switch (key) {
      case 'campbell':
        return [
          ['Speed Range', formatRange(settings?.campbell?.initialRpm, settings?.campbell?.finalRpm)],
          ['Speed Divisions', String(settings?.campbell?.divisions ?? '—')],
          ['Rotations', String(settings?.campbell?.rotations ?? '—')],
          ['Interpolation', String(settings?.campbell?.interpolationPoints ?? '—')],
        ];
      case 'modes':
        return [
          ['Modes to Compute', String(settings?.modes?.modes ?? '—')],
          ['Nominal Speed', settings?.speed?.nominalRpm != null ? `${settings.speed.nominalRpm} rpm` : '—'],
          ['Young\'s Modulus', settings?.material?.youngsModulusPa != null ? settings.material.youngsModulusPa.toExponential(3) : '—'],
          ['Density', settings?.material?.densityKgPerM3 != null ? `${settings.material.densityKgPerM3} kg/m³` : '—'],
        ];
      case 'response':
        return [
          ['Speed Range', formatRange(settings?.unbalanceResponse?.initialRpm, settings?.unbalanceResponse?.finalRpm)],
          ['Speed Divisions', String(settings?.unbalanceResponse?.divisions ?? '—')],
          ['Modes', String(settings?.unbalanceResponse?.modes ?? '—')],
          ['Unbalance Positions', String(currentProject.forces.filter((force) => force.type === 'unbalance').length)],
        ];
      case 'line':
        return [
          ['Speed', settings?.speed?.nominalRpm != null ? `${settings.speed.nominalRpm} rpm` : '—'],
          ['Inclination', `${settings?.elasticLine?.inclinationDeg ?? 0}°`],
          ['Gravity', `${settings?.elasticLine?.gravityMPerS2 ?? 9.81} m/s²`],
          ['Display', 'X, Z (2D)'],
        ];
      case 'map':
        return [
          ['Initial Stiffness', settings?.criticalSpeedMap?.initialStiffnessNPerM != null ? settings.criticalSpeedMap.initialStiffnessNPerM.toExponential(2) : '—'],
          ['Divisions', String(settings?.criticalSpeedMap?.divisions ?? '—')],
          ['Maps', String(settings?.graphics?.maps ?? '—')],
          ['Identification', 'Automatic'],
        ];
      default:
        return fallback;
    }
  };
  const [prepareStatus, setPrepareStatus] = useState<string>();
  const keyToAnalysis: Record<string, AnalysisKind> = {
    campbell: 'campbell',
    modes: 'modes',
    response: 'unbalance-response',
    line: 'elastic-line',
    map: 'critical-speed-map',
    orbit: 'orbit',
    stability: 'stability',
  };
  const runSelected = async () => {
    if (!validation.valid) {
      setPrepareStatus('Model validation must pass before solver execution.');
      return;
    }
    const selected = analysisCards
      .filter((item) => enabled[item.key])
      .map((item) => keyToAnalysis[item.key]);
    if (!selected.length) {
      setPrepareStatus('Select at least one analysis.');
      return;
    }
    setPrepareStatus('Running qualified RotorDin analyses…');
    const result = await executeQualifiedRun(selected);
    setPrepareStatus(result
      ? `Completed ${result.manifest.runId}: ${result.jobs.map((job) => job.analysis).join(', ')}. Artifacts are frozen in the run workspace.`
      : 'Run blocked or failed. Review the validation/qualification message.');
  };
  const count = Object.values(enabled).filter(Boolean).length;
  return (
    <div className="analysis-layout">
      <div className="analysis-top"><div><h1><BarChart3/> Analysis Plan</h1><p>Select and configure analyses to run. Each analysis uses the current model and shared settings.</p></div><div className="analysis-top-metrics"><div><CheckCircle2/> <span>Analysis Package<strong>Ready</strong></span></div><div><BarChart3/> <span>Enabled Analyses<strong>{count} / 7</strong></span></div><div><Triangle/> <span>Warnings<strong>0</strong></span></div><Button primary icon={Play} onClick={() => { void runSelected(); }}>Run Selected</Button></div></div>{prepareStatus && <div className="run-preparation-note">{prepareStatus}</div>}
      <div className="analysis-cards">{analysisCards.map((item) => <AnalysisCard key={item.key} item={{ ...item, rows: displayRows(item.key, item.rows) }} enabled={enabled[item.key]} qualification={qualificationFor(keyToAnalysis[item.key])} onToggle={() => onToggle(item.key)} />)}<div className="more-analysis"><Plus size={30}/><strong>More Analyses</strong><span>Additional specialized analyses coming soon.</span></div></div>
      <Card title="Shared Analysis Settings" icon={Settings} className="shared-settings"><h3 className="minor-title">Material Properties</h3><FormField label="Young's Modulus (E)" value="2.10e+11" unit="Pa"/><FormField label="Density (ρ)" value="7,800" unit="kg/m³"/><FormField label="Poisson's Ratio (ν)" value="0.30"/><h3 className="minor-title">Analysis Controls</h3><FormField label="Speed Range" value="0 – 6,000" unit="rpm"/><FormField label="Speed Divisions" value="200"/><FormField label="Frequency Points" value="50"/><FormField label="Modes (Default)" value="1 – 20"/><h3 className="minor-title">Plot & Display</h3>{['Show Grid','Show Critical Speeds','Show Bearings'].map((label) => <label className="check-row" key={label}><input type="checkbox" defaultChecked/><span>{label}</span></label>)}<Button icon={RefreshCcw}>Reset to Defaults</Button></Card>
    </div>
  );
}

function AnalysisCard({ item, enabled, qualification, onToggle }: { item: typeof analysisCards[number]; enabled: boolean; qualification: ReturnType<typeof qualificationFor>; onToggle: () => void }) {
  const Icon = item.icon;
  return <div className={'analysis-card ' + (enabled ? '' : 'disabled-card')}><div className="analysis-card-title"><Icon size={30}/><div><strong>{item.title}</strong><span>{item.subtitle}</span></div><button className={'toggle ' + (enabled ? 'on' : '')} onClick={onToggle}><span/></button></div><KeyValue rows={item.rows} /><div className="analysis-card-footer"><div className="analysis-card-badges"><Badge tone={enabled ? 'success' : 'neutral'}>{enabled ? 'Valid' : 'Disabled'}</Badge><Badge tone={qualification.status === 'qualified' ? 'info' : 'warning'}>{qualification.status === 'qualified' ? 'Qualified' : 'Qualification pending'}</Badge></div><span>{qualification.qualificationId}</span></div></div>;
}

function ResultsPage() {
  return (
    <div className="results-layout">
      <div className="run-context"><div><span>Run ID</span><strong>#007</strong></div><div><span>Analysis Type</span><strong>Critical Speed + Response</strong></div><div><span>Solver</span><strong>Direct (FE)</strong></div><div><span>Date</span><strong>03/09/2026 13:15</strong></div><div><span>Status</span><Badge tone="success">Completed</Badge></div></div>
      <div className="results-toolbar"><div><h1><BarChart3/> Results Workspace</h1><p>Review analysis results, identify critical speeds and dynamic response.</p></div><div><Button icon={Upload}>Export</Button><Button icon={BarChart3}>Compare Runs</Button><Button icon={Wrench}>Annotate</Button><Button icon={FolderOpen}>Open Files</Button><Button icon={Settings}/></div></div>
      <Card title="Campbell Diagram" icon={BarChart3} className="campbell-card" actions={<><label className="mini-check"><input type="checkbox" defaultChecked/> Rotor Modes</label><label className="mini-check red"><input type="checkbox" defaultChecked/> Synchronous Orders</label><label className="mini-check"><input type="checkbox" defaultChecked/> Grid</label><Button icon={ZoomIn}/><Button icon={Maximize2}/></>}><CampbellChart /></Card>
      <Card title="Critical Speed Results" icon={Target} className="critical-card"><DataTable headers={['Mode','Critical Speed [rpm]','Frequency [Hz]','Whirl','Log Dec.','Status']} rows={[["1","6,420","107.0","Forward","0.032","OK"],["2","12,480","208.0","Forward","0.028","OK"],["3","21,360","356.0","Backward","0.015","Check"],["4","28,940","482.3","Forward","0.021","OK"],["5","34,800","580.0","Backward","0.012","Check"],["6","37,920","632.0","Forward","0.018","OK"]]} /></Card>
      <Card title="Unbalance Response at Probe 1" icon={Activity} className="response-card"><ResponseChart /></Card>
      <Card title="Mode Details & Orbit" icon={Orbit} className="orbit-card"><div className="orbit-content"><OrbitChart/><KeyValue rows={[["Mode Number","2"],["Critical Speed","12,480 rpm"],["Frequency","208.0 Hz"],["Whirl Direction","Forward"],["Log Decrement","0.028"],["Damping Ratio (ζ)","0.0045"],["Dominant Motion","Lateral (X–Y)"]]} /></div></Card>
    </div>
  );
}

function RunsPage({ selected, onSelect }: { selected: number; onSelect: (index: number) => void }) {
  return (
    <div className="runs-layout">
      <div className="runs-heading"><div><h1><Play/> Runs & Reports</h1><p>Manage analysis runs, view results, and generate reports.</p></div><div><Button primary icon={Plus}>New Run</Button><Button icon={RefreshCcw}>Refresh</Button><div className="search-box large"><Search size={16}/><input placeholder="Search runs, notes, or hash..."/></div></div></div>
      <Card title="Run History" icon={FileText} className="run-history"><DataTable headers={['Run ID','Started','Analyses','Solver','Status','Elapsed','Input Hash','Notes']} rows={runs} selected={selected} onSelect={onSelect} statusColumn={4}/></Card>
      <Card title={'Run ' + runs[selected][0]} icon={FileText} className="run-detail" actions={<><Badge tone={runs[selected][4] === 'Failed' ? 'danger' : 'success'}>{runs[selected][4]}</Badge><Badge tone="info">Qualified</Badge></>}><div className="detail-heading"><div><strong>{runs[selected][2]}</strong><span>{runs[selected][7]}</span></div><div><span>Started<strong>{runs[selected][1]}</strong></span><span>Elapsed Time<strong>{runs[selected][5]}</strong></span></div></div><div className="tabs"><button className="active"><FileText size={15}/>Artifacts</button><button><BarChart3 size={15}/>Analysis Summary</button><button><Activity size={15}/>Key Results</button><button><FileCode2 size={15}/>Log Preview</button></div><div className="artifact-grid"><Artifact icon={FileText} title="Input File" file="run007_input.rdin" meta="1.2 MB"/><Artifact icon={Database} title="Output File" file="run007_output.h5" meta="8.4 MB"/><Artifact icon={FileBarChart2} title="Plots (12)" file="Natural frequencies, modes, response" meta="12 files"/><Artifact icon={FileText} title="Solver Log" file="run007_solver.log" meta="156 KB"/><Artifact icon={FileCode2} title="Run Manifest" file="run007_manifest.json" meta="18 KB"/><Artifact icon={FileDown} title="Report" file="run007_report.pdf" meta="2.1 MB"/></div><div className="run-actions"><Button primary icon={FolderOpen}>Open Artifacts</Button><Button icon={FileText}>Generate Report</Button><Button icon={FileDown}>Export PDF</Button><Button icon={BarChart3}>Compare Runs</Button><Button icon={Play}>Re-run</Button></div></Card>
      <Card title="Run Traceability" icon={ShieldCheck} className="trace-card"><KeyValue rows={[["Solver Executable","RD-Solver.exe (v2026.1)"],["Solver Hash","5e7c3d9a2b1f6c8e4…"],["Project Hash","a3f9c7e18d4b2e1f0…"],["Exit Code","0"],["Warning Count","0"],["Run Status","Success"],["Qualification","Qualified"],["Environment","Windows 11"],["User","engineer"],["Host","RD-WS-01"]]} /></Card>
    </div>
  );
}

function SettingsPage() {
  return (
    <div className="settings-layout">
      <div className="settings-heading"><div><h1><Settings/> Settings & Integrations</h1><p>Configure external tools, data sources, run workspace and migration qualification checks.</p></div><div><Button icon={Upload}>Import Settings</Button><Button icon={Download}>Export Settings</Button></div></div>
      <Card title="Solver Integrations" subtitle="Configure and validate external solver installations." icon={Box} className="solver-settings"><DataTable headers={['Solver','Status','Installation Path','Actions']} rows={[["RotorDin","Installed","C:\Program Files\RotorDin\bin","Test"],["BIEGE","Installed","C:\Program Files\BIEGE\bin","Test"],["FLECHA","Installed","C:\Program Files\FLECHA\bin","Test"],["FDE","Not Found","C:\Program Files\FDE\bin","Test"]]} statusColumn={1}/><div className="split-actions"><Button icon={ShieldCheck}>Validate Installation</Button><Button icon={FolderOpen}>Open Solvers Folder</Button></div></Card>
      <Card title="Database & Credentials" subtitle="Configure database connections and manage secure credentials." icon={Database} className="db-settings"><div className="db-box"><div className="section-toolbar"><h3><Database size={18}/> Primary Database</h3><Badge tone="success">Connected</Badge></div><KeyValue rows={[["Type","Oracle / ODBC"],["Server","oraclewm"],["Schema","adm_eng"],["Authentication","Credential Vault"],["Status","Connection successful"]]} /></div><div className="vertical-actions"><Button icon={Play}>Test Connection</Button><Button icon={Database}>Configure Connection</Button><Button icon={Lock}>Manage Credentials</Button></div><div className="vault-box"><Lock size={18}/><div><strong>Credential Vault</strong><span>Secure storage for solver and database credentials.</span></div><Badge tone="success">Vault Unlocked</Badge></div></Card>
      <Card title="Run Workspace" subtitle="Configure per-run working directory structure and file management." icon={Folder} className="workspace-settings"><FormField label="Workspace Root" value="D:\RotorDin\Runs"/><div className="folder-tree"><div><ChevronDown size={15}/><Folder size={16}/><strong>{'{Project}_{Date}_{RunID}'}</strong></div>{[['01_Input','Project files, geometry, bearing data'],['02_Solvers','RotorDin, BIEGE, FLECHA, FDE inputs/outputs'],['03_Results','Processed results, plots, reports'],['04_Logs','Run logs and solver output'],['05_Temp','Temporary files (auto-cleanup)']].map(([folder,note]) => <div className="tree-child" key={folder}><Folder size={15}/><strong>{folder}</strong><span>{note}</span></div>)}</div><div className="split-actions"><Button icon={FolderOpen}>Open Workspace Folder</Button><Button icon={Settings}>Run Folder Settings</Button></div></Card>
      <Card title="Migration Qualification" subtitle="Validate installation and data migration readiness." icon={ShieldCheck} className="qualification-card"><DataTable headers={['Validation Item','Status','Details','Actions']} rows={[["Project round-trip","Passed","Load → Save → Reload successful","Run Test"],["Geometry golden","Passed","Reference geometries match • tol 1e-6","Run Test"],["Bearings custom K","Passed","Custom coefficients match","Run Test"],["Solver output freshness","Passed","Per-run output verified","Run Test"],["FDE cleanup","Passed","No shared temp files","Run Test"],["Credential rotation","Pending","Rotation recommended every 90 days","Rotate Now"]]} statusColumn={1}/></Card>
    </div>
  );
}

function FormField({ label, value, unit, onChange }: { label: string; value: string; unit?: string; onChange?: (value: string) => void }) {
  return <label className="form-field"><span>{label}</span><div><input value={value} readOnly={!onChange} onChange={(event) => onChange?.(event.target.value)}/>{unit && <em>{unit}</em>}</div></label>;
}

function KeyValue({ rows }: { rows: readonly (readonly string[])[] | string[][] }) {
  return <div className="key-value">{rows.map((row) => <div key={row[0]}><span>{row[0]}</span><strong>{row[1]}</strong></div>)}</div>;
}

function DataTable({ headers, rows, selected, onSelect, statusColumn }: { headers: string[]; rows: string[][]; selected?: number; onSelect?: (index: number) => void; statusColumn?: number }) {
  return <div className="table-wrap"><table><thead><tr>{headers.map((h) => <th key={h}>{h}</th>)}</tr></thead><tbody>{rows.map((row, i) => <tr key={i} className={selected === i ? 'selected' : ''} onClick={() => onSelect?.(i)}>{row.map((cell, j) => <td key={i + '-' + j}>{statusColumn === j ? <Badge tone={cell === 'Failed' || cell === 'Not Found' ? 'danger' : cell === 'Warning' || cell === 'Pending' ? 'warning' : cell === 'Qualified' ? 'info' : 'success'}>{cell}</Badge> : cell}</td>)}</tr>)}</tbody></table></div>;
}

function SourceTile({ icon: Icon, title, subtitle, active }: { icon: LucideIcon; title: string; subtitle: string; active?: boolean }) {
  return <div className={'source-tile ' + (active ? 'active' : '')}><Icon size={20}/><div><strong>{title}</strong><span>{subtitle}</span></div></div>;
}

function MatrixEditor({ title, values, editable = [false, false, false, false], onChange }: { title: string; values: string[]; editable?: boolean[]; onChange?: (index: number, value: string) => void }) {
  const input = (index: number) => <input value={values[index]} readOnly={!editable[index]} onChange={(event) => onChange?.(index, event.target.value)}/>;
  return <div className="matrix-editor"><div className="matrix-title"><strong>{title}</strong><Badge tone="info">Symmetric</Badge></div><div className="matrix-labels"><span></span><span>X</span><span>Z</span><span>X</span>{input(0)}{input(1)}<span>Z</span>{input(2)}{input(3)}</div></div>;
}

function Artifact({ icon: Icon, title, file, meta }: { icon: LucideIcon; title: string; file: string; meta: string }) {
  return <div className="artifact"><Icon size={28}/><div><strong>{title}</strong><span>{file}</span><small>{meta}</small></div><Download size={17}/></div>;
}

function CampbellChart() {
  return <div className="chart-shell"><svg viewBox="0 0 760 300"><g className="chart-grid">{[40,90,140,190,240].map((y) => <line key={y} x1="55" y1={y} x2="730" y2={y}/>)}{[55,160,265,370,475,580,685].map((x) => <line key={x} x1={x} y1="25" x2={x} y2="255"/>)}</g><line className="axis" x1="55" y1="255" x2="730" y2="255"/><line className="axis" x1="55" y1="25" x2="55" y2="255"/><path className="mode-line" d="M55 205 C160 200, 250 198, 340 190 S570 165,730 135"/><path className="mode-line" d="M55 160 C160 158, 245 160, 350 150 S585 112,730 84"/><path className="mode-line" d="M55 90 C180 90, 260 85, 350 72 S580 45,730 25"/><path className="order-line" d="M55 255 L730 70"/><path className="order-line" d="M55 255 L730 155"/><path className="order-line" d="M55 255 L730 215"/><g className="chart-labels"><text x="20" y="28">Hz</text><text x="660" y="287">Speed [rpm]</text><text x="700" y="60">3X</text><text x="700" y="145">2X</text><text x="700" y="208">1X</text></g></svg></div>;
}

function ResponseChart() {
  return <div className="chart-shell"><svg viewBox="0 0 760 265"><g className="chart-grid">{[35,75,115,155,195,235].map((y) => <line key={y} x1="55" y1={y} x2="730" y2={y}/>)}{[55,160,265,370,475,580,685].map((x) => <line key={x} x1={x} y1="20" x2={x} y2="235"/>)}</g><line className="axis" x1="55" y1="235" x2="730" y2="235"/><line className="axis" x1="55" y1="20" x2="55" y2="235"/><path className="response-line" d="M55 225 C95 205,125 178,150 72 C168 182,195 200,255 207 C340 210,360 195,382 80 C400 193,465 200,560 196 C605 190,625 166,650 75 C670 178,690 190,730 192"/><path className="response-line alt" d="M55 230 C110 215,135 190,150 108 C170 195,210 212,290 214 C345 214,365 205,382 112 C404 203,470 210,565 205 C610 201,632 185,650 112 C675 193,700 200,730 202"/></svg></div>;
}

function OrbitChart() {
  return <div className="orbit-plot"><svg viewBox="0 0 280 240"><g className="chart-grid">{[40,80,120,160,200].map((y) => <line key={y} x1="30" y1={y} x2="250" y2={y}/>)}{[50,90,130,170,210].map((x) => <line key={x} x1={x} y1="20" x2={x} y2="220"/>)}</g><line className="axis" x1="30" y1="120" x2="250" y2="120"/><line className="axis" x1="140" y1="20" x2="140" y2="220"/><ellipse cx="140" cy="120" rx="82" ry="38" transform="rotate(-28 140 120)" fill="none" stroke="#1677c8" strokeWidth="3"/><circle cx="140" cy="120" r="4" fill="#1677c8"/></svg><Badge tone="info">Forward Whirl</Badge></div>;
}

export default App;
