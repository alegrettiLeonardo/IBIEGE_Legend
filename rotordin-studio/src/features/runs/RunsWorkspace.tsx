import { useEffect, useMemo, useState } from 'react';
import {
  BarChart3,
  CheckCircle2,
  Clock3,
  FileCode2,
  FileText,
  FolderOpen,
  RefreshCcw,
  Search,
  ShieldCheck,
  TerminalSquare,
} from 'lucide-react';
import type { InspectRunResponse, QualifiedRunSummary, ReadRunArtifactResponse } from '../../../electron/contracts';
import type { RunArtifact } from '../../runs/model';
import { compareQualifiedRuns, type RunComparison } from '../../runs/comparison';
import { useProject } from '../../state/ProjectContext';

type DetailTab = 'artifacts' | 'logs' | 'traceability' | 'compare';

function formatDate(value?: string): string {
  if (!value) return '—';
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
}

function shortHash(value?: string): string {
  return value ? `${value.slice(0, 12)}…` : '—';
}

function formatElapsed(milliseconds?: number): string {
  if (milliseconds == null) return '—';
  const seconds = milliseconds / 1000;
  if (seconds < 60) return `${seconds.toFixed(2)} s`;
  const minutes = Math.floor(seconds / 60);
  return `${minutes}m ${(seconds - minutes * 60).toFixed(1)}s`;
}

function formatBytes(bytes?: number): string {
  if (bytes == null) return '—';
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 ** 2) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / 1024 ** 2).toFixed(2)} MB`;
}

function statusClass(status: string): string {
  return status === 'success' ? 'success'
    : status === 'failed' ? 'danger'
      : status === 'warning' ? 'warning'
        : 'neutral';
}

function artifactLabel(artifact: RunArtifact): string {
  const name = artifact.relativePath.split('/').at(-1) ?? artifact.relativePath;
  return `${artifact.kind.toUpperCase()} • ${name}`;
}

function isPreviewable(artifact: RunArtifact): boolean {
  return /\.(?:txt|out|log|json|csv|dat)$/i.test(artifact.relativePath);
}

function RunHistoryTable({
  rows,
  selectedRunId,
  onSelect,
}: {
  rows: QualifiedRunSummary[];
  selectedRunId?: string;
  onSelect: (runId: string) => void;
}) {
  return (
    <div className="run-manifest-table">
      <table>
        <thead>
          <tr>
            <th>Run ID</th>
            <th>Started</th>
            <th>Analyses</th>
            <th>Status</th>
            <th>Elapsed</th>
            <th>Input hash</th>
            <th>Solver hash</th>
            <th>Artifacts</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((row) => (
            <tr key={row.runId} className={row.runId === selectedRunId ? 'selected' : ''} onClick={() => onSelect(row.runId)}>
              <td><strong>{row.runId}</strong><small>{row.projectReference}</small></td>
              <td>{formatDate(row.startedAt)}</td>
              <td>{row.analyses.join(', ') || '—'}</td>
              <td><span className={`result-status ${statusClass(row.status)}`}>{row.status.toUpperCase()}</span></td>
              <td>{formatElapsed(row.elapsedMs)}</td>
              <td className="hash-cell">{shortHash(row.inputHash)}</td>
              <td className="hash-cell">{shortHash(row.solverHash)}</td>
              <td>{row.artifactCount}{row.hasResults ? ' • results' : ''}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function ArtifactsPanel({
  runId,
  inspection,
  onPreview,
  onReveal,
}: {
  runId: string;
  inspection: InspectRunResponse;
  onPreview: (artifact: RunArtifact) => void;
  onReveal: (artifact: RunArtifact) => void;
}) {
  const artifacts = [
    ...inspection.manifest.artifacts,
    {
      kind: 'manifest' as const,
      relativePath: 'run_manifest.json',
      sizeBytes: undefined,
      sha256: undefined,
    },
  ];

  return (
    <div className="real-artifact-grid">
      {artifacts.map((artifact) => (
        <div className="real-artifact-card" key={artifact.relativePath}>
          <FileText size={25}/>
          <div>
            <strong>{artifactLabel(artifact)}</strong>
            <span>{artifact.relativePath}</span>
            <small>{formatBytes(artifact.sizeBytes)} • {shortHash(artifact.sha256)}</small>
          </div>
          <div className="artifact-actions">
            {isPreviewable(artifact) && <button onClick={() => onPreview(artifact)}>Preview</button>}
            <button onClick={() => onReveal(artifact)}>Reveal</button>
          </div>
        </div>
      ))}
    </div>
  );
}

function LogsPanel({
  inspection,
  preview,
  onPreview,
}: {
  inspection: InspectRunResponse;
  preview?: ReadRunArtifactResponse;
  onPreview: (artifact: RunArtifact) => void;
}) {
  const logs = inspection.manifest.artifacts.filter((artifact) =>
    artifact.kind === 'log' || /(?:stdout\.out|stderr\.log)$/i.test(artifact.relativePath));

  return (
    <div className="logs-panel">
      <aside>
        <h4>Run logs</h4>
        {logs.length === 0 && <p>No registered log artifacts.</p>}
        {logs.map((artifact) => (
          <button key={artifact.relativePath} className={preview?.relativePath === artifact.relativePath ? 'active' : ''} onClick={() => onPreview(artifact)}>
            <TerminalSquare size={14}/>
            <span>{artifact.relativePath}</span>
          </button>
        ))}
      </aside>
      <section className="log-preview">
        {preview ? (
          <>
            <header><strong>{preview.relativePath}</strong><span>{formatBytes(preview.bytes)}{preview.truncated ? ' • preview truncated' : ''}</span></header>
            <pre>{preview.text || '(empty file)'}</pre>
          </>
        ) : (
          <div className="log-empty">Select a registered log or stdout/stderr artifact.</div>
        )}
      </section>
    </div>
  );
}

function TraceabilityPanel({ inspection }: { inspection: InspectRunResponse }) {
  const manifest = inspection.manifest;
  return (
    <div className="traceability-real">
      <div className="trace-block">
        <ShieldCheck size={23}/>
        <div><span>Solver executable hash</span><strong>{manifest.solver.executableHash ?? '—'}</strong></div>
      </div>
      <div className="trace-block">
        <FileCode2 size={23}/>
        <div><span>Project hash</span><strong>{manifest.projectHash ?? '—'}</strong></div>
      </div>
      <div className="trace-block">
        <FileCode2 size={23}/>
        <div><span>Aggregate input hash</span><strong>{manifest.inputHash ?? '—'}</strong></div>
      </div>
      <div className="trace-block">
        <Clock3 size={23}/>
        <div><span>Started / finished</span><strong>{formatDate(manifest.startedAt)} → {formatDate(manifest.finishedAt)}</strong></div>
      </div>
      <div className="trace-block full">
        <CheckCircle2 size={23}/>
        <div><span>Qualification IDs</span><strong>{Object.entries(manifest.qualifications ?? {}).map(([analysis, id]) => `${analysis}: ${id}`).join(' • ') || '—'}</strong></div>
      </div>

      <div className="job-trace-table">
        <table>
          <thead><tr><th>Analysis</th><th>Flags</th><th>Status</th><th>Exit</th><th>Input SHA-256</th><th>Parsed result SHA-256</th><th>Native sections</th></tr></thead>
          <tbody>{(manifest.jobs ?? []).map((job) => <tr key={`${job.analysis}-${job.qualificationId}`}>
            <td>{job.analysis}<small>{job.qualificationId}</small></td>
            <td>{job.flags.join(' ')}</td>
            <td><span className={`result-status ${statusClass(job.status)}`}>{job.status}</span></td>
            <td>{job.exitCode ?? '—'}</td>
            <td className="hash-cell">{job.inputSha256}</td>
            <td className="hash-cell">{job.parsedResultSha256 ?? '—'}</td>
            <td>{job.sections.join(', ') || '—'}</td>
          </tr>)}</tbody>
        </table>
      </div>
    </div>
  );
}

function ComparisonPanel({
  comparison,
  compareRunId,
  candidates,
  onCompareRunId,
}: {
  comparison?: RunComparison;
  compareRunId?: string;
  candidates: QualifiedRunSummary[];
  onCompareRunId: (runId: string) => void;
}) {
  return (
    <div className="run-compare-panel">
      <div className="compare-controls">
        <label>Compare against
          <select value={compareRunId ?? ''} onChange={(event) => onCompareRunId(event.target.value)}>
            <option value="">Select another qualified run…</option>
            {candidates.filter((row) => row.hasResults).map((row) => <option key={row.runId} value={row.runId}>{row.runId} • {row.projectReference}</option>)}
          </select>
        </label>
      </div>
      {!comparison ? <div className="compare-empty">Choose another run with persisted native results.</div> : (
        <>
          <div className="compare-hash-row">
            <span className={comparison.solverHashSame ? 'same' : 'different'}>Solver hash {comparison.solverHashSame ? 'SAME' : 'DIFF'}</span>
            <span className={comparison.projectHashSame ? 'same' : 'different'}>Project hash {comparison.projectHashSame ? 'SAME' : 'DIFF'}</span>
            <span className={comparison.inputHashSame ? 'same' : 'different'}>Input hash {comparison.inputHashSame ? 'SAME' : 'DIFF'}</span>
          </div>
          <p className="compare-scope">
            Common: {comparison.commonAnalyses.join(', ') || 'none'}
            {comparison.baselineOnlyAnalyses.length ? ` • only selected: ${comparison.baselineOnlyAnalyses.join(', ')}` : ''}
            {comparison.candidateOnlyAnalyses.length ? ` • only comparison: ${comparison.candidateOnlyAnalyses.join(', ')}` : ''}
          </p>
          <div className="result-table-wrap compare-table">
            <table><thead><tr><th>Group</th><th>Metric</th><th>Selected</th><th>Comparison</th><th>Δ</th><th>Δ %</th></tr></thead>
              <tbody>{comparison.metrics.map((item, index) => <tr key={`${item.group}-${item.metric}-${index}`}>
                <td>{item.group}</td>
                <td>{item.metric}</td>
                <td>{item.baseline.toLocaleString(undefined, { maximumSignificantDigits: 7 })} {item.unit}</td>
                <td>{item.candidate.toLocaleString(undefined, { maximumSignificantDigits: 7 })} {item.unit}</td>
                <td>{item.delta.toLocaleString(undefined, { maximumSignificantDigits: 7 })} {item.unit}</td>
                <td>{item.relativePercent == null ? '—' : `${item.relativePercent.toFixed(4)}%`}</td>
              </tr>)}</tbody>
            </table>
          </div>
        </>
      )}
    </div>
  );
}

export default function RunsWorkspace({ onResults }: { onResults: () => void }) {
  const {
    runHistory,
    selectedRun,
    refreshRunHistory,
    selectQualifiedRun,
    loadQualifiedRunData,
    inspectRun,
    readRunArtifact,
    revealRunArtifact,
    openRunWorkspace,
  } = useProject();

  const [query, setQuery] = useState('');
  const [selectedRunId, setSelectedRunId] = useState<string>();
  const [inspection, setInspection] = useState<InspectRunResponse>();
  const [activeTab, setActiveTab] = useState<DetailTab>('artifacts');
  const [preview, setPreview] = useState<ReadRunArtifactResponse>();
  const [compareRunId, setCompareRunId] = useState<string>();
  const [comparison, setComparison] = useState<RunComparison>();

  const filtered = useMemo(() => {
    const needle = query.trim().toLowerCase();
    if (!needle) return runHistory;
    return runHistory.filter((row) => [
      row.runId,
      row.projectReference,
      row.status,
      row.analyses.join(' '),
      row.inputHash ?? '',
      row.solverHash ?? '',
    ].some((value) => value.toLowerCase().includes(needle)));
  }, [runHistory, query]);

  useEffect(() => {
    const wanted = selectedRunId ?? selectedRun?.manifest.runId ?? filtered[0]?.runId;
    if (wanted && wanted !== selectedRunId) setSelectedRunId(wanted);
  }, [filtered, selectedRun, selectedRunId]);

  useEffect(() => {
    if (!selectedRunId) {
      setInspection(undefined);
      return;
    }
    void inspectRun(selectedRunId).then((value) => {
      setInspection(value);
      setPreview(undefined);
      setComparison(undefined);
      setCompareRunId(undefined);
    });
  }, [selectedRunId]);

  const selectedSummary = runHistory.find((row) => row.runId === selectedRunId);

  const previewArtifact = async (artifact: RunArtifact) => {
    if (!selectedRunId) return;
    const value = await readRunArtifact(selectedRunId, artifact.relativePath);
    if (value) {
      setPreview(value);
      setActiveTab('logs');
    }
  };

  const compareAgainst = async (runId: string) => {
    setCompareRunId(runId || undefined);
    setComparison(undefined);
    if (!runId || !selectedRunId || runId === selectedRunId) return;
    const [baseline, candidate] = await Promise.all([
      loadQualifiedRunData(selectedRunId),
      loadQualifiedRunData(runId),
    ]);
    if (baseline && candidate) setComparison(compareQualifiedRuns(baseline, candidate));
  };

  const openResults = async () => {
    if (!selectedRunId || !selectedSummary?.hasResults) return;
    if (await selectQualifiedRun(selectedRunId)) onResults();
  };

  return (
    <div className="runs-real-layout">
      <div className="runs-heading real">
        <div><h1><FileText size={21}/> Runs & Reports</h1><p>Manifest-backed history, native artifacts, logs, hashes and engineering run comparison.</p></div>
        <div>
          <button className="button" onClick={() => { void refreshRunHistory(); }}><RefreshCcw size={15}/>Refresh</button>
          <div className="search-box large"><Search size={16}/><input value={query} onChange={(event) => setQuery(event.target.value)} placeholder="Search run, project, analysis or hash..."/></div>
        </div>
      </div>

      <section className="card run-history real">
        <header className="card-header"><div className="card-title-wrap"><FileText size={21}/><div><h2>Run History</h2><p>{filtered.length} valid RotorDin manifests</p></div></div></header>
        <div className="card-body"><RunHistoryTable rows={filtered} selectedRunId={selectedRunId} onSelect={setSelectedRunId}/></div>
      </section>

      <section className="card run-detail real">
        <header className="card-header">
          <div className="card-title-wrap"><FileCode2 size={21}/><div><h2>{selectedRunId ? `Run ${selectedRunId}` : 'Run detail'}</h2><p>{selectedSummary?.analyses.join(', ') || 'Select a run manifest'}</p></div></div>
          <div className="card-actions">
            {selectedSummary && <span className={`result-status ${statusClass(selectedSummary.status)}`}>{selectedSummary.status.toUpperCase()}</span>}
            <button className="button" disabled={!selectedRunId} onClick={() => { if (selectedRunId) void openRunWorkspace(selectedRunId); }}><FolderOpen size={15}/>Open workspace</button>
            <button className="button primary" disabled={!selectedSummary?.hasResults} onClick={() => { void openResults(); }}><BarChart3 size={15}/>Open Results</button>
          </div>
        </header>

        {!inspection ? <div className="run-detail-empty">Select a run to inspect its real run_manifest.json.</div> : (
          <>
            <div className="run-detail-tabs">
              {([
                ['artifacts', 'Artifacts'],
                ['logs', 'Logs'],
                ['traceability', 'Hashes & Traceability'],
                ['compare', 'Compare Runs'],
              ] as const).map(([id, label]) => <button key={id} className={activeTab === id ? 'active' : ''} onClick={() => setActiveTab(id)}>{label}</button>)}
            </div>
            <div className="run-detail-body">
              {activeTab === 'artifacts' && <ArtifactsPanel runId={inspection.manifest.runId} inspection={inspection} onPreview={(artifact) => { void previewArtifact(artifact); }} onReveal={(artifact) => { void revealRunArtifact(inspection.manifest.runId, artifact.relativePath); }}/>}
              {activeTab === 'logs' && <LogsPanel inspection={inspection} preview={preview} onPreview={(artifact) => { void previewArtifact(artifact); }}/>}
              {activeTab === 'traceability' && <TraceabilityPanel inspection={inspection}/>}
              {activeTab === 'compare' && <ComparisonPanel comparison={comparison} compareRunId={compareRunId} candidates={runHistory.filter((row) => row.runId !== inspection.manifest.runId)} onCompareRunId={(runId) => { void compareAgainst(runId); }}/>}
            </div>
          </>
        )}
      </section>
    </div>
  );
}
