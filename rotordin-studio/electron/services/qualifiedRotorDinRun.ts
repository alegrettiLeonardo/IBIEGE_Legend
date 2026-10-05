import { app } from 'electron';
import { createHash } from 'node:crypto';
import { access, mkdir, readFile, stat, writeFile } from 'node:fs/promises';
import path from 'node:path';
import type {
  ExecuteQualifiedRunRequest,
  ExecuteQualifiedRunResponse,
  QualifiedRunJobResponse,
} from '../contracts';
import { parseCampbellCriticalSpeeds, parseRotorDinMarkedSections } from '../../src/adapters/rotordinResults';
import { parseRotorDinResultData } from '../../src/adapters/rotordinResultData';
import { serializeRotorDinNative } from '../../src/adapters/rotordinNative';
import {
  nativeJobsForQualifiedAnalyses,
  type AnalysisQualification,
  type QualifiedAnalysis,
} from '../../src/runs/qualification';
import type { RunArtifact, RunJobRecord, RunManifest } from '../../src/runs/model';
import { prepareRunWorkspace } from './runWorkspace';
import { runQualifiedProcess } from './processRunner';

const sha256Buffer = (data: Uint8Array | string) =>
  createHash('sha256').update(data).digest('hex');

const toPosix = (value: string) => value.split(path.sep).join('/');

async function sha256File(filePath: string): Promise<string> {
  return sha256Buffer(await readFile(filePath));
}

async function exists(filePath: string): Promise<boolean> {
  try {
    await access(filePath);
    return true;
  } catch {
    return false;
  }
}

async function resolveSolverExecutable(): Promise<string> {
  const executableName = process.platform === 'win32' ? 'rotordin.exe' : 'rotordin';
  const candidates = [
    process.env.ROTORDIN_SOLVER,
    path.join(process.resourcesPath, 'solver', executableName),
    path.join(app.getAppPath(), 'solver', executableName),
  ].filter((value): value is string => Boolean(value));

  for (const candidate of candidates) {
    if (await exists(candidate)) return path.resolve(candidate);
  }

  throw new Error(
    'RUN-SOLVER-001: Qualified RotorDin executable was not found. '
    + 'Build the vendored solver or set ROTORDIN_SOLVER.',
  );
}

function solverEnvironment(executable: string): NodeJS.ProcessEnv {
  const env = { ...process.env };
  if (process.platform === 'win32') {
    const runtime = path.join(path.dirname(executable), 'runtime');
    env.PATH = [runtime, env.PATH ?? ''].filter(Boolean).join(path.delimiter);
  }
  return env;
}

async function appendArtifact(
  manifest: RunManifest,
  root: string,
  absolutePath: string,
  kind: RunArtifact['kind'],
): Promise<RunArtifact> {
  const info = await stat(absolutePath);
  const artifact: RunArtifact = {
    kind,
    relativePath: toPosix(path.relative(root, absolutePath)),
    sha256: await sha256File(absolutePath),
    sizeBytes: info.size,
    modifiedAt: info.mtime.toISOString(),
  };
  manifest.artifacts.push(artifact);
  return artifact;
}

async function persistManifest(root: string, manifest: RunManifest): Promise<void> {
  await writeFile(
    path.join(root, 'run_manifest.json'),
    JSON.stringify(manifest, null, 2) + '\n',
    'utf8',
  );
}

function analysisForSerializer(record: AnalysisQualification): QualifiedAnalysis {
  return record.analysis;
}

export async function executeQualifiedRotorDinRun(
  request: ExecuteQualifiedRunRequest,
): Promise<ExecuteQualifiedRunResponse> {
  if (request.analyses.length === 0) {
    throw new Error('RUN-QUAL-003: Select at least one qualified analysis.');
  }

  const records = nativeJobsForQualifiedAnalyses(request.analyses);
  const prepared = await prepareRunWorkspace({
    project: request.project,
    solver: 'rotordin',
  });

  const root = prepared.workspaceAbsolutePath;
  const manifest = prepared.manifest;
  const executable = await resolveSolverExecutable();
  manifest.status = 'running';
  manifest.solver.executablePath = executable;
  manifest.solver.executableHash = await sha256File(executable);
  manifest.qualifications = Object.fromEntries(
    request.analyses.map((analysis) => [
      analysis,
      records.find((record) =>
        record.analysis === analysis
        || (analysis === 'stability' && record.nativeJob === 'campbell'))?.qualificationId
        ?? 'UNKNOWN',
    ]),
  );
  manifest.jobs = [];
  await persistManifest(root, manifest);

  const responses: QualifiedRunJobResponse[] = [];
  const inputHashes: string[] = [];

  try {
    for (let index = 0; index < records.length; index += 1) {
      const record = records[index];
      const jobName = `${String(index + 1).padStart(2, '0')}_${record.nativeJob}`;
      const jobDir = path.join(root, '02_Solvers', jobName);
      const resultDir = path.join(root, '03_Results', record.nativeJob);
      await mkdir(jobDir, { recursive: false });
      await mkdir(resultDir, { recursive: true });

      const serialization = serializeRotorDinNative(
        request.project,
        analysisForSerializer(record),
      );
      const inputPath = path.join(jobDir, 'input.txt');
      await writeFile(inputPath, serialization.input, 'utf8');
      const inputArtifact = await appendArtifact(manifest, root, inputPath, 'input');
      const inputHash = inputArtifact.sha256 ?? sha256Buffer(serialization.input);
      inputHashes.push(`${record.nativeJob}:${inputHash}`);

      const job: RunJobRecord = {
        analysis: record.analysis,
        qualificationId: record.qualificationId,
        flags: record.flags,
        inputSha256: inputHash,
        status: 'running',
        sections: [],
      };
      manifest.jobs.push(job);
      await persistManifest(root, manifest);

      const processResult = await runQualifiedProcess({
        executable,
        args: record.flags,
        cwd: jobDir,
        timeoutMs: 300_000,
        stdinText: serialization.input,
        qualificationId: record.qualificationId,
        env: solverEnvironment(executable),
      });

      const stdoutPath = path.join(jobDir, 'stdout.out');
      const stderrPath = path.join(jobDir, 'stderr.log');
      await writeFile(stdoutPath, processResult.stdout, 'utf8');
      await writeFile(stderrPath, processResult.stderr, 'utf8');
      const stdoutArtifact = await appendArtifact(manifest, root, stdoutPath, 'output');
      const stderrArtifact = await appendArtifact(manifest, root, stderrPath, 'log');

      job.stdoutSha256 = stdoutArtifact.sha256;
      job.stderrSha256 = stderrArtifact.sha256;
      job.exitCode = processResult.exitCode;

      if (processResult.timedOut) {
        job.status = 'failed';
        throw new Error(`RUN-TIMEOUT-001: RotorDin ${record.nativeJob} exceeded 300 s.`);
      }
      if (processResult.exitCode !== 0) {
        job.status = 'failed';
        throw new Error(
          `RUN-SOLVER-002: RotorDin ${record.nativeJob} failed with exit code ${processResult.exitCode}. `
          + (processResult.stderr.trim() || 'See stderr.log.'),
        );
      }

      const sections = parseRotorDinMarkedSections(processResult.stdout);
      for (const [name, body] of Object.entries(sections)) {
        const safe = name.replace(/[\\/]/g, '_');
        const sectionPath = path.join(resultDir, safe);
        await writeFile(sectionPath, body, 'utf8');
        await appendArtifact(manifest, root, sectionPath, 'output');
      }

      job.sections = Object.keys(sections).sort();
      job.status = 'success';

      const criticalSpeeds = parseCampbellCriticalSpeeds(sections['campbell.out'] ?? '');
      const results = parseRotorDinResultData(sections, criticalSpeeds);
      const parsedResultPath = path.join(resultDir, 'parsed_results.json');
      const parsedResultText = JSON.stringify(results, null, 2) + '\n';
      await writeFile(parsedResultPath, parsedResultText, 'utf8');
      const parsedArtifact = await appendArtifact(manifest, root, parsedResultPath, 'output');

      job.parsedResultPath = toPosix(path.relative(root, parsedResultPath));
      job.parsedResultSha256 = parsedArtifact.sha256;

      responses.push({
        analysis: record.analysis,
        qualificationId: record.qualificationId,
        flags: [...record.flags],
        sectionNames: job.sections,
        criticalSpeeds,
        parsedResultPath: job.parsedResultPath,
        results,
      });
      await persistManifest(root, manifest);
    }

    manifest.inputHash = sha256Buffer(inputHashes.sort().join('\n'));
    manifest.status = 'success';
    manifest.exitCode = 0;
    manifest.finishedAt = new Date().toISOString();
    await persistManifest(root, manifest);

    return {
      workspaceAbsolutePath: root,
      manifest,
      jobs: responses,
    };
  } catch (error) {
    manifest.status = 'failed';
    manifest.exitCode = manifest.jobs?.at(-1)?.exitCode ?? -1;
    manifest.finishedAt = new Date().toISOString();
    manifest.warnings.push(error instanceof Error ? error.message : String(error));
    await persistManifest(root, manifest);
    throw error;
  }
}
