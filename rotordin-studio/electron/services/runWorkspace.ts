import { app } from 'electron';
import { createHash } from 'node:crypto';
import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';
import type { PrepareRunRequest, PrepareRunResponse } from '../contracts';
import {
  createRunId,
  createRunManifestDraft,
  planRunWorkspace,
  type RunArtifact,
} from '../../src/runs/model';

const hashText = (text: string) => createHash('sha256').update(text, 'utf8').digest('hex');

async function createUniqueWorkspace(reference: string): Promise<{
  runId: string;
  root: string;
}> {
  const runRoot = path.join(app.getPath('userData'), 'runs');
  await mkdir(runRoot, { recursive: true });

  for (let sequence = 1; sequence <= 999; sequence += 1) {
    const runId = createRunId(reference, new Date(), sequence);
    const root = path.join(runRoot, runId);
    try {
      await mkdir(root, { recursive: false });
      return { runId, root };
    } catch (error) {
      const code = (error as NodeJS.ErrnoException).code;
      if (code === 'EEXIST') continue;
      throw error;
    }
  }

  throw new Error('RUN-WORKSPACE-001: Unable to allocate a unique run directory.');
}

export async function prepareRunWorkspace(request: PrepareRunRequest): Promise<PrepareRunResponse> {
  const allocated = await createUniqueWorkspace(request.project.reference);
  const workspace = planRunWorkspace(allocated.runId);

  const absolute = {
    inputDir: path.join(allocated.root, '01_Input'),
    solverDir: path.join(allocated.root, '02_Solvers'),
    resultDir: path.join(allocated.root, '03_Results'),
    logDir: path.join(allocated.root, '04_Logs'),
    tempDir: path.join(allocated.root, '05_Temp'),
  };

  await Promise.all(Object.values(absolute).map((directory) => mkdir(directory, { recursive: true })));

  const projectText = JSON.stringify(request.project, null, 2) + '\n';
  const projectPath = path.join(absolute.inputDir, 'project.json');
  await writeFile(projectPath, projectText, 'utf8');

  const projectHash = hashText(projectText);
  const projectArtifact: RunArtifact = {
    kind: 'input',
    relativePath: '01_Input/project.json',
    sha256: projectHash,
    sizeBytes: Buffer.byteLength(projectText),
    modifiedAt: new Date().toISOString(),
  };

  const manifest = createRunManifestDraft({
    runId: allocated.runId,
    projectId: request.project.id,
    projectReference: request.project.reference,
    solver: request.solver,
    startedAt: new Date().toISOString(),
  });

  manifest.projectHash = projectHash;
  manifest.artifacts.push(projectArtifact);

  const manifestPath = path.join(allocated.root, 'run_manifest.json');
  await writeFile(manifestPath, JSON.stringify(manifest, null, 2) + '\n', 'utf8');

  return {
    workspaceAbsolutePath: allocated.root,
    workspace,
    manifest,
  };
}
