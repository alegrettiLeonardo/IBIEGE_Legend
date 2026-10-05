import { dialog } from 'electron';
import { rename, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import type { RotorProject } from '../../src/domain';
import type {
  ProjectOpenResponse,
  ProjectSaveRequest,
  ProjectSaveResponse,
} from '../contracts';

interface RotorProjectFile {
  schemaVersion: 1;
  project: RotorProject;
}

function isProject(value: unknown): value is RotorProject {
  if (!value || typeof value !== 'object') return false;
  const candidate = value as Partial<RotorProject>;
  return typeof candidate.id === 'string'
    && typeof candidate.reference === 'string'
    && typeof candidate.description === 'string'
    && Array.isArray(candidate.segments)
    && Array.isArray(candidate.bearings)
    && Array.isArray(candidate.legacyLoads);
}

function decodeProjectFile(text: string): RotorProject {
  const parsed: unknown = JSON.parse(text);

  if (
    parsed
    && typeof parsed === 'object'
    && (parsed as Partial<RotorProjectFile>).schemaVersion === 1
    && isProject((parsed as RotorProjectFile).project)
  ) {
    return (parsed as RotorProjectFile).project;
  }

  // Development compatibility for early UI project JSON files.
  if (isProject(parsed)) return parsed;

  throw new Error('PROJECT-FILE-001: Selected file is not a RotorDin Studio project.');
}

export async function saveProjectFile(request: ProjectSaveRequest): Promise<ProjectSaveResponse> {
  let target = request.filePath;

  if (!target) {
    const result = await dialog.showSaveDialog({
      title: 'Save RotorDin Studio Project',
      defaultPath: `${request.project.reference || 'project'}.rdin.json`,
      filters: [
        { name: 'RotorDin Studio Project', extensions: ['json'] },
      ],
    });
    if (result.canceled || !result.filePath) return { cancelled: true };
    target = result.filePath;
  }

  const envelope: RotorProjectFile = {
    schemaVersion: 1,
    project: request.project,
  };

  const tmp = path.join(
    path.dirname(target),
    `.${path.basename(target)}.${process.pid}.tmp`,
  );

  await writeFile(tmp, JSON.stringify(envelope, null, 2) + '\n', { encoding: 'utf8' });
  await rename(tmp, target);

  return { cancelled: false, filePath: target };
}

export async function openProjectFile(): Promise<ProjectOpenResponse> {
  const result = await dialog.showOpenDialog({
    title: 'Open RotorDin Studio Project',
    properties: ['openFile'],
    filters: [
      { name: 'RotorDin Studio Project', extensions: ['json'] },
    ],
  });

  if (result.canceled || result.filePaths.length === 0) return { cancelled: true };

  const filePath = result.filePaths[0];
  const text = await readFile(filePath, 'utf8');
  const project = decodeProjectFile(text);

  return {
    cancelled: false,
    filePath,
    project,
  };
}
