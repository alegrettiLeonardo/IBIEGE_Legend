import { contextBridge, ipcRenderer } from 'electron';
import {
  IPC_CHANNELS,
  type ExecuteQualifiedRunRequest,
  type InspectRunRequest,
  type LoadQualifiedRunRequest,
  type OpenRunWorkspaceRequest,
  type PrepareRunRequest,
  type ReadRunArtifactRequest,
  type RevealRunArtifactRequest,
  type ProjectSaveRequest,
  type RotorDinDesktopApi,
} from './contracts';

const api: RotorDinDesktopApi = {
  saveProject: (request: ProjectSaveRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.projectSave, request),
  openProject: () =>
    ipcRenderer.invoke(IPC_CHANNELS.projectOpen),
  prepareRun: (request: PrepareRunRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runPrepare, request),
  executeQualifiedRun: (request: ExecuteQualifiedRunRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runExecuteQualified, request),
  listQualifiedRuns: () =>
    ipcRenderer.invoke(IPC_CHANNELS.runListQualified),
  loadQualifiedRun: (request: LoadQualifiedRunRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runLoadQualified, request),
  inspectRun: (request: InspectRunRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runInspect, request),
  readRunArtifact: (request: ReadRunArtifactRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runReadArtifact, request),
  revealRunArtifact: (request: RevealRunArtifactRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runRevealArtifact, request),
  openRunWorkspace: (request: OpenRunWorkspaceRequest) =>
    ipcRenderer.invoke(IPC_CHANNELS.runOpenWorkspace, request),
};

contextBridge.exposeInMainWorld('rotorDinDesktop', api);
