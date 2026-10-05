import { contextBridge, ipcRenderer } from 'electron';
import {
  IPC_CHANNELS,
  type ExecuteQualifiedRunRequest,
  type LoadQualifiedRunRequest,
  type PrepareRunRequest,
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
};

contextBridge.exposeInMainWorld('rotorDinDesktop', api);
