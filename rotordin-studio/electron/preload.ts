import { contextBridge, ipcRenderer } from 'electron';
import {
  IPC_CHANNELS,
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
};

contextBridge.exposeInMainWorld('rotorDinDesktop', api);
