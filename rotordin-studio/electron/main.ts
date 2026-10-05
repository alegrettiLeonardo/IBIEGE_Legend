import { app, BrowserWindow, ipcMain } from 'electron';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import { IPC_CHANNELS, type ExecuteQualifiedRunRequest, type InspectRunRequest, type LoadQualifiedRunRequest, type OpenRunWorkspaceRequest, type PrepareRunRequest, type ProjectSaveRequest, type ReadRunArtifactRequest, type RevealRunArtifactRequest } from './contracts';
import { openProjectFile, saveProjectFile } from './services/projectFiles';
import { prepareRunWorkspace } from './services/runWorkspace';
import { executeQualifiedRotorDinRun } from './services/qualifiedRotorDinRun';
import { inspectRun, listQualifiedRuns, loadQualifiedRun, openRunWorkspace, readRunArtifact, revealRunArtifact } from './services/runHistory';

const moduleDirectory = path.dirname(fileURLToPath(import.meta.url));

function createWindow() {
  const window = new BrowserWindow({
    width: 1600,
    height: 1000,
    minWidth: 1200,
    minHeight: 760,
    backgroundColor: '#eef3f6',
    show: false,
    webPreferences: {
      preload: path.join(moduleDirectory, 'preload.cjs'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
    },
  });

  window.once('ready-to-show', () => window.show());

  const devUrl = process.env.ROTOR_STUDIO_DEV_URL;
  if (devUrl) {
    void window.loadURL(devUrl);
  } else {
    void window.loadFile(path.join(app.getAppPath(), 'dist', 'index.html'));
  }
}

function registerIpc() {
  ipcMain.handle(IPC_CHANNELS.projectSave, async (_event, request: ProjectSaveRequest) =>
    saveProjectFile(request));

  ipcMain.handle(IPC_CHANNELS.projectOpen, async () =>
    openProjectFile());

  ipcMain.handle(IPC_CHANNELS.runPrepare, async (_event, request: PrepareRunRequest) =>
    prepareRunWorkspace(request));

  ipcMain.handle(IPC_CHANNELS.runExecuteQualified, async (_event, request: ExecuteQualifiedRunRequest) =>
    executeQualifiedRotorDinRun(request));

  ipcMain.handle(IPC_CHANNELS.runListQualified, async () =>
    listQualifiedRuns());

  ipcMain.handle(IPC_CHANNELS.runLoadQualified, async (_event, request: LoadQualifiedRunRequest) =>
    loadQualifiedRun(request));

  ipcMain.handle(IPC_CHANNELS.runInspect, async (_event, request: InspectRunRequest) =>
    inspectRun(request));

  ipcMain.handle(IPC_CHANNELS.runReadArtifact, async (_event, request: ReadRunArtifactRequest) =>
    readRunArtifact(request));

  ipcMain.handle(IPC_CHANNELS.runRevealArtifact, async (_event, request: RevealRunArtifactRequest) =>
    revealRunArtifact(request));

  ipcMain.handle(IPC_CHANNELS.runOpenWorkspace, async (_event, request: OpenRunWorkspaceRequest) =>
    openRunWorkspace(request));
}

app.whenReady().then(() => {
  registerIpc();
  createWindow();

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});
