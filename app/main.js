const { app, BrowserWindow, dialog } = require('electron');
const path = require('path');
const { spawn } = require('child_process');
const net = require('net');

let mainWindow;
let rProc;

function waitForPort(port, host = '127.0.0.1', timeoutMs = 60000) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = () => {
      const socket = new net.Socket();
      socket.setTimeout(800);
      socket.on('connect', () => { socket.destroy(); resolve(true); });
      socket.on('error', () => { socket.destroy(); retry(); });
      socket.on('timeout', () => { socket.destroy(); retry(); });
      const retry = () => {
        if (Date.now() - start > timeoutMs) return reject(new Error('Timeout esperando a Shiny.'));
        setTimeout(tick, 300);
      };
      socket.connect(port, host);
    };
    tick();
  });
}

function resourcesRoot() {
  // En dev: app.getAppPath(); empaquetado: process.resourcesPath
  return app.isPackaged ? process.resourcesPath : app.getAppPath();
}

function platformKey() {
  if (process.platform === 'win32') return 'win';
  if (process.platform === 'darwin') return 'mac';
  throw new Error('Plataforma no soportada en este build (solo Windows/macOS).');
}

function runtimeRoot() {
  return path.join(resourcesRoot(), 'app', 'runtime', platformKey());
}

function rBinary() {
  const isWin = process.platform === 'win32';
  return path.join(runtimeRoot(), 'R', 'bin', isWin ? 'Rscript.exe' : 'Rscript');
}

function rLib() {
  return path.join(runtimeRoot(), 'library');
}

function banffAppDir() {
  return path.join(resourcesRoot(), 'app', 'banff-app');
}

async function startShiny() {
  const port = 3939;
  const rscript = rBinary();
  const lib = rLib();
  const dir = banffAppDir();

  const code = `
    .libPaths("${lib.replace(/\\/g, '/')}")
    options(shiny.port=${port}, shiny.host="127.0.0.1")
    setwd("${dir.replace(/\\/g, '/')}")
    library(shiny)
    runApp(".", launch.browser=FALSE, host="127.0.0.1", port=${port})
  `;

  rProc = spawn(rscript, ['-e', code], { stdio: 'pipe' });

  rProc.stdout.on('data', (d) => console.log('[R]', d.toString()));
  rProc.stderr.on('data', (d) => console.error('[R-ERR]', d.toString()));

  rProc.on('exit', (code) => {
    dialog.showErrorBox('Banff Automation System', 'R se cerró. Revisa los logs del ejecutable.');
    app.quit();
  });

  await waitForPort(port);
  return `http://127.0.0.1:${port}`;
}

async function createWindow(url) {
  mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true
    }
  });

  await mainWindow.loadURL(url);
}

app.whenReady().then(async () => {
  try {
    const url = await startShiny();
    await createWindow(url);
  } catch (e) {
    dialog.showErrorBox('Banff Automation System', String(e && (e.message || e) || 'Error desconocido'));
    app.quit();
  }
});

app.on('window-all-closed', () => {
  if (rProc) rProc.kill();
  if (process.platform !== 'darwin') app.quit();
});
