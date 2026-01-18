const { app, BrowserWindow, dialog } = require("electron");
const path = require("path");
const { spawn } = require("child_process");
const net = require("net");

let win;
let rProc;

function resourcesRoot() {
  return app.isPackaged ? process.resourcesPath : app.getAppPath();
}

function rscriptPath() {
  if (process.platform === "win32") {
    return path.join(resourcesRoot(), "app", "runtime", "win", "R", "bin", "Rscript.exe");
  }
  return path.join(resourcesRoot(), "app", "runtime", "mac", "R", "bin", "Rscript");
}

function libPath() {
  if (process.platform === "win32") {
    return path.join(resourcesRoot(), "app", "runtime", "win", "library");
  }
  return path.join(resourcesRoot(), "app", "runtime", "mac", "library");
}

function banffAppPath() {
  return path.join(resourcesRoot(), "app", "banff-app");
}

function waitForPort(port, host = "127.0.0.1", timeoutMs = 60000) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = () => {
      const socket = new net.Socket();
      socket.setTimeout(900);
      socket.on("connect", () => { socket.destroy(); resolve(true); });
      socket.on("error", () => { socket.destroy(); retry(); });
      socket.on("timeout", () => { socket.destroy(); retry(); });

      function retry() {
        if (Date.now() - start > timeoutMs) return reject(new Error("Timeout esperando a Shiny."));
        setTimeout(tick, 350);
      }
      socket.connect(port, host);
    };
    tick();
  });
}

function fatal(msg) {
  dialog.showErrorBox("Banff Automation System", msg);
  app.quit();
}

async function startShiny() {
  const port = 3939;

  const rscript = rscriptPath();
  const lib = libPath().replace(/\\/g, "/");
  const appDir = banffAppPath().replace(/\\/g, "/");

  const rCode = `
    lib <- "${lib}"
    appDir <- "${appDir}"
    .libPaths(lib)
    setwd(appDir)
    suppressPackageStartupMessages(library(shiny))
    shiny::runApp(".", host="127.0.0.1", port=${port}, launch.browser=FALSE)
  `;

  rProc = spawn(rscript, ["--vanilla", "-e", rCode], {
    stdio: ["ignore", "ignore", "ignore"],
    env: {
      ...process.env,
      R_LIBS_USER: libPath()
    }
  });

  rProc.on("exit", (code) => {
    fatal(`R se cerró (exit code: ${code}).\nAsegura que los paquetes están en:\n${libPath()}`);
  });

  await waitForPort(port);
  return `http://127.0.0.1:${port}`;
}

app.whenReady().then(async () => {
  try {
    const url = await startShiny();
    win = new BrowserWindow({ width: 1400, height: 900 });
    await win.loadURL(url);
  } catch (e) {
    fatal(String(e.message || e));
  }
});

app.on("window-all-closed", () => {
  try { if (rProc) rProc.kill(); } catch {}
  if (process.platform !== "darwin") app.quit();
});
