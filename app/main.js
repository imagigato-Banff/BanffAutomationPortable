const { app, BrowserWindow, dialog } = require("electron");
const path = require("path");
const { spawn } = require("child_process");
const fs = require("fs");
const os = require("os");
const net = require("net");

let win;
let rProc;

function root() {
  return app.isPackaged ? process.resourcesPath : app.getAppPath();
}

function winRRoot() {
  return path.join(root(), "app", "runtime", "win", "R");
}
function winRscript() {
  return path.join(winRRoot(), "bin", "Rscript.exe");
}
function winLib() {
  return path.join(root(), "app", "runtime", "win", "library");
}
function banffApp() {
  return path.join(root(), "app", "banff-app");
}

function waitPort(port, host = "127.0.0.1", timeout = 60000) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    (function tick() {
      const s = new net.Socket();
      s.setTimeout(800);
      s.once("connect", () => { s.destroy(); resolve(); });
      s.once("error", retry);
      s.once("timeout", retry);
      function retry() {
        s.destroy();
        if (Date.now() - start > timeout) reject(new Error("Timeout esperando a Shiny."));
        else setTimeout(tick, 300);
      }
      s.connect(port, host);
    })();
  });
}

function showFatal(msg) {
  dialog.showErrorBox("Banff Automation System", msg);
  app.quit();
}

async function startShiny() {
  const port = 3939;

  const rscript = winRscript();
  const libDir = winLib();
  const appDir = banffApp();

  if (!fs.existsSync(rscript)) throw new Error("No existe Rscript.exe en el bundle.");
  if (!fs.existsSync(libDir)) throw new Error("No existe runtime/win/library en el bundle.");
  if (!fs.existsSync(appDir)) throw new Error("No existe app/banff-app en el bundle.");

  const logDir = path.join(os.homedir(), "BanffAutomationLogs");
  fs.mkdirSync(logDir, { recursive: true });
  const outLog = path.join(logDir, "electron-r-stdout.log");
  const errLog = path.join(logDir, "electron-r-stderr.log");

  // Limpia logs previos
  try { fs.writeFileSync(outLog, ""); } catch {}
  try { fs.writeFileSync(errLog, ""); } catch {}

  const libPosix = libDir.replace(/\\/g, "/");
  const appPosix = appDir.replace(/\\/g, "/");
  const rCode = `
    lib <- "${libPosix}"
    .libPaths(lib)
    setwd("${appPosix}")
    cat("LIBPATH=", paste(.libPaths(), collapse=" | "), "\\n")
    cat("WD=", getwd(), "\\n")
    suppressPackageStartupMessages(library(shiny))
    shiny::runApp(".", host="127.0.0.1", port=${port}, launch.browser=FALSE)
  `;

  rProc = spawn(rscript, ["--vanilla", "-e", rCode], {
    env: {
      ...process.env,
      // claves: obligan a usar TU R y TU library
      R_HOME: winRRoot(),
      R_LIBS_USER: libDir,
      R_LIBS: libDir
    },
    stdio: ["ignore", "pipe", "pipe"]
  });

  // Guarda logs SIEMPRE
  rProc.stdout.on("data", (d) => { try { fs.appendFileSync(outLog, d); } catch {} });
  rProc.stderr.on("data", (d) => { try { fs.appendFileSync(errLog, d); } catch {} });

  rProc.on("exit", (code) => {
    showFatal(
      `R se cerró (exit code: ${code}).\n\n` +
      `Revisa estos logs:\n${outLog}\n${errLog}\n\n` +
      `Library esperado:\n${libDir}`
    );
  });

  await waitPort(port);
  return `http://127.0.0.1:${port}`;
}

app.whenReady().then(async () => {
  try {
    const url = await startShiny();
    win = new BrowserWindow({ width: 1400, height: 900 });
    await win.loadURL(url);
  } catch (e) {
    showFatal(String(e.message || e));
  }
});

app.on("window-all-closed", () => {
  try { if (rProc) rProc.kill(); } catch {}
  if (process.platform !== "darwin") app.quit();
});
