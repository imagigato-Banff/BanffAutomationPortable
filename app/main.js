// app/main.js
const { app, BrowserWindow, dialog } = require("electron");
const path = require("path");
const { spawn } = require("child_process");
const net = require("net");
const fs = require("fs");
const os = require("os");

let mainWindow;
let rProc;

// ---------- Logging (always to file) ----------
const logDir = path.join(os.homedir(), "BanffAutomationLogs");
try { fs.mkdirSync(logDir, { recursive: true }); } catch (_) {}

const appLogPath = path.join(logDir, "electron.log");
const rOutPath = path.join(logDir, "r-stdout.log");
const rErrPath = path.join(logDir, "r-stderr.log");

function appendAppLog(line) {
  try {
    fs.appendFileSync(appLogPath, `[${new Date().toISOString()}] ${line}\n`);
  } catch (_) {}
}

function tailFile(filePath, maxChars = 4000) {
  try {
    if (!fs.existsSync(filePath)) return "";
    const buf = fs.readFileSync(filePath);
    const text = buf.toString("utf8");
    return text.length > maxChars ? text.slice(-maxChars) : text;
  } catch (_) {
    return "";
  }
}

// ---------- Helpers ----------
function resourcesRoot() {
  // packaged: process.resourcesPath; dev: app.getAppPath()
  return app.isPackaged ? process.resourcesPath : app.getAppPath();
}

function runtimeRoot() {
  // You are bundling runtime/win and runtime/mac inside resources/app/runtime/...
  if (process.platform === "win32") return path.join(resourcesRoot(), "app", "runtime", "win");
  return path.join(resourcesRoot(), "app", "runtime", "mac");
}

function rBinary() {
  const isWin = process.platform === "win32";
  return path.join(runtimeRoot(), "R", "bin", isWin ? "Rscript.exe" : "Rscript");
}

function rLib() {
  return path.join(runtimeRoot(), "library");
}

function banffAppDir() {
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
      const retry = () => {
        if (Date.now() - start > timeoutMs) return reject(new Error("Timeout esperando a Shiny."));
        setTimeout(tick, 350);
      };
      socket.connect(port, host);
    };
    tick();
  });
}

function showFatal(title, msg) {
  appendAppLog(`FATAL: ${title} | ${msg}`);
  dialog.showErrorBox(title, msg);
}

// ---------- Start Shiny via Rscript ----------
async function startShiny() {
  const port = 3939;

  const rscript = rBinary();
  const lib = rLib();
  const dir = banffAppDir();

  appendAppLog(`resourcesRoot: ${resourcesRoot()}`);
  appendAppLog(`runtimeRoot: ${runtimeRoot()}`);
  appendAppLog(`rBinary: ${rscript}`);
  appendAppLog(`rLib: ${lib}`);
  appendAppLog(`banffAppDir: ${dir}`);

  if (!fs.existsSync(rscript)) {
    throw new Error(`No existe Rscript en: ${rscript}`);
  }
  if (!fs.existsSync(dir)) {
    throw new Error(`No existe banff-app en: ${dir}`);
  }

  // Ensure log files exist
  fs.writeFileSync(appLogPath, "", { flag: "a" });
  fs.writeFileSync(rOutPath, "", { flag: "a" });
  fs.writeFileSync(rErrPath, "", { flag: "a" });

  const rCode = `
    tryCatch({
      .libPaths("${lib.replace(/\\/g, "/")}")
      options(shiny.port=${port}, shiny.host="127.0.0.1")
      setwd("${dir.replace(/\\/g, "/")}")

      suppressPackageStartupMessages(library(shiny))

      # Run the app (must be a Shiny app dir with app.R or ui/server)
      runApp(".", launch.browser=FALSE, host="127.0.0.1", port=${port})
    }, error=function(e) {
      message("FATAL_R_ERROR: ", conditionMessage(e))
      quit(status=1)
    })
  `;

  // Spawn R
  rProc = spawn(rscript, ["-e", rCode], {
    stdio: ["ignore", "pipe", "pipe"],
    env: {
      ...process.env,
      // Force R to use bundled library first:
      R_LIBS_USER: lib,
      // Useful debugging flags:
      R_VERBOSE: "1"
    }
  });

  const rOut = fs.createWriteStream(rOutPath, { flags: "a" });
  const rErr = fs.createWriteStream(rErrPath, { flags: "a" });

  rProc.stdout.on("data", (d) => rOut.write(d));
  rProc.stderr.on("data", (d) => rErr.write(d));

  rProc.on("error", (err) => {
    const tailErr = tailFile(rErrPath);
    showFatal(
      "Banff Automation System",
      `No se pudo iniciar R.\n\n${String(err)}\n\nLogs:\n${logDir}\n\nÚltimo stderr:\n${tailErr}`
    );
    app.quit();
  });

  rProc.on("exit", (code) => {
    const tailErr = tailFile(rErrPath);
    const tailOut = tailFile(rOutPath);

    // If R exits before Shiny is ready, show logs immediately:
    if (code !== 0) {
      showFatal(
        "Banff Automation System",
        `R se cerró (exit code: ${code}).\n\nAbre estos logs:\n${logDir}\n\nÚltimo stderr:\n${tailErr}\n\nÚltimo stdout:\n${tailOut}`
      );
    }
    app.quit();
  });

  // Wait until Shiny listens
  await waitForPort(port);
  return `http://127.0.0.1:${port}`;
}

// ---------- Electron window ----------
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

// ---------- App lifecycle ----------
app.whenReady().then(async () => {
  try {
    const url = await startShiny();
    await createWindow(url);
  } catch (e) {
    const tailErr = tailFile(rErrPath);
    showFatal(
      "Banff Automation System",
      `Error al arrancar.\n\n${String(e.message || e)}\n\nLogs:\n${logDir}\n\nÚltimo stderr:\n${tailErr}`
    );
    app.quit();
  }
});

app.on("window-all-closed", () => {
  try { if (rProc) rProc.kill(); } catch (_) {}
  if (process.platform !== "darwin") app.quit();
});

app.on("activate", () => {
  if (BrowserWindow.getAllWindows().length === 0 && mainWindow) {
    mainWindow.show();
  }
});
