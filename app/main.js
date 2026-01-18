const { app, BrowserWindow, dialog } = require("electron");
const path = require("path");
const { spawn } = require("child_process");
const fs = require("fs");
const os = require("os");
const net = require("net");

let win;
let rProc;

const logDir = path.join(os.homedir(), "BanffAutomationLogs");
fs.mkdirSync(logDir, { recursive: true });

const rErr = path.join(logDir, "r-stderr.log");
const rOut = path.join(logDir, "r-stdout.log");

function fatal(msg) {
  dialog.showErrorBox("Banff Automation System", msg + `\n\nLogs en:\n${logDir}`);
  app.quit();
}

function resources() {
  return app.isPackaged ? process.resourcesPath : app.getAppPath();
}

function rBin() {
  return process.platform === "win32"
    ? path.join(resources(), "app", "runtime", "win", "R", "bin", "Rscript.exe")
    : path.join(resources(), "app", "runtime", "mac", "R", "bin", "Rscript");
}

function rLib() {
  return path.join(resources(), "app", "runtime", "win", "library");
}

function appDir() {
  return path.join(resources(), "app", "banff-app");
}

function waitPort(port, timeout = 60000) {
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
        if (Date.now() - start > timeout) reject();
        else setTimeout(tick, 300);
      }
      s.connect(port, "127.0.0.1");
    })();
  });
}

async function startR() {
  const port = 3939;

  const code = `
    dir.create("${rLib().replace(/\\/g,"/")}", recursive=TRUE, showWarnings=FALSE)
    .libPaths("${rLib().replace(/\\/g,"/")}")

    pkgs <- c("shiny","jsonlite","dplyr","ggplot2")
    for (p in pkgs) {
      if (!require(p, character.only=TRUE)) {
        install.packages(p, repos="https://cloud.r-project.org")
        library(p, character.only=TRUE)
      }
    }

    setwd("${appDir().replace(/\\/g,"/")}")

    if (!file.exists("app.R") && !file.exists("server.R")) {
      stop("No es una app Shiny válida (falta app.R o server.R)")
    }

    shiny::runApp(".", host="127.0.0.1", port=${port}, launch.browser=FALSE)
  `;

  rProc = spawn(rBin(), ["-e", code], {
    env: { ...process.env, R_LIBS_USER: rLib() },
    stdio: ["ignore", "pipe", "pipe"]
  });

  rProc.stdout.pipe(fs.createWriteStream(rOut));
  rProc.stderr.pipe(fs.createWriteStream(rErr));

  rProc.on("exit", () => {
    fatal("R se cerró.\n\nAbre r-stderr.log para ver el motivo exacto.");
  });

  await waitPort(port);
  return `http://127.0.0.1:${port}`;
}

app.whenReady().then(async () => {
  try {
    const url = await startR();
    win = new BrowserWindow({ width: 1400, height: 900 });
    win.loadURL(url);
  } catch {
    fatal("Shiny no arrancó (timeout).");
  }
});
