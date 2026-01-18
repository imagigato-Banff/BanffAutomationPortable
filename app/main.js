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

const rInternalLog = path.join(logDir, "r-internal.log");

function resources() {
  return app.isPackaged ? process.resourcesPath : app.getAppPath();
}

function rBin() {
  if (process.platform === "win32") {
    return path.join(resources(), "app", "runtime", "win", "R", "bin", "Rscript.exe");
  }
  return path.join(resources(), "app", "runtime", "mac", "R", "bin", "Rscript");
}

function rLibWin() {
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
      s.setTimeout(900);
      s.once("connect", () => { s.destroy(); resolve(); });
      s.once("error", retry);
      s.once("timeout", retry);
      function retry() {
        s.destroy();
        if (Date.now() - start > timeout) reject(new Error("Timeout esperando a Shiny."));
        else setTimeout(tick, 300);
      }
      s.connect(port, "127.0.0.1");
    })();
  });
}

function fatal(msg) {
  dialog.showErrorBox("Banff Automation System", msg + `\n\nLogs en:\n${logDir}`);
  app.quit();
}

async function startR() {
  const port = 3939;
  const rscript = rBin();
  const lib = rLibWin();
  const dir = appDir();

  if (!fs.existsSync(rscript)) throw new Error("Rscript.exe no existe en el bundle.");
  if (!fs.existsSync(dir)) throw new Error("No existe app/banff-app dentro del bundle.");

  // Código R: escribe log interno desde el primer microsegundo
  const code = `
    logf <- "${rInternalLog.replace(/\\/g, "/")}"
    try({
      dir.create(dirname(logf), recursive=TRUE, showWarnings=FALSE)
      con <- file(logf, open="at")
      sink(con); sink(con, type="message")
      cat("=== R BOOT ===\\n")
      cat("WD before:", getwd(), "\\n")
      cat("libPath before:", paste(.libPaths(), collapse=" | "), "\\n")

      dir.create("${lib.replace(/\\/g, "/")}", recursive=TRUE, showWarnings=FALSE)
      .libPaths("${lib.replace(/\\/g, "/")}")
      cat("libPath after:", paste(.libPaths(), collapse=" | "), "\\n")

      cat("Setting wd to:", "${dir.replace(/\\/g, "/")}", "\\n")
      setwd("${dir.replace(/\\/g, "/")}")
      cat("WD after:", getwd(), "\\n")

      pkgs <- c("shiny","shinythemes","tidyverse","dplyr","collapsibleTree","shinycssloaders","shinyWidgets","rmarkdown","knitr","readxl","writexl","kableExtra","stringr","shinyjs")
      cat("Checking packages...\\n")
      for (p in pkgs) {
        cat(" -", p, ": ")
        ok <- suppressWarnings(require(p, character.only=TRUE))
        cat(if (ok) "OK\\n" else "MISSING\\n")
      }

      if (!file.exists("app.R") && !file.exists("server.R")) {
        stop("banff-app NO es una app Shiny válida: falta app.R o server.R")
      }

      cat("Starting Shiny...\\n")
      options(shiny.port=${port}, shiny.host="127.0.0.1")
      shiny::runApp(".", host="127.0.0.1", port=${port}, launch.browser=FALSE)
    }, silent=FALSE)
  `;

  rProc = spawn(rscript, ["--vanilla", "-e", code], {
    env: {
      ...process.env,
      R_LIBS_USER: lib,
      TMPDIR: os.tmpdir(),
      TEMP: os.tmpdir(),
      TMP: os.tmpdir()
    },
    stdio: ["ignore", "ignore", "ignore"] // todo va a r-internal.log vía sink()
  });

  rProc.on("exit", (code) => {
    fatal(`R se cerró (exit code: ${code}).\nAbre r-internal.log para ver el motivo exacto.`);
  });

  await waitPort(port);
  return `http://127.0.0.1:${port}`;
}

app.whenReady().then(async () => {
  try {
    const url = await startR();
    win = new BrowserWindow({ width: 1400, height: 900 });
    win.loadURL(url);
  } catch (e) {
    fatal(String(e.message || e));
  }
});
