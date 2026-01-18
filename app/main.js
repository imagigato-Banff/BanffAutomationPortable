const { app, dialog } = require("electron");
const path = require("path");
const { spawn } = require("child_process");
const fs = require("fs");
const os = require("os");

function showFatal(msg) {
  try {
    dialog.showErrorBox("Banff Automation System", msg);
  } catch {}
  app.quit();
}

app.whenReady().then(() => {
  try {
    if (process.platform !== "win32") {
      showFatal("Este main.js está configurado solo para Windows.");
      return;
    }

    // Ruta al .bat dentro del paquete (debe existir en: resources/app/run_windows.bat)
    const batPath = path.join(process.resourcesPath, "app", "run_windows.bat");

    if (!fs.existsSync(batPath)) {
      showFatal(
        "No se encontró run_windows.bat.\n\n" +
          "Debe existir en:\n" + batPath + "\n\n" +
          "Crea app/run_windows.bat y recompila."
      );
      return;
    }

    // Log mínimo para confirmar que el .bat se lanzó
    const logDir = path.join(os.homedir(), "BanffAutomationLogs");
    fs.mkdirSync(logDir, { recursive: true });
    const launchLog = path.join(logDir, "electron-launch.log");
    fs.appendFileSync(
      launchLog,
      `[${new Date().toISOString()}] Launching BAT: ${batPath}\n`
    );

    // Lanza el .bat con cmd.exe en modo detached
    const p = spawn("cmd.exe", ["/c", `"${batPath}"`], {
      detached: true,
      stdio: "ignore",
      windowsHide: true
    });

    p.unref();

    // Salimos: el .bat abrirá el navegador con Shiny
    app.quit();
  } catch (e) {
    showFatal(String(e && e.message ? e.message : e));
  }
});
