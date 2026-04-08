package com.shaft.tools.io.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

/**
 * Internal utility class that manages the Allure reporting lifecycle for SHAFT test runs.
 * Responsibilities include initialising the results directory, generating an {@code allurerc.yaml}
 * configuration file, bootstrapping the Allure 3 CLI (downloaded automatically when not present),
 * invoking it to produce the single-file HTML report, and optionally producing a ZIP archive.
 *
 * <p>This class is not intended for direct use in test code. It is invoked automatically by the
 * SHAFT framework listeners at suite start and finish.
 *
 * <p><b>Allure 3 CLI resolution order (batteries-included):</b>
 * <ol>
 *   <li>A globally installed {@code allure} binary already on {@code PATH}.</li>
 *   <li>{@code npx} on {@code PATH} → {@code npx --yes allure@<version>} (auto-downloads).</li>
 *   <li>Portable Node.js downloaded to {@code ~/.m2/repository/nodejs/} → its bundled
 *       {@code npx} → same {@code npx --yes allure@<version>} invocation.</li>
 * </ol>
 * No manual installation is required; SHAFT handles it transparently.
 *
 * <p>Thread safety: all public methods are {@code static} and intended to be called from a
 * single thread (the test runner thread). The result directory fields are mutable class-level
 * state and should not be accessed concurrently.
 */
public class AllureManager {
    // ─── Allure 3 report paths & config ────────────────────────────────────────
    private static final String allureReportPath = "allure-report";
    private static final String allureConfigFileName = "allurerc.yaml";

    // ─── Portable Node.js bootstrap ────────────────────────────────────────────
    /** Cache directory for the downloaded portable Node.js distribution. */
    private static final String NODEJS_CACHE_DIR = System.getProperty("user.home")
            + File.separator + ".m2" + File.separator + "repository"
            + File.separator + "nodejs" + File.separator;

    /** Cached resolved command prefix for allure (e.g. {@code "allure"} or {@code "npx --yes allure@3.x.x"}).
     *  {@code null} means resolution has not happened yet; {@code ""} means no CLI was found. */
    private static volatile String cachedAllureCommandPrefix = null;

    // ─── SHAFT internal helpers ─────────────────────────────────────────────────
    private static final TerminalActions internalTerminalSession = TerminalActions.getInstance(false, false, true);
    private static final FileActions internalFileSession = FileActions.getInstance(true);
    private static String allureResultsFolderPath = "";
    private static String allureOutPutDirectory = "";

    /**
     * Initialises the Allure reporting environment before a test suite begins.
     * This method:
     * <ol>
     *   <li>Resolves the Allure results folder path from SHAFT properties.</li>
     *   <li>Optionally cleans the report and results directories.</li>
     *   <li>Writes convenience shell/batch scripts to the project root for manual report generation.</li>
     *   <li>Writes the current JVM system properties to {@code environment.xml} in the results directory.</li>
     * </ol>
     *
     * <p>The Allure 3 CLI is resolved (and downloaded if necessary) automatically at report-generation
     * time; no separate installation step is required.
     *
     * <p>Example (called automatically by SHAFT listeners):
     * <pre>{@code
     * AllureManager.initializeAllureReportingEnvironment();
     * }</pre>
     */
    public static void initializeAllureReportingEnvironment() {
        ReportManager.logDiscrete("Initializing Allure Reporting Environment...");
        /*
         * Force screenshot link to be shown in the results as a link not text
         */
        System.setProperty("org.uncommons.reportng.escape-output", "false");
        allureResultsFolderPath = SHAFT.Properties.paths.allureResults();
        cleanAllureReportDirectory();
        cleanAllureResultsDirectory();
        writeGenerateReportShellFilesToProjectDirectory();
        writeEnvironmentVariablesToAllureResultsDirectory();
    }

    /**
     * Generates the Allure HTML report and opens it in the default browser when
     * {@code SHAFT.Properties.allure.automaticallyOpen()} is {@code true}.
     * The generated report is copied to the {@code allure-report} directory, renamed
     * (optionally with a timestamp when {@code accumulateReports} is enabled), and the
     * intermediate output directory is deleted.
     *
     * <p>Example (called automatically by SHAFT listeners after suite completion):
     * <pre>{@code
     * AllureManager.openAllureReportAfterExecution();
     * }</pre>
     */
    public static void openAllureReportAfterExecution() {
        writeAllureReport();
        copyAndOpenAllure();
    }

    private static void copyAndOpenAllure() {
        internalFileSession.copyFolder(allureOutPutDirectory, allureReportPath);
        internalFileSession.deleteFile(allureOutPutDirectory);
        String newFileName = renameAllureReport();
        if (newFileName != null) {
            openAllureReport(newFileName);
        }
    }

    /**
     * Renames the generated {@code index.html} to a descriptive filename.
     *
     * @return the new filename, or {@code null} if {@code index.html} was not found
     *         (which means Allure report generation failed or the CLI is not installed)
     */
    private static String renameAllureReport() {
        String newFileName = "AllureReport.html";
        if (SHAFT.Properties.allure.accumulateReports())
            newFileName = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")) + "_AllureReport.html";
        String sourceFile = System.getProperty("user.dir") + File.separator + allureReportPath + File.separator + "index.html";
        if (!internalFileSession.doesFileExist(sourceFile)) {
            // Allure report was not generated (CLI bootstrap failed or allure-results was empty)
            ReportManager.logDiscrete("Allure report 'index.html' not found — 'allure generate' may have failed."
                    + " Check that allure-results is not empty and that SHAFT could reach nodejs.org to download Node.js.");
            return null;
        }
        internalFileSession.renameFile(sourceFile, newFileName);
        return newFileName;
    }

    private static void openAllureReport(String newFileName) {
        if (SHAFT.Properties.allure.automaticallyOpen()) {
            if (SystemUtils.IS_OS_WINDOWS) {
                internalTerminalSession.performTerminalCommand(".\\" + allureReportPath + File.separator + newFileName);
            } else {
                internalTerminalSession.performTerminalCommand("open ./" + allureReportPath + File.separator + newFileName);
            }
        }
    }

    /**
     * Generates a self-contained ZIP archive of the Allure report when
     * {@code SHAFT.Properties.allure.generateArchive()} is {@code true}.
     * The archive is written to the project directory with a timestamped filename
     * (e.g. {@code generatedReport_2024-01-15_10-30-00-000.zip}).
     *
     * <p>This method is a no-op when archive generation is disabled in properties.
     *
     * <p>Example (called automatically by SHAFT listeners):
     * <pre>{@code
     * AllureManager.generateAllureReportArchive();
     * }</pre>
     */
    public static void generateAllureReportArchive() {
        if (Boolean.TRUE.equals(SHAFT.Properties.allure.generateArchive())) {
            ReportManager.logDiscrete("Generating Allure Report Archive...");
            ReportHelper.disableLogging();
            writeAllureReport();
            createAllureReportArchive();
            ReportHelper.enableLogging();
        }
    }

    /**
     * Returns the Allure results path with any trailing path separator removed.
     * This trimmed form is used directly in CLI commands and config values.
     *
     * @return the allure results folder path without a trailing separator
     */
    private static String getResultsPath() {
        return allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1);
    }

    private static void writeGenerateReportShellFilesToProjectDirectory() {
        String resultsPath = getResultsPath();
        // create generate_allure_report.sh or generate_allure_report.bat
        // Uses allure if available on PATH, otherwise falls back to npx (which comes with Node.js).
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            commandsToServeAllureReport = Arrays.asList(
                    "@echo off",
                    "where allure >nul 2>&1 && (allure open \"" + resultsPath + "\\allure-report\") || (npx --yes allure@" + SHAFT.Properties.internal.allure3Version() + " open \"" + resultsPath + "\\allure-report\")",
                    "pause", "exit");
            internalFileSession.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            commandsToServeAllureReport = Arrays.asList(
                    "#!/bin/bash",
                    "if command -v allure >/dev/null 2>&1; then",
                    "  allure open '" + resultsPath + "/allure-report'",
                    "else",
                    "  npx --yes allure@" + SHAFT.Properties.internal.allure3Version() + " open '" + resultsPath + "/allure-report'",
                    "fi");
            internalFileSession.writeToFile("", "generate_allure_report.sh", commandsToServeAllureReport);
            // make script executable on Unix-based shells
            internalTerminalSession.performTerminalCommand("chmod u+x generate_allure_report.sh");
        }
    }

    private static void cleanAllureResultsDirectory() {
        if (SHAFT.Properties.allure.cleanResultsDirectory()) {
            // clean allure-results directory before execution
            var allureResultsPath = allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1);
            try {
                internalFileSession.deleteFolder(allureResultsPath);
            } catch (Exception t) {
                ReportManager.log("Failed to delete '" + allureResultsPath + "' as it is currently open. Kindly restart your device to unlock the directory.");
            }
        }
    }

    private static void cleanAllureReportDirectory() {
        // clean allure-report directory before execution
        if (!SHAFT.Properties.allure.accumulateReports()) {
            try {
                internalFileSession.deleteFolder(allureReportPath);
            } catch (Exception t) {
                ReportManager.log("Failed to delete '" + allureReportPath + "' as it is currently open. Kindly restart your device to unlock the directory.");
            }
        }
    }

    private static void writeAllureReport() {
        allureOutPutDirectory = System.getProperty("user.dir") + File.separator + "target" + File.separator + allureReportPath;
        var customReportName = SHAFT.Properties.allure.customTitle();
        internalFileSession.createFolder(allureOutPutDirectory);

        // Write allurerc.yaml with current settings (including custom title and optional history path)
        writeAllureConfig(customReportName);

        // Resolve the Allure 3 CLI (downloading Node.js if needed) and generate the report
        String cmd = getCommandToCreateAllureReport();
        if (cmd != null && !cmd.isBlank()) {
            internalTerminalSession.performTerminalCommand(cmd);
        } else {
            ReportManager.logDiscrete("Allure report generation skipped: could not resolve the Allure 3 CLI."
                    + " Install Node.js (https://nodejs.org) and re-run to generate the report.");
        }
    }

    /**
     * Writes an {@code allurerc.yaml} configuration file to the project working directory
     * before invoking the Allure 3 CLI. The file captures the custom report name, output
     * directory, optional history path, and awesome-plugin options (single-file mode, grouping).
     *
     * @param reportName the display name for the generated report
     */
    private static void writeAllureConfig(String reportName) {
        var configBuilder = new StringBuilder();
        configBuilder.append("name: \"").append(reportName).append("\"\n");
        configBuilder.append("output: \"").append(allureOutPutDirectory.replace("\\", "/")).append("\"\n");

        if (SHAFT.Properties.allure.accumulateHistory()) {
            String historyPath = (System.getProperty("user.dir") + File.separator + "target"
                    + File.separator + "history.jsonl").replace("\\", "/");
            configBuilder.append("historyPath: \"").append(historyPath).append("\"\n");
            configBuilder.append("appendHistory: true\n");
        }

        configBuilder.append("\nplugins:\n");
        configBuilder.append("  awesome:\n");
        configBuilder.append("    options:\n");
        configBuilder.append("      reportName: \"").append(reportName).append("\"\n");
        configBuilder.append("      singleFile: true\n");
        configBuilder.append("      reportLanguage: \"en\"\n");
        configBuilder.append("      open: false\n");
        configBuilder.append("      groupBy:\n");
        configBuilder.append("        - parentSuite\n");
        configBuilder.append("        - suite\n");
        configBuilder.append("        - subSuite\n");

        internalFileSession.writeToFile(
                System.getProperty("user.dir") + File.separator + allureConfigFileName,
                configBuilder.toString());
    }

    private static String getCommandToCreateAllureReport() {
        String prefix = resolveAllureCommandPrefix();
        if (prefix == null) return null;
        String resultsPath = getResultsPath();
        if (SystemUtils.IS_OS_WINDOWS) {
            return prefix + " generate \"" + resultsPath + "\" -o \"" + allureOutPutDirectory + "\"";
        } else {
            return prefix + " generate " + resultsPath + " -o " + allureOutPutDirectory;
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Batteries-included Allure 3 CLI bootstrap
    // ═══════════════════════════════════════════════════════════════════════════

    /**
     * Resolves the command prefix used to invoke the Allure 3 CLI.
     *
     * <p>The result is computed once and cached for the JVM lifetime.
     *
     * <p>Resolution order:
     * <ol>
     *   <li>{@code allure} binary on {@code PATH} (user-installed globally).</li>
     *   <li>{@code npx} on {@code PATH} → {@code npx --yes allure@<version>}.</li>
     *   <li>Portable Node.js downloaded to {@value #NODEJS_CACHE_DIR} → its {@code npx}.</li>
     * </ol>
     *
     * @return the command prefix (e.g. {@code "allure"} or {@code "npx --yes allure@3.x.x"}),
     *         or {@code null} when no CLI could be resolved or downloaded
     */
    private static String resolveAllureCommandPrefix() {
        if (cachedAllureCommandPrefix != null) {
            // "" means "already tried, nothing found"
            return cachedAllureCommandPrefix.isEmpty() ? null : cachedAllureCommandPrefix;
        }

        // 1. allure binary on PATH
        if (isExecutableOnPath("allure")) {
            cachedAllureCommandPrefix = "allure";
            ReportManager.logDiscrete("Allure 3 CLI resolved: using system 'allure' binary.");
            return cachedAllureCommandPrefix;
        }

        // 2. npx on PATH
        if (isExecutableOnPath("npx")) {
            cachedAllureCommandPrefix = "npx --yes allure@" + SHAFT.Properties.internal.allure3Version();
            ReportManager.logDiscrete("Allure 3 CLI resolved: using system npx.");
            return cachedAllureCommandPrefix;
        }

        // 3. Download portable Node.js and use its npx
        ReportManager.logDiscrete("Node.js not found on PATH. Downloading portable Node.js v" + SHAFT.Properties.internal.nodeLtsVersion() + " to bootstrap Allure 3 CLI...");
        String downloadedNpxPath = downloadNodeJsPortable();
        if (downloadedNpxPath != null) {
            cachedAllureCommandPrefix = q(downloadedNpxPath) + " --yes allure@" + SHAFT.Properties.internal.allure3Version();
            ReportManager.logDiscrete("Allure 3 CLI resolved: using downloaded Node.js npx.");
            return cachedAllureCommandPrefix;
        }

        // Nothing worked
        cachedAllureCommandPrefix = ""; // sentinel: tried but failed
        return null;
    }

    /**
     * Checks whether an executable is available on the system {@code PATH}.
     *
     * @param name the executable name (e.g. {@code "allure"}, {@code "npx"})
     * @return {@code true} if the executable was found
     */
    private static boolean isExecutableOnPath(String name) {
        try {
            ProcessBuilder pb = SystemUtils.IS_OS_WINDOWS
                    ? new ProcessBuilder("cmd.exe", "/c", "where", name)
                    : new ProcessBuilder("which", name);
            pb.redirectErrorStream(true);
            return pb.start().waitFor() == 0;
        } catch (IOException | InterruptedException e) {
            if (e instanceof InterruptedException) {
                ReportManager.logDiscrete("PATH-check for '" + name + "' was interrupted; assuming not found.");
                Thread.currentThread().interrupt();
            }
            return false;
        }
    }

    /**
     * Downloads and extracts the portable Node.js LTS distribution to {@value #NODEJS_CACHE_DIR}
     * if it is not already cached there.
     *
     * @return the absolute path to the {@code npx} executable inside the extracted archive,
     *         or {@code null} if the download/extraction failed
     */
    private static String downloadNodeJsPortable() {
        String npxPath = getNpxBinPath();
        if (new File(npxPath).exists()) {
            return npxPath; // already downloaded in a previous run
        }

        String downloadUrl = getNodeJsDownloadUrl();
        String archiveName = downloadUrl.substring(downloadUrl.lastIndexOf('/') + 1);
        String archivePath = NODEJS_CACHE_DIR + archiveName;

        if (!new File(NODEJS_CACHE_DIR).mkdirs() && !new File(NODEJS_CACHE_DIR).exists()) {
            ReportManager.logDiscrete("Could not create Node.js cache directory: " + NODEJS_CACHE_DIR);
            return null;
        }
        URL downloaded = internalFileSession.downloadFile(downloadUrl, archivePath);
        if (downloaded == null) {
            ReportManager.logDiscrete("Failed to download portable Node.js from " + downloadUrl);
            return null;
        }

        if (archiveName.endsWith(".zip")) {
            // Windows: reuse existing ZIP support in FileActions
            internalFileSession.unpackArchive(downloaded, NODEJS_CACHE_DIR);
        } else {
            // Linux / macOS: standard tar.gz
            internalTerminalSession.performTerminalCommand(
                    "tar -xzf \"" + archivePath + "\" -C \"" + NODEJS_CACHE_DIR + "\"");
        }

        if (!new File(npxPath).exists()) {
            ReportManager.logDiscrete("Node.js extraction appeared to succeed but npx was not found at: " + npxPath);
            return null;
        }

        // Ensure executables are runnable on Unix
        if (!SystemUtils.IS_OS_WINDOWS) {
            internalTerminalSession.performTerminalCommand("chmod +x \"" + getNodeBinPath() + "\"");
            internalTerminalSession.performTerminalCommand("chmod +x \"" + npxPath + "\"");
        }
        return npxPath;
    }

    /** @return the download URL for the portable Node.js LTS archive for the current OS/arch. */
    private static String getNodeJsDownloadUrl() {
        String arch = System.getProperty("os.arch", "amd64").toLowerCase();
        boolean isArm = arch.contains("aarch64") || arch.contains("arm");
        if (SystemUtils.IS_OS_WINDOWS) {
            return "https://nodejs.org/dist/v" + SHAFT.Properties.internal.nodeLtsVersion() + "/node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-win-x64.zip";
        } else if (SystemUtils.IS_OS_MAC) {
            String suffix = isArm ? "darwin-arm64" : "darwin-x64";
            return "https://nodejs.org/dist/v" + SHAFT.Properties.internal.nodeLtsVersion() + "/node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-" + suffix + ".tar.gz";
        } else {
            String suffix = isArm ? "linux-arm64" : "linux-x64";
            return "https://nodejs.org/dist/v" + SHAFT.Properties.internal.nodeLtsVersion() + "/node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-" + suffix + ".tar.gz";
        }
    }

    /** @return the directory name of the extracted Node.js archive (platform-specific). */
    private static String getNodeJsFolderName() {
        String arch = System.getProperty("os.arch", "amd64").toLowerCase();
        boolean isArm = arch.contains("aarch64") || arch.contains("arm");
        if (SystemUtils.IS_OS_WINDOWS) {
            return "node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-win-x64";
        } else if (SystemUtils.IS_OS_MAC) {
            return "node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-" + (isArm ? "darwin-arm64" : "darwin-x64");
        } else {
            return "node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-" + (isArm ? "linux-arm64" : "linux-x64");
        }
    }

    /** @return absolute path to the {@code node} executable in the downloaded portable distribution. */
    private static String getNodeBinPath() {
        String base = NODEJS_CACHE_DIR + getNodeJsFolderName() + File.separator;
        return SystemUtils.IS_OS_WINDOWS
                ? base + "node.exe"
                : base + "bin" + File.separator + "node";
    }

    /** @return absolute path to the {@code npx} executable in the downloaded portable distribution. */
    private static String getNpxBinPath() {
        String base = NODEJS_CACHE_DIR + getNodeJsFolderName() + File.separator;
        return SystemUtils.IS_OS_WINDOWS
                ? base + "npx.cmd"
                : base + "bin" + File.separator + "npx";
    }

    /** Wraps a path in double-quotes for safe shell inclusion. */
    private static String q(String path) {
        return "\"" + path + "\"";
    }

    private static void createAllureReportArchive() {
        internalFileSession.zipFiles(allureReportPath + "/", "generatedReport_" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")) + ".zip");
    }

    private static void writeEnvironmentVariablesToAllureResultsDirectory() {
        // reads all environment variables and then formats and writes them to be read
        // by the Allure report
        var props = ThreadLocalPropertiesManager.getEffectiveProperties();
        var propertiesFileBuilder = new StringBuilder();
        propertiesFileBuilder.append("<environment>");
        // read properties from any explicit properties files
        for (var i = 0; i < props.size(); i++) {
            String propertyKey = ((String) (props.keySet().toArray())[i]).trim();
            String propertyValue = props.getProperty(propertyKey).trim();

            // excluding empty values, system properties (all system properties have "." in
            // their names), and any git branch issues
            if (!propertyValue.isEmpty() && !propertyValue.contains("==") && !propertyKey.contains(">>>")
                    && !propertyKey.contains("<<<")) {

                if (propertyValue.contains("&")) {
                    propertyValue = propertyValue.replace("&", "&amp;");
                }
                String parameter = "<parameter>" + "<key>" + propertyKey + "</key>" + "<value>" + propertyValue
                        + "</value>" + "</parameter>";
                if (propertyKey.equals("shaftEngineVersion")) {
                    // there's an open issue, when fixed this will be displayed properly
                    // https://github.com/allure-framework/allure2/issues/382
                    propertiesFileBuilder.insert(13, parameter);
                } else {
                    propertiesFileBuilder.append(parameter);
                }
            }
        }
        propertiesFileBuilder.append("</environment>");
        internalFileSession.writeToFile(SHAFT.Properties.paths.allureResults(), "environment.xml",
                RestActions.formatXML(propertiesFileBuilder.toString()));
    }
}
