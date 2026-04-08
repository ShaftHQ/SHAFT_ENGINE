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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
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
            String prefix = resolveAllureCommandPrefix();
            if (prefix == null) return;
            String configPath = System.getProperty("user.dir") + File.separator + allureConfigFileName;
            String resultsPath = getResultsPath();
            // Use `allure open` to regenerate and serve the report via HTTP on a random port.
            // This avoids opening via file:// URL, which causes JavaScript console errors
            // in the Allure 3 awesome SPA (broken base-URL setup and History-API restrictions).
            String openCommand = prefix + " open --config \"" + configPath + "\" \"" + resultsPath + "\"";
            // Run asynchronously so the HTTP server stays alive while the user views the report
            // without blocking SHAFT's main thread or preventing JVM shutdown.
            TerminalActions.getInstance(true, false, true).performTerminalCommand(openCommand);
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
        // create generate_allure_report.sh or generate_allure_report.bat
        // These scripts re-generate the report from allure-results and serve it via HTTP.
        String allure3Version = SHAFT.Properties.internal.allure3Version();
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            commandsToServeAllureReport = Arrays.asList(
                    "@echo off",
                    "where allure >nul 2>&1 && (allure open allure-results) || (npx --yes allure@" + allure3Version + " open allure-results)",
                    "pause", "exit");
            internalFileSession.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            commandsToServeAllureReport = Arrays.asList(
                    "#!/bin/bash",
                    "if command -v allure >/dev/null 2>&1; then",
                    "  allure open allure-results",
                    "else",
                    "  npx --yes allure@" + allure3Version + " open allure-results",
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

        // Pre-process result files to ensure every step has a statusDetails object.
        // The awesome plugin's renderer crashes with a TypeError when step.statusDetails is
        // undefined (absent from passing steps). Adding an empty object prevents the crash
        // and allows test details to be opened without JavaScript console errors.
        patchMissingStatusDetailsInResults(getResultsPath());

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
        String safeReportName = escapeYamlString(reportName);
        String safeOutputDir = escapeYamlString(allureOutPutDirectory.replace("\\", "/"));
        var configBuilder = new StringBuilder();
        configBuilder.append("name: \"").append(safeReportName).append("\"\n");
        configBuilder.append("output: \"").append(safeOutputDir).append("\"\n");

        if (SHAFT.Properties.allure.accumulateHistory()) {
            String historyPath = escapeYamlString((System.getProperty("user.dir") + File.separator + "target"
                    + File.separator + "history.jsonl").replace("\\", "/"));
            configBuilder.append("historyPath: \"").append(historyPath).append("\"\n");
            configBuilder.append("appendHistory: true\n");
        }

        configBuilder.append("\nplugins:\n");
        configBuilder.append("  awesome:\n");
        configBuilder.append("    options:\n");
        configBuilder.append("      reportName: \"").append(safeReportName).append("\"\n");
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

    /**
     * Escapes a string value for safe embedding inside a double-quoted YAML scalar.
     * Handles backslash, double-quote, and common control characters.
     *
     * @param value the raw string value
     * @return the YAML-safe escaped string
     */
    private static String escapeYamlString(String value) {
        if (value == null) return "";
        return value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    /**
     * Patches Allure result JSON files so that every step (at any nesting depth) has a
     * {@code statusDetails} object. The Allure 3 awesome-plugin renderer assumes the field
     * is always present and throws a {@code TypeError: Cannot read properties of undefined
     * (reading 'message')} when a passing step omits it (the default for
     * {@code allure-testng} / {@code allure-junit5} adapters).
     *
     * <p>The patch is purely additive: files and steps that already have
     * {@code statusDetails} are untouched. If the results directory does not exist, or a
     * particular file cannot be read/written, the failure is silently logged so that report
     * generation still proceeds.
     *
     * @param resultsPath path to the directory containing {@code *-result.json} files
     */
    private static void patchMissingStatusDetailsInResults(String resultsPath) {
        File dir = new File(resultsPath);
        if (!dir.isDirectory()) return;
        File[] resultFiles = dir.listFiles((d, name) -> name.endsWith("-result.json"));
        if (resultFiles == null) return;

        for (File file : resultFiles) {
            try {
                String original = Files.readString(file.toPath(), StandardCharsets.UTF_8);
                String patched = patchStatusDetailsInJson(original);
                if (!patched.equals(original)) {
                    Files.writeString(file.toPath(), patched, StandardCharsets.UTF_8);
                }
            } catch (IOException e) {
                ReportManager.logDiscrete("Could not patch statusDetails in " + file.getName() + ": " + e.getMessage());
            }
        }
    }

    /**
     * Adds {@code "statusDetails":{"message":""}} after every {@code "stage":"finished"}
     * token that is not already followed by a {@code "statusDetails"} key in the same step.
     * The transformation works on the raw JSON text to avoid a full-parse dependency while
     * still handling arbitrary nesting.
     *
     * <p>The boundary for "same step" is the position of the <em>next</em> {@code "stage"}
     * token in the string. Any {@code "statusDetails"} found between the current
     * {@code "finished"} and the next {@code "stage"} belongs to the same step object;
     * anything beyond that boundary belongs to a sibling or child step.
     *
     * <p>Because allure result files are small (typically &lt;1 MB) and processed once,
     * the performance impact is negligible.
     *
     * @param json the raw JSON string of a single result file
     * @return the patched JSON string (identical to the input when no patch was needed)
     */
    private static String patchStatusDetailsInJson(String json) {
        StringBuilder sb = new StringBuilder(json.length() + 64);
        String marker = "\"stage\"";
        String finished = "\"finished\"";
        String sdKey = "\"statusDetails\"";
        int pos = 0;
        int len = json.length();
        while (pos < len) {
            int markerIdx = json.indexOf(marker, pos);
            if (markerIdx < 0) {
                sb.append(json, pos, len);
                break;
            }
            // Check if this "stage" refers to "finished"
            int afterMarker = markerIdx + marker.length();
            // skip whitespace and colon
            int valueStart = afterMarker;
            while (valueStart < len && (json.charAt(valueStart) == ' ' || json.charAt(valueStart) == '\t'
                    || json.charAt(valueStart) == ':' || json.charAt(valueStart) == '\n' || json.charAt(valueStart) == '\r')) {
                valueStart++;
            }
            int finishedEnd = valueStart + finished.length();
            boolean isFinished = finishedEnd <= len && json.substring(valueStart, finishedEnd).equals(finished);
            if (!isFinished) {
                sb.append(json, pos, finishedEnd <= len ? finishedEnd : len);
                pos = finishedEnd <= len ? finishedEnd : len;
                continue;
            }
            // Determine the look-ahead boundary: the next "stage" token marks a new step
            // (could be a sibling or a child step). "statusDetails" between finishedEnd and
            // that boundary belongs to the current step; beyond it belongs to another step.
            int nextStageIdx = json.indexOf(marker, finishedEnd);
            int lookaheadEnd = nextStageIdx >= 0 ? nextStageIdx : len;
            boolean alreadyHasSD = json.indexOf(sdKey, finishedEnd) >= 0
                    && json.indexOf(sdKey, finishedEnd) < lookaheadEnd;
            sb.append(json, pos, finishedEnd);
            if (!alreadyHasSD) {
                sb.append(",\"statusDetails\":{\"message\":\"\"}");
            }
            pos = finishedEnd;
        }
        return sb.toString();
    }


    private static String getCommandToCreateAllureReport() {
        String prefix = resolveAllureCommandPrefix();
        if (prefix == null) return null;
        String resultsPath = getResultsPath();
        String configPath = System.getProperty("user.dir") + File.separator + allureConfigFileName;
        // Always quote paths to handle spaces; always pass --config explicitly so
        // allurerc.yaml is guaranteed to be applied regardless of the working directory.
        return prefix + " generate --config \"" + configPath + "\" \""
                + resultsPath + "\" -o \"" + allureOutPutDirectory + "\"";
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
     * if it is not already cached there. The downloaded archive is verified against the SHA-256
     * checksum published by Node.js before extraction; the download is rejected if verification fails.
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

        // Verify the archive checksum before extracting to guard against download corruption
        // or supply-chain tampering.
        if (!verifyNodeJsChecksum(archivePath, downloadUrl, archiveName)) {
            ReportManager.logDiscrete("Node.js archive integrity check failed; deleting corrupt download: " + archivePath);
            new File(archivePath).delete();
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

    /**
     * Verifies the SHA-256 checksum of a downloaded Node.js archive against the official
     * {@code SHASUMS256.txt} file published by nodejs.org.
     *
     * <p>If the checksums file cannot be retrieved (e.g., no network access), or if the archive
     * name is not found in it, the check is skipped with a discrete log message (soft failure).
     * Only a positive mismatch causes a hard failure.
     *
     * @param archivePath  path to the locally downloaded archive
     * @param downloadUrl  the URL the archive was downloaded from (used to derive the checksums URL)
     * @param archiveName  the archive filename (e.g., {@code node-v20.19.1-linux-x64.tar.gz})
     * @return {@code true} if the checksum matches or could not be verified; {@code false} on mismatch
     */
    private static boolean verifyNodeJsChecksum(String archivePath, String downloadUrl, String archiveName) {
        String dir = downloadUrl.substring(0, downloadUrl.lastIndexOf('/') + 1);
        String checksumUrl = dir + "SHASUMS256.txt";
        String checksumFilePath = NODEJS_CACHE_DIR + "SHASUMS256.txt";

        URL checksumDownloaded = internalFileSession.downloadFile(checksumUrl, checksumFilePath);
        if (checksumDownloaded == null) {
            ReportManager.logDiscrete("Could not download Node.js SHASUMS256.txt from " + checksumUrl
                    + ". Skipping integrity check.");
            return true; // soft fail — don't block when checksums are unreachable
        }
        try {
            // Parse expected SHA-256 from the checksums file
            String expectedHash = null;
            for (String line : java.nio.file.Files.readAllLines(java.nio.file.Paths.get(checksumFilePath))) {
                line = line.trim();
                if (line.endsWith("  " + archiveName) || line.endsWith("\t" + archiveName)) {
                    expectedHash = line.split("\\s+")[0];
                    break;
                }
            }
            if (expectedHash == null) {
                ReportManager.logDiscrete("Node.js checksum entry not found for '" + archiveName
                        + "' in SHASUMS256.txt. Skipping integrity check.");
                return true; // soft fail — archive name not listed
            }
            // Compute SHA-256 of the downloaded archive using streaming to avoid large heap allocation
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
            try (java.io.InputStream is = new java.io.FileInputStream(archivePath);
                 java.security.DigestInputStream dis = new java.security.DigestInputStream(is, md)) {
                byte[] buf = new byte[65536];
                //noinspection StatementWithEmptyBody - consume entire stream to compute SHA-256 digest
                while (dis.read(buf) != -1) { }
            }
            byte[] digest = md.digest();
            StringBuilder sb = new StringBuilder(64);
            for (byte b : digest) sb.append(String.format("%02x", b));
            String actualHash = sb.toString();
            if (!expectedHash.equalsIgnoreCase(actualHash)) {
                ReportManager.logDiscrete("Node.js integrity check FAILED for '" + archiveName
                        + "'. Expected SHA-256: " + expectedHash + ", computed: " + actualHash);
                return false;
            }
            return true;
        } catch (Exception e) {
            ReportManager.logDiscrete("Error during Node.js integrity verification: " + e.getMessage()
                    + ". Skipping check.");
            return true; // soft fail — don't block on unexpected errors
        }
    }

    /** @return the download URL for the portable Node.js LTS archive for the current OS/arch. */
    private static String getNodeJsDownloadUrl() {
        String arch = System.getProperty("os.arch", "amd64").toLowerCase();
        boolean isArm = arch.contains("aarch64") || arch.contains("arm");
        String version = SHAFT.Properties.internal.nodeLtsVersion();
        if (SystemUtils.IS_OS_WINDOWS) {
            // Detect Windows ARM64 (os.arch is "aarch64" on 64-bit ARM Windows)
            String winSuffix = isArm ? "win-arm64" : "win-x64";
            return "https://nodejs.org/dist/v" + version + "/node-v" + version + "-" + winSuffix + ".zip";
        } else if (SystemUtils.IS_OS_MAC) {
            String suffix = isArm ? "darwin-arm64" : "darwin-x64";
            return "https://nodejs.org/dist/v" + version + "/node-v" + version + "-" + suffix + ".tar.gz";
        } else {
            String suffix = isArm ? "linux-arm64" : "linux-x64";
            return "https://nodejs.org/dist/v" + version + "/node-v" + version + "-" + suffix + ".tar.gz";
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
