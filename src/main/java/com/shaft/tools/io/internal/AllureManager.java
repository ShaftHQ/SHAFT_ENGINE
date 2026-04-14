package com.shaft.tools.io.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
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
    private AllureManager() {
        throw new IllegalStateException("Utility class");
    }

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
    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
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
        String reportPath = new File(allureReportPath, newFileName).getAbsolutePath();
        if (SystemUtils.IS_OS_WINDOWS) {
            reportPath = reportPath.replace("'", "''");
            internalTerminalSession.performTerminalCommand(
                    "powershell -NoProfile -Command \"Start-Process -FilePath '" + reportPath + "'\""
            );
        } else if (SystemUtils.IS_OS_MAC) {
            internalTerminalSession.performTerminalCommand("open \"" + reportPath + "\"");
        } else {
            internalTerminalSession.performTerminalCommand("xdg-open \"" + reportPath + "\"");
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
        if (allureResultsFolderPath == null || allureResultsFolderPath.isEmpty()) {
            return allureResultsFolderPath;
        }
        char last = allureResultsFolderPath.charAt(allureResultsFolderPath.length() - 1);
        // Only strip a real trailing path separator so that valid directory names are not truncated.
        if (last == '/' || last == '\\') {
            return allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1);
        }
        return allureResultsFolderPath;
    }

    private static void writeGenerateReportShellFilesToProjectDirectory() {
        // create generate_allure_report.sh or generate_allure_report.bat
        // These scripts re-generate the report from allure-results and serve it via HTTP.
        // --config allurerc.yaml is passed explicitly so the singleFile/groupBy/reportName options
        // written by writeAllureConfig() are honoured when the user runs the script manually.
        String allure3Version = SHAFT.Properties.internal.allure3Version();
        String resultsPath = getResultsPath();
        String serveArguments = "serve --config allurerc.yaml \"" + resultsPath + "\"";
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            commandsToServeAllureReport = Arrays.asList(
                    "@echo off",
                    "where allure >nul 2>&1 && (allure " + serveArguments + ") || (npx --yes allure@" + allure3Version + " " + serveArguments + ")",
                    "pause", "exit");
            internalFileSession.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            commandsToServeAllureReport = Arrays.asList(
                    "#!/bin/bash",
                    "if command -v allure >/dev/null 2>&1; then",
                    "  allure " + serveArguments,
                    "else",
                    "  npx --yes allure@" + allure3Version + " " + serveArguments,
                    "fi");
            internalFileSession.writeToFile("", "generate_allure_report.sh", commandsToServeAllureReport);
            // make script executable on Unix-based shells
            internalTerminalSession.performTerminalCommand("chmod u+x generate_allure_report.sh");
        }
    }

    private static void cleanAllureResultsDirectory() {
        if (SHAFT.Properties.allure.cleanResultsDirectory()) {
            // clean allure-results directory before execution
            var allureResultsPath = getResultsPath();
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
        // Patch both test-case results and fixture containers.
        // Container files hold before/after hook steps (befores[]/afters[]) whose steps also
        // need "statusDetails" — otherwise the awesome-plugin throws the same TypeError when
        // navigating to a test whose fixtures ran.
        File[] resultFiles = dir.listFiles((d, name) ->
                name.endsWith("-result.json") || name.endsWith("-container.json"));
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
     * Ensures every step in a result/container JSON has a {@code statusDetails.message} value.
     *
     * <p>The Allure 3 awesome-plugin renderer reads {@code step.statusDetails.message} for each
     * rendered step. Some adapters emit steps with:
     * <ul>
     *   <li>no {@code statusDetails} object at all, or</li>
     *   <li>{@code statusDetails} containing only {@code trace} / flags and no {@code message} key.</li>
     * </ul>
     * Both forms trigger frontend runtime errors and blank test-detail panes. This method normalizes
     * both cases by guaranteeing an object and adding an empty {@code message} when missing.
     *
     * @param json the raw JSON string of a single result or container file
     * @return the patched JSON string (identical to input when no patch was needed)
     */
    private static String patchStatusDetailsInJson(String json) {
        try {
            JsonNode parsed = JSON_MAPPER.readTree(json);
            if (!(parsed instanceof ObjectNode root)) {
                return json;
            }

            boolean changed = false;
            changed |= ensureStatusDetailsMessage(root);

            JsonNode steps = root.get("steps");
            if (steps instanceof ArrayNode) {
                changed |= ensureStatusDetailsInSteps((ArrayNode) steps);
            }

            for (String fixtureKey : new String[]{"befores", "afters"}) {
                JsonNode fixtures = root.get(fixtureKey);
                if (fixtures instanceof ArrayNode fixtureArray) {
                    changed |= pruneEmptyFixtures(fixtureArray);
                    for (JsonNode fixtureNode : fixtureArray) {
                        if (fixtureNode instanceof ObjectNode fixtureObject) {
                            changed |= ensureStatusDetailsMessage(fixtureObject);
                            JsonNode fixtureSteps = fixtureObject.get("steps");
                            if (fixtureSteps instanceof ArrayNode) {
                                changed |= ensureStatusDetailsInSteps((ArrayNode) fixtureSteps);
                            }
                        }
                    }
                }
            }

            if (!changed) {
                return json;
            }
            return JSON_MAPPER.writeValueAsString(root);
        } catch (Exception e) {
            ReportManager.logDiscrete("Could not normalize statusDetails in Allure JSON: " + e.getMessage());
            return json;
        }
    }

    /**
     * Recursively normalizes step nodes so each step includes {@code statusDetails.message}.
     *
     * @param steps the step array to normalize (including any nested child steps)
     * @return {@code true} when at least one step node was modified; otherwise {@code false}
     */
    private static boolean ensureStatusDetailsInSteps(ArrayNode steps) {
        boolean changed = false;
        for (JsonNode stepNode : steps) {
            if (!(stepNode instanceof ObjectNode stepObject)) {
                continue;
            }
            changed |= ensureStatusDetailsMessage(stepObject);
            JsonNode nestedSteps = stepObject.get("steps");
            if (nestedSteps instanceof ArrayNode) {
                changed |= ensureStatusDetailsInSteps((ArrayNode) nestedSteps);
            }
        }
        return changed;
    }

    /**
     * Removes fixture entries that contain no actionable content.
     *
     * <p>A fixture is considered empty when it has no steps, attachments, or parameters.
     * Pruning these entries reduces frontend rendering edge cases in Allure detail panes.
     *
     * @param fixtures the fixture array ({@code befores} or {@code afters}) to prune
     * @return {@code true} when at least one fixture was removed; otherwise {@code false}
     */
    private static boolean pruneEmptyFixtures(ArrayNode fixtures) {
        boolean changed = false;
        for (int i = fixtures.size() - 1; i >= 0; i--) {
            JsonNode fixtureNode = fixtures.get(i);
            if (!(fixtureNode instanceof ObjectNode fixtureObject)) {
                continue;
            }

            int stepsCount = (fixtureObject.get("steps") instanceof ArrayNode stepsArray) ? stepsArray.size() : 0;
            int attachmentsCount = (fixtureObject.get("attachments") instanceof ArrayNode attachmentsArray) ? attachmentsArray.size() : 0;
            int parametersCount = (fixtureObject.get("parameters") instanceof ArrayNode parametersArray) ? parametersArray.size() : 0;

            if (stepsCount == 0 && attachmentsCount == 0 && parametersCount == 0) {
                fixtures.remove(i);
                changed = true;
            }
        }
        return changed;
    }

    /**
     * Ensures the target node has a {@code statusDetails} object with a non-null {@code message}.
     *
     * @param node the result/fixture/step node to normalize
     * @return {@code true} when the node was modified; otherwise {@code false}
     */
    private static boolean ensureStatusDetailsMessage(ObjectNode node) {
        JsonNode statusDetailsNode = node.get("statusDetails");
        ObjectNode statusDetailsObject;
        boolean changed = false;

        if (statusDetailsNode instanceof ObjectNode existingObject) {
            statusDetailsObject = existingObject;
        } else {
            statusDetailsObject = node.putObject("statusDetails");
            changed = true;
        }

        JsonNode messageNode = statusDetailsObject.get("message");
        if (messageNode == null || messageNode.isNull()) {
            statusDetailsObject.put("message", "");
            changed = true;
        }

        return changed;
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

        // Validate both version strings against a strict SemVer-like allowlist before they are
        // embedded in shell command strings. This prevents command injection if a malicious value
        // is supplied via system properties (e.g. -Dallure3Version="3.4.0; rm -rf /").
        String allure3Version = SHAFT.Properties.internal.allure3Version();
        String nodeLtsVersion = SHAFT.Properties.internal.nodeLtsVersion();
        if (!allure3Version.matches("[0-9]+\\.[0-9]+\\.[0-9]+(-[A-Za-z0-9.]+)?")) {
            ReportManager.logDiscrete("Invalid allure3Version value '" + allure3Version
                    + "' — must be a SemVer string (e.g. 3.4.0). Report generation skipped.");
            cachedAllureCommandPrefix = "";
            return null;
        }
        if (!nodeLtsVersion.matches("[0-9]+\\.[0-9]+\\.[0-9]+")) {
            ReportManager.logDiscrete("Invalid nodeLtsVersion value '" + nodeLtsVersion
                    + "' — must be a SemVer string (e.g. 20.19.1). Portable Node.js download skipped.");
            // nodeLtsVersion only affects the download fallback; still try allure/npx on PATH
        }

        // 1. allure binary on PATH
        if (isExecutableOnPath("allure")) {
            cachedAllureCommandPrefix = "allure";
            ReportManager.logDiscrete("Allure 3 CLI resolved: using system 'allure' binary.");
            return cachedAllureCommandPrefix;
        }

        // 2. npx on PATH
        if (isExecutableOnPath("npx")) {
            cachedAllureCommandPrefix = "npx --yes allure@" + allure3Version;
            ReportManager.logDiscrete("Allure 3 CLI resolved: using system npx.");
            return cachedAllureCommandPrefix;
        }

        // 3. Download portable Node.js and use its npx
        ReportManager.logDiscrete("Node.js not found on PATH. Downloading portable Node.js v" + nodeLtsVersion + " to bootstrap Allure 3 CLI...");
        String downloadedNpxPath = downloadNodeJsPortable();
        if (downloadedNpxPath != null) {
            cachedAllureCommandPrefix = q(downloadedNpxPath) + " --yes allure@" + allure3Version;
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
            // Keep the folder name consistent with the download URL suffix selection.
            return "node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-" + (isArm ? "win-arm64" : "win-x64");
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
        internalFileSession.zipFiles(allureOutPutDirectory, "generatedReport_" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")) + ".zip");
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
