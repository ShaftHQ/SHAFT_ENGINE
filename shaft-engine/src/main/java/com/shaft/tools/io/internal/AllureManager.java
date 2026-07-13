package com.shaft.tools.io.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import org.apache.commons.lang3.SystemUtils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Internal utility class that manages the Allure reporting lifecycle for SHAFT test runs.
 * Responsibilities include initialising the results directory, optionally generating an
 * {@code allurerc.yaml} configuration file, bootstrapping the Allure 3 CLI (downloaded
 * automatically when not present), invoking it to produce the HTML report, and optionally
 * producing a ZIP archive.
 *
 * <p>This class is not intended for direct use in test code. It is invoked automatically by the
 * SHAFT framework listeners at suite start and finish.
 *
 * <p><b>Allure 2 compatibility:</b> when a system {@code allure} binary on {@code PATH} reports
 * a version starting with {@code 2.}, SHAFT automatically switches to Allure 2 compatibility mode.
 * In this mode the {@code allurerc.yaml} configuration is not written, the Allure 3-specific
 * {@code statusDetails} patch is skipped, the generate command uses the Allure 2 CLI flags
 * ({@code --clean}), and real-time monitoring is disabled (Allure 2 does not support
 * {@code allure watch}).
 *
 * <p><b>Allure 3 CLI resolution order (batteries-included):</b>
 * <ol>
 *   <li>A globally installed {@code allure} binary on {@code PATH}.
 *       <ul>
 *         <li>If the binary reports a 2.x version, Allure 2 compatibility mode is activated.</li>
 *         <li>If {@code allure.forceConfiguredCliVersion=true}, SHAFT additionally verifies that
 *             the 3.x binary exactly matches {@code SHAFT.Properties.internal.allure3Version()};
 *             mismatched versions are ignored and resolution falls through to npx.</li>
 *       </ul>
 *   </li>
 *   <li>{@code npx} on {@code PATH} → {@code npx --yes allure@<version>} (auto-downloads the configured version).</li>
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
    private static final int INDEX_PATCH_BUFFER_SIZE = 64 * 1024;

    private AllureManager() {
        throw new IllegalStateException("Utility class");
    }

    // ─── Allure 3 report paths & config ────────────────────────────────────────
    private static final String allureReportPath = "allure-report";
    private static final String allureConfigFileName = "allurerc.yaml";
    private static final String ALLURE_ATTACHMENT_PREVIEW_FIX_ID = "shaft-allure-attachment-preview-fix";
    private static final String ALLURE_ATTACHMENT_PREVIEW_SCRIPT_ID = "shaft-allure-attachment-preview-script";
    private static final String ALLURE_THEME_COLORS_ID = "shaft-allure-theme-colors";
    private static final String ALLURE_ATTACHMENT_PREVIEW_FIX_STYLE = """
            <style id="shaft-allure-attachment-preview-fix">
            .shaft-allure-image-modal {
              overflow-y: auto !important;
              overflow-x: hidden !important;
            }
            .shaft-allure-image-preview-wrapper {
              display: block !important;
              overflow: visible !important;
              width: 100% !important;
              height: auto !important;
              max-height: none !important;
            }
            .shaft-allure-image-preview {
              display: block !important;
              width: 100% !important;
              max-width: 100% !important;
              height: auto !important;
              max-height: none !important;
              object-fit: contain !important;
            }
            .shaft-allure-html-modal {
              overflow-y: auto !important;
              overflow-x: hidden !important;
            }
            .shaft-allure-html-preview-wrapper {
              display: block !important;
              overflow: hidden !important;
              width: 100% !important;
              height: min(78vh, 1000px) !important;
              max-width: 100% !important;
            }
            .shaft-allure-html-preview {
              display: block !important;
              width: 100% !important;
              min-width: 0 !important;
              height: 100% !important;
              border: 0 !important;
              background: #fff !important;
            }
            </style>
            """;
    private static final String ALLURE_ATTACHMENT_PREVIEW_FIX_SCRIPT = """
            <script id="shaft-allure-attachment-preview-script">
            (() => {
              const imageSelector = 'img[src^="blob:"], img[src^="data:image/"], img[src*="/attachments/"]';
              const htmlSelector = 'iframe[src^="blob:"], iframe[src*="/attachments/"]';
              const isModalRoot = (element) => {
                const style = window.getComputedStyle(element);
                const rect = element.getBoundingClientRect();
                const zIndex = Number.parseInt(style.zIndex, 10);
                return element.matches('[role="dialog"], [aria-modal="true"]')
                  || style.position === 'fixed'
                  || (['absolute', 'fixed'].includes(style.position)
                    && Number.isFinite(zIndex)
                    && zIndex > 10
                    && rect.width > window.innerWidth * 0.5
                    && rect.height > window.innerHeight * 0.5);
              };
              const findModalRoot = (element) => {
                for (let node = element.parentElement, depth = 0; node && depth < 16; node = node.parentElement, depth += 1) {
                  if (isModalRoot(node)) {
                    return node;
                  }
                }
                return null;
              };
              const markAncestors = (element, modalRoot, className) => {
                for (let node = element.parentElement, depth = 0; node && depth < 16; node = node.parentElement, depth += 1) {
                  const style = window.getComputedStyle(node);
                  const scrollableOverflow = ['auto', 'scroll'].includes(style.overflow)
                    || ['auto', 'scroll'].includes(style.overflowY);
                  const clippedOverflow = style.overflow === 'hidden' || style.overflowY === 'hidden';
                  if (node === modalRoot || scrollableOverflow || clippedOverflow) {
                    node.classList.add(className);
                  }
                  if (node === modalRoot) {
                    break;
                  }
                }
              };
              const patch = () => {
                document.querySelectorAll(imageSelector).forEach((img) => {
                  const modalRoot = findModalRoot(img);
                  if (!modalRoot) {
                    return;
                  }
                  img.classList.add('shaft-allure-image-preview');
                  if (img.parentElement) {
                    img.parentElement.classList.add('shaft-allure-image-preview-wrapper');
                  }
                  markAncestors(img, modalRoot, 'shaft-allure-image-modal');
                });
                document.querySelectorAll(htmlSelector).forEach((frame) => {
                  const modalRoot = findModalRoot(frame);
                  if (!modalRoot) {
                    return;
                  }
                  frame.classList.add('shaft-allure-html-preview');
                  if (frame.parentElement) {
                    frame.parentElement.classList.add('shaft-allure-html-preview-wrapper');
                  }
                  markAncestors(frame, modalRoot, 'shaft-allure-html-modal');
                });
              };
              new MutationObserver(patch).observe(document.documentElement, { childList: true, subtree: true });
              document.addEventListener('click', () => window.setTimeout(patch, 0), true);
              patch();
            })();
            </script>
            """;
    private static final String ALLURE_THEME_COLORS_STYLE = """
            <style id="shaft-allure-theme-colors">
            :root:not([data-theme="dark"]) {
              --color-text-primary: #102a31;
              --color-text-secondary: #181f2a;
              --color-text-muted: #405765;
              --color-text-inverse: #ffffff;
              --color-text-inverse-secondary: #c8d6e7;
              --color-text-on-accent: #ffffff;
              --color-icon-primary: #102a31;
              --color-icon-secondary: #405765;
              --color-icon-muted: #66788a;
              --color-border-subtle: rgba(16, 42, 49, 0.10);
              --color-border-default: rgba(16, 42, 49, 0.18);
              --color-border-medium: rgba(16, 42, 49, 0.28);
              --color-border-control: rgba(16, 42, 49, 0.28);
              --color-border-strong: #102a31;
              --color-focus-ring: #006ec0;
              --color-control-bg: rgba(200, 214, 231, 0.55);
              --color-control-bg-hover: rgba(200, 214, 231, 0.75);
              --color-control-bg-active: rgba(200, 214, 231, 0.92);
              --color-control-bg-ghost-hover: rgba(0, 110, 192, 0.08);
              --color-control-bg-ghost-active: rgba(0, 110, 192, 0.14);
              --color-nav-item-bg-hover: rgba(0, 110, 192, 0.08);
              --color-nav-item-bg-active: rgba(0, 110, 192, 0.14);
              --color-nav-item-bg-active-hover: rgba(0, 110, 192, 0.20);
              --color-nav-item-text-active: #006ec0;
              --color-nav-item-icon-active: #006ec0;
              --color-row-bg-hover: rgba(0, 110, 192, 0.06);
              --color-row-bg-selected: rgba(0, 110, 192, 0.10);
              --color-row-bg-selected-hover: rgba(0, 110, 192, 0.14);
              --color-sorter-fg-active: #006ec0;
              --color-link-text: #006ec0;
              --color-link-text-hover: #006ec0;
              --color-link-text-active: #004d86;
              --color-intent-primary-bg: #006ec0;
              --color-intent-primary-bg-hover: #005da3;
              --color-intent-primary-bg-active: #004d86;
              --color-intent-primary-text: #006ec0;
              --color-intent-primary-on-bg: #ffffff;
              --color-chart-categorical-1: #006ec0;
              --color-chart-categorical-2: #102a31;
              --color-chart-categorical-3: #4b9bd6;
              --color-chart-categorical-4: #181f2a;
              --color-chart-categorical-5: #c8d6e7;
              --color-tag-default-bg: rgba(200, 214, 231, 0.65);
              --color-tag-default-text: #102a31;
            }
            :root[data-theme="dark"] {
              --color-text-primary: #f5fdff;
              --color-text-secondary: #dff5f4;
              --color-text-muted: #c8d6e7;
              --color-text-inverse: #f5fdff;
              --color-text-inverse-secondary: #dff5f4;
              --color-text-on-accent: #07111f;
              --color-icon-primary: #f5fdff;
              --color-icon-secondary: #dff5f4;
              --color-icon-muted: #c8d6e7;
              --color-border-subtle: rgba(223, 245, 244, 0.10);
              --color-border-default: rgba(223, 245, 244, 0.18);
              --color-border-medium: rgba(223, 245, 244, 0.28);
              --color-border-control: rgba(223, 245, 244, 0.28);
              --color-border-strong: #dff5f4;
              --color-focus-ring: #4cc2ff;
              --color-control-bg: rgba(223, 245, 244, 0.08);
              --color-control-bg-hover: rgba(223, 245, 244, 0.13);
              --color-control-bg-active: rgba(223, 245, 244, 0.18);
              --color-control-bg-ghost-hover: rgba(76, 194, 255, 0.10);
              --color-control-bg-ghost-active: rgba(76, 194, 255, 0.16);
              --color-nav-item-bg-hover: rgba(76, 194, 255, 0.10);
              --color-nav-item-bg-active: rgba(76, 194, 255, 0.16);
              --color-nav-item-bg-active-hover: rgba(76, 194, 255, 0.22);
              --color-nav-item-text-active: #4cc2ff;
              --color-nav-item-icon-active: #4cc2ff;
              --color-row-bg-hover: rgba(76, 194, 255, 0.08);
              --color-row-bg-selected: rgba(76, 194, 255, 0.12);
              --color-row-bg-selected-hover: rgba(76, 194, 255, 0.18);
              --color-sorter-fg-active: #4cc2ff;
              --color-link-text: #4cc2ff;
              --color-link-text-hover: #7bcfff;
              --color-link-text-active: #a9dfff;
              --color-intent-primary-bg: #4cc2ff;
              --color-intent-primary-bg-hover: #7bcfff;
              --color-intent-primary-bg-active: #a9dfff;
              --color-intent-primary-text: #4cc2ff;
              --color-intent-primary-on-bg: #07111f;
              --color-chart-categorical-1: #4cc2ff;
              --color-chart-categorical-2: #dff5f4;
              --color-chart-categorical-3: #102a31;
              --color-chart-categorical-4: #f5fdff;
              --color-chart-categorical-5: #c8d6e7;
              --color-tag-default-bg: rgba(223, 245, 244, 0.10);
              --color-tag-default-text: #f5fdff;
            }
            </style>
            """;

    /**
     * Exposes the injected Allure theme-color overrides for the brand-palette drift-guard test
     * (issue #3504), so consistency with {@code ReportHtmlTheme} is checked without reflection.
     *
     * @return the {@code <style>} block SHAFT patches into the generated Allure report
     */
    static String allureThemeColorsStyle() {
        return ALLURE_THEME_COLORS_STYLE;
    }

    // ─── Portable Node.js bootstrap ────────────────────────────────────────────
    /** Cache directory for the downloaded portable Node.js distribution. */
    private static final String NODEJS_CACHE_DIR = System.getProperty("user.home")
            + File.separator + ".m2" + File.separator + "repository"
            + File.separator + "nodejs" + File.separator;

    /** Cached resolved command prefix for allure (e.g. {@code "allure"} or {@code "npx --yes allure@3.x.x"}).
     *  {@code null} means resolution has not happened yet; {@code ""} means no CLI was found. */
    private static volatile String cachedAllureCommandPrefix = null;
    /** {@code true} when the system {@code allure} binary on {@code PATH} reported a 2.x version,
     *  causing SHAFT to activate Allure 2 compatibility mode for this JVM lifetime. */
    private static volatile boolean cachedIsAllure2 = false;
    private static final Pattern SEMVER_IN_TEXT_PATTERN = Pattern.compile("([0-9]+\\.[0-9]+\\.[0-9]+(?:-[A-Za-z0-9.]+)?)");

    // ─── SHAFT internal helpers ─────────────────────────────────────────────────
    private static final TerminalActions internalTerminalSession = TerminalActions.getInstance(false, false, true);
    private static final FileActions internalFileSession = FileActions.getInstance(true);
    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private static String allureResultsFolderPath = "";
    private static String allureOutPutDirectory = "";
    private static volatile Process realtimeMonitoringProcess;

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
        ReportManager.logDiscrete("Preparing Allure reporting.");
        /*
         * Force screenshot link to be shown in the results as a link not text
         */
        ThreadLocalPropertiesManager.setGlobalProperty("org.uncommons.reportng.escape-output", "false");
        allureResultsFolderPath = SHAFT.Properties.paths.allureResults();
        cleanAllureReportDirectory();
        cleanAllureResultsDirectory();
        writeEnvironmentVariablesToAllureResultsDirectory();
        if (!SHAFT.Properties.allure.generateReport()) {
            ReportManager.logDiscrete("Allure report generation is disabled.");
            return;
        }
        // Resolve the Allure CLI now so cachedIsAllure2 is set before we generate the helper scripts.
        resolveAllureCommandPrefix();
        writeGenerateReportShellFilesToProjectDirectory();
        startRealtimeMonitoringIfEligible();
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
        stopRealtimeMonitoring();
        if (!SHAFT.Properties.allure.generateReport()) {
            ReportManager.logDiscrete("Allure report generation is disabled.");
            return;
        }
        writeAllureReport();
        copyAndOpenAllure();
    }

    private static void copyAndOpenAllure() {
        internalFileSession.copyFolder(allureOutPutDirectory, reportDirectoryPath().toString());
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
        String sourceFile = reportDirectoryPath().resolve("index.html").toString();
        if (!internalFileSession.doesFileExist(sourceFile)) {
            // Allure report was not generated (CLI bootstrap failed or allure-results was empty)
            ReportManager.logDiscrete("Allure report index.html was not found. Report generation may have failed."
                    + " Check that allure-results is not empty and that SHAFT could reach nodejs.org to download Node.js.");
            return null;
        }
        internalFileSession.renameFile(sourceFile, newFileName);
        return newFileName;
    }

    private static boolean openAllureReport(String newFileName) {
        if (!SHAFT.Properties.allure.automaticallyOpen()) {
            ReportManager.logDiscrete("Allure report automatic opening is disabled.");
            return false;
        }
        String reportPath = reportDirectoryPath().resolve(newFileName).toFile().getAbsolutePath();
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
        return true;
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
        if (!SHAFT.Properties.allure.generateReport()) {
            ReportManager.logDiscrete("Allure report generation is disabled.");
            return;
        }
        if (SHAFT.Properties.allure.generateArchive()) {
            ReportManager.logDiscrete("Generating Allure report archive.");
            ReportHelper.disableLogging();
            stopRealtimeMonitoring();
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

    private static Path getExecutionRootPath() {
        String resultsPath = getResultsPath();
        if (resultsPath != null && !resultsPath.isBlank()) {
            Path configuredResultsPath = Path.of(resultsPath);
            if (configuredResultsPath.isAbsolute()) {
                Path normalizedResultsPath = configuredResultsPath.toAbsolutePath().normalize();
                Path parent = normalizedResultsPath.getParent();
                return parent == null ? normalizedResultsPath : parent;
            }
        }
        return Path.of(System.getProperty("user.dir")).toAbsolutePath().normalize();
    }

    private static Path resolveExecutionPath(String first, String... more) {
        return getExecutionRootPath().resolve(Path.of(first, more)).normalize();
    }

    private static Path reportDirectoryPath() {
        return resolveExecutionPath(allureReportPath);
    }

    private static Path reportOutputDirectoryPath() {
        return resolveExecutionPath("target", allureReportPath);
    }

    private static Path reportWatchOutputDirectoryPath() {
        return resolveExecutionPath("target", "allure-watch-report");
    }

    private static Path allureConfigPath() {
        return resolveExecutionPath(allureConfigFileName);
    }

    private static Path allureRealtimeLogPath() {
        return resolveExecutionPath("target", "allure-watch.log");
    }

    private static void writeGenerateReportShellFilesToProjectDirectory() {
        // create generate_allure_report.sh or generate_allure_report.bat
        // These scripts re-generate the report from allure-results and serve it via HTTP.
        String resultsPath = getResultsPath();

        if (cachedIsAllure2) {
            // Allure 2 mode: simple serve command, no --config needed.
            if (SystemUtils.IS_OS_WINDOWS) {
                List<String> commands = Arrays.asList(
                        "@echo off",
                        "allure serve \"" + resultsPath + "\"",
                        "pause", "exit");
                internalFileSession.writeToFile(resolveExecutionPath("generate_allure_report.bat").toString(),
                        String.join(System.lineSeparator(), commands));
            } else {
                List<String> commands = Arrays.asList(
                        "#!/bin/bash",
                        "allure serve \"" + resultsPath + "\"");
                Path scriptPath = resolveExecutionPath("generate_allure_report.sh");
                internalFileSession.writeToFile(scriptPath.toString(), String.join(System.lineSeparator(), commands));
                internalTerminalSession.performTerminalCommand("chmod u+x \"" + scriptPath + "\"");
            }
            return;
        }

        // Allure 3 mode: --config is passed explicitly so the singleFile/groupBy/reportName
        // options written by writeAllureConfig() are honoured when the user runs the script manually.
        String allure3Version = SHAFT.Properties.internal.allure3Version();
        boolean enforceConfiguredCliVersion = SHAFT.Properties.allure.forceConfiguredCliVersion();
        String serveArguments = "serve --config \"" + allureConfigPath() + "\" \"" + resultsPath + "\"";
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            if (enforceConfiguredCliVersion) {
                commandsToServeAllureReport = Arrays.asList(
                        "@echo off",
                        "npx --yes allure@" + allure3Version + " " + serveArguments,
                        "pause", "exit");
            } else {
                commandsToServeAllureReport = Arrays.asList(
                        "@echo off",
                        "where allure >nul 2>&1 && (allure " + serveArguments + ") || (npx --yes allure@" + allure3Version + " " + serveArguments + ")",
                        "pause", "exit");
            }
            internalFileSession.writeToFile(resolveExecutionPath("generate_allure_report.bat").toString(),
                    String.join(System.lineSeparator(), commandsToServeAllureReport));
        } else {
            if (enforceConfiguredCliVersion) {
                commandsToServeAllureReport = Arrays.asList(
                        "#!/bin/bash",
                        "npx --yes allure@" + allure3Version + " " + serveArguments);
            } else {
                commandsToServeAllureReport = Arrays.asList(
                        "#!/bin/bash",
                        "if command -v allure >/dev/null 2>&1; then",
                        "  allure " + serveArguments,
                        "else",
                        "  npx --yes allure@" + allure3Version + " " + serveArguments,
                        "fi");
            }
            Path scriptPath = resolveExecutionPath("generate_allure_report.sh");
            internalFileSession.writeToFile(scriptPath.toString(), String.join(System.lineSeparator(), commandsToServeAllureReport));
            // make script executable on Unix-based shells
            internalTerminalSession.performTerminalCommand("chmod u+x \"" + scriptPath + "\"");
        }
    }

    private static void cleanAllureResultsDirectory() {
        if (SHAFT.Properties.allure.cleanResultsDirectory()) {
            var allureResultsPath = getResultsPath();
            try {
                ensureAllureResultsDirectoryExists(allureResultsPath);
                cleanAllureResultsDirectoryContents(allureResultsPath);
            } catch (Exception t) {
                ReportManager.log("Could not clean '" + allureResultsPath + "' because it is currently open. Restart the machine to unlock the directory.");
            }
            ensureAllureResultsDirectoryExists(allureResultsPath);
        }
    }

    private static void cleanAllureResultsDirectoryContents(String allureResultsPath) throws IOException {
        if (allureResultsPath == null || allureResultsPath.isBlank()) {
            return;
        }
        var resultsDirectory = new File(allureResultsPath).toPath();
        if (!Files.isDirectory(resultsDirectory)) {
            return;
        }
        try (var resultsDirectoryContents = Files.walk(resultsDirectory)) {
            resultsDirectoryContents
                    .sorted(Comparator.reverseOrder())
                    .filter(path -> !path.equals(resultsDirectory))
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException e) {
                            ReportManager.log("Could not delete '" + path + "' from Allure results: " + e.getMessage());
                        }
                    });
        }
    }

    private static void ensureAllureResultsDirectoryExists(String allureResultsPath) {
        if (allureResultsPath == null || allureResultsPath.isBlank()) {
            return;
        }
        var resultsDirectory = new File(allureResultsPath).toPath();
        try {
            Files.createDirectories(resultsDirectory);
        } catch (FileAlreadyExistsException e) {
            if (!Files.isDirectory(resultsDirectory)) {
                ReportManager.log("Could not create '" + allureResultsPath + "' for Allure results: " + e.getMessage());
            }
        } catch (IOException e) {
            ReportManager.log("Could not create '" + allureResultsPath + "' for Allure results: " + e.getMessage());
        }
    }

    private static void cleanAllureReportDirectory() {
        // clean allure-report directory before execution
        if (!SHAFT.Properties.allure.accumulateReports()) {
            try {
                internalFileSession.deleteFolder(reportDirectoryPath().toString());
            } catch (Exception t) {
                ReportManager.log("Could not clean '" + reportDirectoryPath() + "' because it is currently open. Restart the machine to unlock the directory.");
            }
        }
    }

    private static void writeAllureReport() {
        allureOutPutDirectory = reportOutputDirectoryPath().toString();
        var customReportName = SHAFT.Properties.allure.customTitle();
        internalFileSession.createFolder(allureOutPutDirectory);

        if (!cachedIsAllure2) {
            // Pre-process result files for Awesome report compatibility and SHAFT's default tree.
            // This prevents crashes on absent statusDetails and normalizes package/testClass labels
            // before Allure groups results.
            patchMissingStatusDetailsInResults(getResultsPath());
            writeAllureCategoriesIfSupported();

            // Write allurerc.yaml with current settings (including custom title and optional history path)
            writeAllureConfig(customReportName, allureOutPutDirectory);
        }

        // Resolve the Allure CLI and generate the report synchronously so stdout, stderr,
        // and the exit code are visible in terminal/log output when generation fails.
        String cmd = getCommandToCreateAllureReport();
        if (cmd != null && !cmd.isBlank()) {
            executeAllureGenerateCommand(cmd);
            if (!cachedIsAllure2) {
                patchGeneratedAllureReportIndex(Path.of(allureOutPutDirectory));
            }
        } else {
            ReportManager.logDiscrete("Allure report generation skipped because the Allure CLI could not be resolved."
                    + (cachedIsAllure2
                    ? " Ensure 'allure' (version 2) is on PATH."
                    : " Install Node.js (https://nodejs.org) and re-run to generate the report."));
        }
    }

    /**
     * Writes an {@code allurerc.yaml} configuration file to the project working directory
     * before invoking the Allure 3 CLI. The file captures the custom report name, output
     * directory, optional history path, and awesome-plugin options (single-file mode, grouping).
     *
     * @param reportName the display name for the generated report
     * @param outputDirectory the target directory where the Allure CLI writes generated report files
     */
    private static void writeAllureConfig(String reportName, String outputDirectory) {
        String safeReportName = escapeYamlString(reportName);
        String safeOutputDir = escapeYamlString(outputDirectory.replace("\\", "/"));
        String safeCustomLogo = escapeYamlString(SHAFT.Properties.allure.customLogo());
        var configBuilder = new StringBuilder();
        configBuilder.append("name: \"").append(safeReportName).append("\"\n");
        configBuilder.append("output: \"").append(safeOutputDir).append("\"\n");

        if (SHAFT.Properties.allure.accumulateHistory()) {
            String historyPath = escapeYamlString(resolveExecutionPath("target", "history.jsonl").toString().replace("\\", "/"));
            configBuilder.append("historyPath: \"").append(historyPath).append("\"\n");
            configBuilder.append("appendHistory: true\n");
        }

        configBuilder.append("\nplugins:\n");
        configBuilder.append("  awesome:\n");
        configBuilder.append("    options:\n");
        configBuilder.append("      reportName: \"").append(safeReportName).append("\"\n");
        configBuilder.append("      singleFile: ").append(SHAFT.Properties.allure.singleFile()).append("\n");
        if (!safeCustomLogo.isBlank()) {
            configBuilder.append("      logo: \"").append(safeCustomLogo).append("\"\n");
        }
        configBuilder.append("      theme: \"").append(normalizeAllureTheme(SHAFT.Properties.allure.theme())).append("\"\n");
        configBuilder.append("      reportLanguage: \"")
                .append(escapeYamlString(SHAFT.Properties.allure.reportLanguage()))
                .append("\"\n");
        configBuilder.append("      open: ").append(SHAFT.Properties.allure.open()).append("\n");
        configBuilder.append("      groupBy:\n");
        for (String groupByOption : getAllureGroupByOptions()) {
            configBuilder.append("        - ").append(escapeYamlString(groupByOption)).append("\n");
        }

        internalFileSession.writeToFile(
                allureConfigPath().toString(),
                configBuilder.toString());
    }

    private static List<String> getAllureGroupByOptions() {
        List<String> configuredOptions = Arrays.stream(SHAFT.Properties.allure.groupBy().split(","))
                .map(String::trim)
                .filter(option -> !option.isBlank())
                .toList();
        if (configuredOptions.isEmpty()) {
            return List.of("package", "testClass");
        }
        return configuredOptions;
    }

    private static String normalizeAllureTheme(String theme) {
        if (theme == null) {
            return "auto";
        }
        String normalizedTheme = theme.trim().toLowerCase();
        return switch (normalizedTheme) {
            case "light", "dark", "auto" -> normalizedTheme;
            default -> "auto";
        };
    }

    /**
     * Result of a single streaming pass over {@code index.html} looking for the SHAFT patch markers
     * and the insertion points ({@code </head>} / {@code </body>}).
     *
     * @param previewFixPresent    {@code true} when the attachment-preview-fix {@code <style>} id is present
     * @param previewScriptPresent {@code true} when the attachment-preview-fix {@code <script>} id is present
     * @param themeColorsPresent   {@code true} when the theme-colors {@code <style>} id is present
     * @param headEndOffset        absolute byte offset of the first {@code </head>}, or {@code -1} if absent
     * @param bodyEndOffset        absolute byte offset of the last {@code </body>}, or {@code -1} if absent
     */
    // package-private so unit tests can assert scan results without reflection
    record IndexMarkerScan(boolean previewFixPresent, boolean previewScriptPresent,
                           boolean themeColorsPresent, long headEndOffset, long bodyEndOffset) {
    }

    /** A byte-range insertion to apply while streaming {@code index.html} to its patched copy. */
    private record IndexInsertion(long offset, byte[] bytes) {
    }

    /**
     * Patches the generated Allure single-file report's {@code index.html} to inject SHAFT's
     * attachment-preview-fix styling/script and theme-color overrides, without ever holding the
     * whole file in memory.
     *
     * <p>Single-file Allure reports inline every attachment as base64, so {@code index.html} can
     * grow far beyond the forked JVM's heap; the previous implementation read the whole file into a
     * {@code String} and built additional full copies via {@code String.substring} concatenation,
     * which could throw {@link OutOfMemoryError} during teardown (issue #3407). This implementation
     * instead scans the file once for markers/insertion-points (bounded 64KB buffer) and then streams
     * a second, single sequential pass to write the patched copy.
     */
    private static void patchGeneratedAllureReportIndex(Path reportDirectory) {
        Path indexPath = reportDirectory.resolve("index.html");
        if (!Files.isRegularFile(indexPath)) {
            return;
        }
        try {
            IndexMarkerScan scan;
            try (InputStream in = new BufferedInputStream(Files.newInputStream(indexPath), INDEX_PATCH_BUFFER_SIZE)) {
                scan = scanIndexMarkers(in);
            }
            String headPatch = "";
            if (!scan.previewFixPresent()) {
                headPatch += ALLURE_ATTACHMENT_PREVIEW_FIX_STYLE;
            }
            if (!scan.previewScriptPresent()) {
                headPatch += ALLURE_ATTACHMENT_PREVIEW_FIX_SCRIPT;
            }
            String bodyPatch = scan.themeColorsPresent() ? "" : ALLURE_THEME_COLORS_STYLE;
            if (headPatch.isEmpty() && bodyPatch.isEmpty()) {
                return;
            }
            writePatchedIndex(indexPath, scan, headPatch, bodyPatch);
        } catch (IOException e) {
            ReportManager.logDiscrete("Could not patch Allure report styling: " + e.getMessage());
        }
    }

    /**
     * Streams {@code in} once, locating the SHAFT patch id markers and the first {@code </head>} /
     * last {@code </body>} byte offsets. Uses a sliding window with a carried-over tail so that a
     * marker split across two reads (regardless of chunk size) is still found.
     */
    private static IndexMarkerScan scanIndexMarkers(InputStream in) throws IOException {
        byte[] previewFixMarker = ("id=\"" + ALLURE_ATTACHMENT_PREVIEW_FIX_ID + "\"").getBytes(StandardCharsets.UTF_8);
        byte[] previewScriptMarker = ("id=\"" + ALLURE_ATTACHMENT_PREVIEW_SCRIPT_ID + "\"").getBytes(StandardCharsets.UTF_8);
        byte[] themeColorsMarker = ("id=\"" + ALLURE_THEME_COLORS_ID + "\"").getBytes(StandardCharsets.UTF_8);
        byte[] headEndMarker = "</head>".getBytes(StandardCharsets.UTF_8);
        byte[] bodyEndMarker = "</body>".getBytes(StandardCharsets.UTF_8);

        boolean previewFixPresent = false;
        boolean previewScriptPresent = false;
        boolean themeColorsPresent = false;
        boolean headFound = false;
        long headEndOffset = -1;
        long bodyEndOffset = -1;

        int maxMarkerLength = Math.max(previewFixMarker.length,
                Math.max(previewScriptMarker.length,
                        Math.max(themeColorsMarker.length,
                                Math.max(headEndMarker.length, bodyEndMarker.length))));
        int carryLength = maxMarkerLength - 1;
        byte[] window = new byte[carryLength + INDEX_PATCH_BUFFER_SIZE];
        int carried = 0;
        long windowStartOffset = 0;

        while (true) {
            int read = in.read(window, carried, window.length - carried);
            if (read < 0) {
                break;
            }
            if (read == 0) {
                continue;
            }
            int windowLength = carried + read;

            if (!previewFixPresent) {
                int minStart = Math.max(0, carried - previewFixMarker.length + 1);
                previewFixPresent = indexOfMarker(window, windowLength, minStart, previewFixMarker, false) >= 0;
            }
            if (!previewScriptPresent) {
                int minStart = Math.max(0, carried - previewScriptMarker.length + 1);
                previewScriptPresent = indexOfMarker(window, windowLength, minStart, previewScriptMarker, false) >= 0;
            }
            if (!themeColorsPresent) {
                int minStart = Math.max(0, carried - themeColorsMarker.length + 1);
                themeColorsPresent = indexOfMarker(window, windowLength, minStart, themeColorsMarker, false) >= 0;
            }
            if (!headFound) {
                int minStart = Math.max(0, carried - headEndMarker.length + 1);
                int matchIndex = indexOfMarker(window, windowLength, minStart, headEndMarker, false);
                if (matchIndex >= 0) {
                    headEndOffset = windowStartOffset + matchIndex;
                    headFound = true;
                }
            }
            int bodyMinStart = Math.max(0, carried - bodyEndMarker.length + 1);
            int bodyMatchIndex = indexOfMarker(window, windowLength, bodyMinStart, bodyEndMarker, true);
            if (bodyMatchIndex >= 0) {
                bodyEndOffset = windowStartOffset + bodyMatchIndex;
            }

            int newCarry = Math.min(carryLength, windowLength);
            System.arraycopy(window, windowLength - newCarry, window, 0, newCarry);
            windowStartOffset += windowLength - newCarry;
            carried = newCarry;
        }

        return new IndexMarkerScan(previewFixPresent, previewScriptPresent, themeColorsPresent, headEndOffset, bodyEndOffset);
    }

    /**
     * Naive byte-array search for {@code marker} within {@code window[0, windowLength)}, restricted
     * to start indices {@code >= minStart}. Returns the first match when {@code findLast} is
     * {@code false}, otherwise the last match; {@code -1} when none is found.
     */
    private static int indexOfMarker(byte[] window, int windowLength, int minStart, byte[] marker, boolean findLast) {
        int maxStart = windowLength - marker.length;
        int result = -1;
        for (int i = minStart; i <= maxStart; i++) {
            boolean matches = true;
            for (int j = 0; j < marker.length; j++) {
                if (window[i + j] != marker[j]) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                result = i;
                if (!findLast) {
                    return result;
                }
            }
        }
        return result;
    }

    /**
     * Streams {@code indexPath} to a sibling temp file, inserting {@code headPatch} immediately
     * before {@code scan.headEndOffset()} (or at offset 0 if absent) and {@code bodyPatch}
     * immediately before {@code scan.bodyEndOffset()} (or at EOF if absent), then atomically
     * replaces {@code indexPath}. Offsets are computed on the original (unpatched) file; since
     * neither patch constant contains the literals {@code </head>} or {@code </body>}, this is
     * equivalent to the old post-insertion string arithmetic.
     */
    private static void writePatchedIndex(Path indexPath, IndexMarkerScan scan, String headPatch, String bodyPatch) throws IOException {
        List<IndexInsertion> insertions = new ArrayList<>(2);
        if (!headPatch.isEmpty()) {
            long offset = scan.headEndOffset() >= 0 ? scan.headEndOffset() : 0L;
            insertions.add(new IndexInsertion(offset, headPatch.getBytes(StandardCharsets.UTF_8)));
        }
        if (!bodyPatch.isEmpty()) {
            long offset = scan.bodyEndOffset() >= 0 ? scan.bodyEndOffset() : Long.MAX_VALUE;
            insertions.add(new IndexInsertion(offset, bodyPatch.getBytes(StandardCharsets.UTF_8)));
        }
        insertions.sort(Comparator.comparingLong(IndexInsertion::offset));

        Path tempPath = indexPath.resolveSibling(indexPath.getFileName() + ".shaft-patch.tmp");
        try {
            try (InputStream in = new BufferedInputStream(Files.newInputStream(indexPath), INDEX_PATCH_BUFFER_SIZE);
                 OutputStream out = new BufferedOutputStream(Files.newOutputStream(tempPath), INDEX_PATCH_BUFFER_SIZE)) {
                long position = 0;
                for (IndexInsertion insertion : insertions) {
                    position = copyIndexBytes(in, out, position, insertion.offset());
                    out.write(insertion.bytes());
                }
                copyIndexBytes(in, out, position, Long.MAX_VALUE);
            }
        } catch (IOException e) {
            Files.deleteIfExists(tempPath);
            throw e;
        }
        try {
            Files.move(tempPath, indexPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (AtomicMoveNotSupportedException e) {
            Files.move(tempPath, indexPath, StandardCopyOption.REPLACE_EXISTING);
        }
    }

    /**
     * Copies bytes from {@code in} to {@code out}, starting at absolute {@code position}, until
     * absolute {@code targetOffset} is reached ({@code Long.MAX_VALUE} means "copy to EOF"). Reads
     * at most {@code min(bufferLength, targetOffset - position)} per iteration. Returns the new
     * position (which may be short of {@code targetOffset} if EOF was reached first).
     */
    private static long copyIndexBytes(InputStream in, OutputStream out, long position, long targetOffset) throws IOException {
        byte[] buffer = new byte[INDEX_PATCH_BUFFER_SIZE];
        long currentPosition = position;
        while (targetOffset == Long.MAX_VALUE || currentPosition < targetOffset) {
            long remaining = targetOffset == Long.MAX_VALUE ? buffer.length : targetOffset - currentPosition;
            int toRead = (int) Math.min(buffer.length, remaining);
            int read = in.read(buffer, 0, toRead);
            if (read < 0) {
                break;
            }
            out.write(buffer, 0, read);
            currentPosition += read;
        }
        return currentPosition;
    }

    private static void writeAllureCategoriesIfSupported() {
        if (cachedIsAllure2) {
            return;
        }
        String resultsPath = getResultsPath();
        if (resultsPath == null || resultsPath.isBlank()) {
            return;
        }
        ensureAllureResultsDirectoryExists(resultsPath);
        try {
            Files.writeString(Path.of(resultsPath).resolve("categories.json"), defaultAllureCategoriesJson(),
                    StandardCharsets.UTF_8);
        } catch (IOException e) {
            ReportManager.logDiscrete("Could not write Allure categories.json: " + e.getMessage());
        }
    }

    private static String defaultAllureCategoriesJson() {
        return """
                [
                  {
                    "name": "Assertion / validation failure",
                    "matchedStatuses": ["failed"],
                    "messageRegex": ".*(Assertion|Assert|Verification|expected .* actual|expected .* but found).*"
                  },
                  {
                    "name": "Locator / element interaction",
                    "matchedStatuses": ["failed", "broken"],
                    "traceRegex": ".*(NoSuchElementException|StaleElementReferenceException|TimeoutException|ElementClickInterceptedException|ElementNotInteractableException|locator|element).*"
                  },
                  {
                    "name": "API contract or response mismatch",
                    "matchedStatuses": ["failed", "broken"],
                    "messageRegex": ".*(OpenAPI|Swagger|JSON|XML|schema|status code|response).*"
                  },
                  {
                    "name": "Accessibility violation",
                    "matchedStatuses": ["failed", "broken"],
                    "messageRegex": ".*(Accessibility|axe|WCAG|violation).*"
                  },
                  {
                    "name": "Performance budget violation",
                    "matchedStatuses": ["failed", "broken"],
                    "messageRegex": ".*(performance budget|p95|latency|duration).*"
                  },
                  {
                    "name": "Provider / grid / device issue",
                    "matchedStatuses": ["broken"],
                    "traceRegex": ".*(SessionNotCreatedException|UnreachableBrowserException|WebDriverException|BrowserStack|LambdaTest|Appium|Selenium Grid|connection refused).*"
                  },
                  {
                    "name": "Environment / configuration issue",
                    "matchedStatuses": ["broken"],
                    "messageRegex": ".*(configuration|property|missing|not found|permission denied|invalid).*"
                  },
                  {
                    "name": "SHAFT: flaky (passed on retry)",
                    "matchedStatuses": ["passed"],
                    "messageRegex": ".*(retry|retried|flaky|passed on attempt).*"
                  },
                  {
                    "name": "SHAFT: self-healed locator",
                    "matchedStatuses": ["passed", "failed", "broken"],
                    "messageRegex": ".*(self-healed|self healing|healed locator|locator healing|Healenium).*"
                  },
                  {
                    "name": "SHAFT: soft verification failure",
                    "matchedStatuses": ["failed"],
                    "messageRegex": ".*(Verification failed|verifyThat|soft assert).*"
                  }
                ]
                """;
    }

    private static void executeAllureGenerateCommand(String command) {
        ReportManager.logDiscrete("Running Allure report generation command: \"" + command + "\".");
        ProcessBuilder processBuilder = SystemUtils.IS_OS_WINDOWS
                ? new ProcessBuilder("cmd.exe", "/c", command)
                : new ProcessBuilder("sh", "-c", command);
        processBuilder.directory(getExecutionRootPath().toFile());

        ExecutorService streamReaders = Executors.newFixedThreadPool(2);
        try {
            Process process = processBuilder.start();
            Future<String> stdout = streamReaders.submit(() -> readProcessStream(process.getInputStream()));
            Future<String> stderr = streamReaders.submit(() -> readProcessStream(process.getErrorStream()));

            boolean completed = process.waitFor(SHAFT.Properties.timeouts.shellSessionTimeout(), TimeUnit.MINUTES);
            if (!completed) {
                process.destroyForcibly();
                ReportManager.logDiscrete("Allure report generation timed out after "
                        + SHAFT.Properties.timeouts.shellSessionTimeout() + " minute(s). Command: " + command);
                logAllureCommandOutput(stdout, stderr);
                return;
            }

            int exitCode = process.exitValue();
            logAllureCommandOutput(stdout, stderr);
            ReportManager.logDiscrete("Allure report generation exited with code " + exitCode + ".");
            if (exitCode != 0) {
                ReportManager.logDiscrete("Allure report generation did not complete successfully. Command: " + command);
            }
        } catch (IOException e) {
            ReportManager.logDiscrete("Could not execute Allure report generation command: " + e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            ReportManager.logDiscrete("Allure report generation was interrupted: " + e.getMessage());
        } finally {
            streamReaders.shutdownNow();
        }
    }

    private static void logAllureCommandOutput(Future<String> stdout, Future<String> stderr) {
        String output = getCompletedProcessOutput(stdout, "stdout");
        if (!output.isBlank()) {
            ReportManager.logDiscrete("Allure generate stdout:\n" + output.trim());
        }

        String error = getCompletedProcessOutput(stderr, "stderr");
        if (!error.isBlank()) {
            ReportManager.logDiscrete("Allure generate stderr:\n" + error.trim());
        }
    }

    private static String getCompletedProcessOutput(Future<String> output, String streamName) {
        try {
            return output.get();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            ReportManager.logDiscrete("Interrupted while reading Allure generate " + streamName + ": " + e.getMessage());
        } catch (ExecutionException e) {
            ReportManager.logDiscrete("Could not read Allure generate " + streamName + ": " + e.getMessage());
        }
        return "";
    }

    private static String readProcessStream(InputStream inputStream) throws IOException {
        return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
    }

    private static void startRealtimeMonitoringIfEligible() {
        if (!SHAFT.Properties.allure.realtimeMonitoring()) {
            return;
        }
        if (!isRealtimeMonitoringExecutionContextEligible()) {
            return;
        }

        // Allure 2 does not support the 'watch' command
        if (cachedIsAllure2) {
            ReportManager.logDiscrete("Allure real-time monitoring is not supported with Allure 2.");
            return;
        }

        String prefix = resolveAllureCommandPrefix();
        if (prefix == null || prefix.isBlank()) {
            ReportManager.logDiscrete("Allure real-time monitoring skipped because the Allure CLI could not be resolved.");
            return;
        }

        if (realtimeMonitoringProcess != null && realtimeMonitoringProcess.isAlive()) {
            return;
        }

        String reportName = SHAFT.Properties.allure.customTitle();
        Path watchOutputDirectory = reportWatchOutputDirectoryPath();
        writeAllureConfig(reportName, watchOutputDirectory.toString());

        String command = prefix + " watch "
                + "--config " + q(allureConfigPath().toString()) + " "
                + "--output " + q(watchOutputDirectory.toString()) + " "
                + "--report-name " + q(reportName) + " "
                + (SHAFT.Properties.allure.automaticallyOpen() ? "--open " : "")
                + q(getResultsPath());
        realtimeMonitoringProcess = startLongRunningCommand(command);
        if (realtimeMonitoringProcess != null && realtimeMonitoringProcess.isAlive()) {
            ReportManager.logDiscrete("Allure real-time monitoring started.");
        }
    }

    private static boolean isRealtimeMonitoringExecutionContextEligible() {
        // Allure 3 watch supports local and remote contexts; keep this hook for future policy gates.
        return true;
    }

    private static Process startLongRunningCommand(String command) {
        try {
            ProcessBuilder processBuilder = SystemUtils.IS_OS_WINDOWS
                    ? new ProcessBuilder("cmd.exe", "/c", command)
                    : new ProcessBuilder("sh", "-c", command);
            Path executionRoot = getExecutionRootPath();
            if (!Files.isDirectory(executionRoot)) {
                return null;
            }
            Path realtimeLogPath = allureRealtimeLogPath();
            Files.createDirectories(realtimeLogPath.getParent());
            processBuilder.directory(executionRoot.toFile());
            processBuilder.redirectErrorStream(true);
            processBuilder.redirectOutput(ProcessBuilder.Redirect.appendTo(realtimeLogPath.toFile()));
            return processBuilder.start();
        } catch (IOException e) {
            ReportManager.logDiscrete("Could not start Allure real-time monitoring: " + e.getMessage());
            return null;
        }
    }

    private static void stopRealtimeMonitoring() {
        Process process = realtimeMonitoringProcess;
        realtimeMonitoringProcess = null;
        if (process == null || !process.isAlive()) {
            return;
        }

        process.destroy();
        try {
            if (!process.waitFor(5, TimeUnit.SECONDS)) {
                process.destroyForcibly();
            }
            ReportManager.logDiscrete("Allure real-time monitoring stopped.");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            process.destroyForcibly();
        }
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
     * Patches Allure result JSON files so steps have {@code statusDetails} and class labels
     * can group reports by Java package and test class.
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
                ReportManager.logDiscrete("Could not patch Allure result metadata in " + file.getName() + ": " + e.getMessage());
            }
        }
    }

    /**
     * Normalizes one Allure result/container JSON document for Awesome report rendering.
     *
     * <p>Passing steps can omit {@code statusDetails.message}, which the Awesome renderer reads
     * unconditionally. TestNG can also emit {@code package} as the full class name, so SHAFT
     * narrows it back to the Java package before applying the default {@code package,testClass}
     * hierarchy.
     *
     * @param json the raw JSON string of a single result or container file
     * @return patched JSON, or the original JSON when no change was needed
     */
    private static String patchStatusDetailsInJson(String json) {
        try {
            JsonNode parsed = JSON_MAPPER.readTree(json);
            if (!(parsed instanceof ObjectNode root)) {
                return json;
            }

            boolean changed = false;
            changed |= ensureStatusDetailsMessage(root);
            changed |= normalizePackageAndClassLabels(root);

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
            ReportManager.logDiscrete("Could not normalize Allure JSON metadata: " + e.getMessage());
            return json;
        }
    }

    private static boolean normalizePackageAndClassLabels(ObjectNode root) {
        JsonNode labelsNode = root.get("labels");
        if (!(labelsNode instanceof ArrayNode labels)) {
            return false;
        }

        String testClass = labelValue(labels, "testClass");
        if (testClass.isBlank()) {
            testClass = classNameFromFullName(root.path("fullName").asText(""));
        }
        if (testClass.isBlank()) {
            return false;
        }

        boolean changed = false;
        if (findLabel(labels, "testClass") == null) {
            addLabel(labels, "testClass", testClass);
            changed = true;
        }

        String packageName = packageNameFromClassName(testClass);
        if (packageName.isBlank()) {
            return changed;
        }

        ObjectNode packageLabel = findLabel(labels, "package");
        if (packageLabel == null) {
            addLabel(labels, "package", packageName);
            return true;
        }

        String currentPackage = packageLabel.path("value").asText("");
        if (currentPackage.isBlank()
                || currentPackage.equals(testClass)
                || currentPackage.equals(root.path("fullName").asText(""))) {
            packageLabel.put("value", packageName);
            changed = true;
        }
        return changed;
    }

    private static ObjectNode findLabel(ArrayNode labels, String name) {
        for (JsonNode label : labels) {
            if (label instanceof ObjectNode labelObject && name.equals(labelObject.path("name").asText())) {
                return labelObject;
            }
        }
        return null;
    }

    private static String labelValue(ArrayNode labels, String name) {
        ObjectNode label = findLabel(labels, name);
        return label == null ? "" : label.path("value").asText("");
    }

    private static void addLabel(ArrayNode labels, String name, String value) {
        ObjectNode label = labels.addObject();
        label.put("name", name);
        label.put("value", value);
    }

    private static String classNameFromFullName(String fullName) {
        int methodSeparator = fullName.lastIndexOf('.');
        return methodSeparator > 0 ? fullName.substring(0, methodSeparator) : "";
    }

    private static String packageNameFromClassName(String className) {
        int classSeparator = className.lastIndexOf('.');
        return classSeparator > 0 ? className.substring(0, classSeparator) : "";
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

        if (cachedIsAllure2) {
            // Allure 2: generate a single-file report so opening the produced HTML works offline.
            return prefix + " generate \"" + resultsPath + "\" --single-file --clean -o \"" + allureOutPutDirectory + "\"";
        }

        // Allure 3: pass --config explicitly so allurerc.yaml is always applied
        String configPath = allureConfigPath().toString();
        return prefix + " generate --config \"" + configPath + "\" \""
                + resultsPath + "\" -o \"" + allureOutPutDirectory + "\"";
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Batteries-included Allure 3 CLI bootstrap
    // ═══════════════════════════════════════════════════════════════════════════

    /**
     * Resolves the command prefix used to invoke the Allure CLI.
     *
     * <p>The result is computed once and cached for the JVM lifetime.
     *
     * <p>Resolution order:
     * <ol>
     *   <li>{@code allure} binary on {@code PATH} — version is always read first:
     *     <ul>
     *       <li>If the version starts with {@code 2.}, Allure 2 compatibility mode is activated
     *           ({@link #cachedIsAllure2} is set to {@code true}) and {@code "allure"} is returned.</li>
     *       <li>If not {@code forceConfiguredCliVersion}, the 3.x (or unknown) binary is used directly.</li>
     *       <li>If {@code forceConfiguredCliVersion=true}, the version must exactly match
     *           {@code allure3Version}; otherwise the binary is ignored and resolution falls through.</li>
     *     </ul>
     *   </li>
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

        boolean enforceConfiguredCliVersion = SHAFT.Properties.allure.forceConfiguredCliVersion();

        // Enforced mode intentionally bypasses any system allure detection (including Allure 2).
        if (enforceConfiguredCliVersion) {
            if (isExecutableOnPath("npx")) {
                cachedAllureCommandPrefix = "npx --yes allure@" + allure3Version;
                ReportManager.logDiscrete("Allure 3 CLI resolved: using configured npx allure@" + allure3Version + ".");
                return cachedAllureCommandPrefix;
            }
            ReportManager.logDiscrete("Node.js not found on PATH. Downloading portable Node.js v" + nodeLtsVersion + " to bootstrap Allure 3 CLI...");
            String downloadedNpxPath = downloadNodeJsPortable();
            if (downloadedNpxPath != null) {
                cachedAllureCommandPrefix = q(downloadedNpxPath) + " --yes allure@" + allure3Version;
                ReportManager.logDiscrete("Allure 3 CLI resolved: using downloaded Node.js npx.");
                return cachedAllureCommandPrefix;
            }
            cachedAllureCommandPrefix = "";
            return null;
        }

        // Legacy mode: prefer system allure and allow Allure 2 compatibility when detected.
        boolean allureOnPath = isExecutableOnPath("allure");
        if (allureOnPath) {
            String systemAllureVersion = readSystemAllureVersion();

            if (systemAllureVersion != null && systemAllureVersion.startsWith("2.")) {
                cachedIsAllure2 = true;
                cachedAllureCommandPrefix = "allure";
                ReportManager.logDiscrete(
                        String.format("Allure 2 CLI detected on PATH (version %s). Activating Allure 2 compatibility mode."
                                + " Features specific to Allure 3 (allurerc.yaml, real-time monitoring) are disabled.",
                                systemAllureVersion));
                return cachedAllureCommandPrefix;
            }

            cachedAllureCommandPrefix = "allure";
            ReportManager.logDiscrete("Allure 3 CLI resolved: using system 'allure' binary."
                    + " Set allure.forceConfiguredCliVersion=true to force configured npx allure@" + allure3Version + ".");
            return cachedAllureCommandPrefix;
        }

        if (isExecutableOnPath("npx")) {
            cachedAllureCommandPrefix = "npx --yes allure@" + allure3Version;
            ReportManager.logDiscrete("Allure 3 CLI resolved: using system npx.");
            return cachedAllureCommandPrefix;
        }

        ReportManager.logDiscrete("Node.js not found on PATH. Downloading portable Node.js v" + nodeLtsVersion + " to bootstrap Allure 3 CLI...");
        String downloadedNpxPath = downloadNodeJsPortable();
        if (downloadedNpxPath != null) {
            cachedAllureCommandPrefix = q(downloadedNpxPath) + " --yes allure@" + allure3Version;
            ReportManager.logDiscrete("Allure 3 CLI resolved: using downloaded Node.js npx.");
            return cachedAllureCommandPrefix;
        }

        cachedAllureCommandPrefix = "";
        return null;
    }

    /**
     * Reads the version of the system {@code allure} binary from {@code allure --version}.
     *
     * @return normalized SemVer-like version extracted from command output, or {@code null} when unavailable
     */
    private static String readSystemAllureVersion() {
        try {
            ProcessBuilder pb = SystemUtils.IS_OS_WINDOWS
                    ? new ProcessBuilder("cmd.exe", "/c", "allure", "--version")
                    : new ProcessBuilder("allure", "--version");
            pb.redirectErrorStream(true);
            Process process = pb.start();
            byte[] outputBytes = process.getInputStream().readAllBytes();
            int exitCode = process.waitFor();
            String output = new String(outputBytes, StandardCharsets.UTF_8).trim();
            return parseAllureVersionCommandOutput(output, exitCode);
        } catch (IOException | InterruptedException e) {
            if (e instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
        }
        return null;
    }

    /**
     * Extracts an Allure version from {@code allure --version} output.
     *
     * <p>Some system wrappers print a valid version token while still returning a non-zero
     * exit code. In that case we still trust the parsed SemVer token to avoid false negatives.
     *
     * @param output   merged stdout/stderr output from the command
     * @param exitCode process exit code
     * @return extracted SemVer-like token, or {@code null} when unavailable
     */
    private static String parseAllureVersionCommandOutput(String output, int exitCode) {
        String parsedVersion = extractSemVerFromText(output);
        if (parsedVersion == null && exitCode != 0) {
            return null;
        }
        return parsedVersion;
    }

    /**
     * Extracts the first SemVer-like token from arbitrary text.
     *
     * @param text raw text that may contain a version token
     * @return extracted SemVer-like token, or {@code null} when no token is found
     */
    private static String extractSemVerFromText(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        Matcher matcher = SEMVER_IN_TEXT_PATTERN.matcher(text);
        if (matcher.find()) {
            return matcher.group(1);
        }
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
            ReportManager.logDiscrete("Could not download portable Node.js from " + downloadUrl);
            return null;
        }

        // Verify the archive checksum before extracting to guard against download corruption
        // or supply-chain tampering.
        if (!verifyNodeJsChecksum(archivePath, downloadUrl, archiveName)) {
            ReportManager.logDiscrete("Node.js archive integrity check failed. Deleting corrupt download: " + archivePath);
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
                ReportManager.logDiscrete("Node.js integrity check failed for '" + archiveName
                        + "'. Expected SHA-256: " + expectedHash + ", computed: " + actualHash);
                return false;
            }
            return true;
        } catch (Exception e) {
            ReportManager.logDiscrete("Could not verify Node.js archive integrity: " + e.getMessage()
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
        String archivePath = resolveExecutionPath("generatedReport_" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")) + ".zip").toString();
        internalFileSession.zipFiles(allureOutPutDirectory, archivePath);
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
