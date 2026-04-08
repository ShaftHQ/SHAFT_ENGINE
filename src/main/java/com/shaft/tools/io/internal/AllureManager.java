package com.shaft.tools.io.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

/**
 * Internal utility class that manages the Allure reporting lifecycle for SHAFT test runs.
 * Responsibilities include initialising the results directory, generating an {@code allurerc.yaml}
 * configuration file, invoking the Allure 3 CLI (npm-based {@code allure} command) to produce
 * the single-file HTML report, and optionally producing a ZIP archive.
 *
 * <p>This class is not intended for direct use in test code. It is invoked automatically by the
 * SHAFT framework listeners at suite start and finish.
 *
 * <p>Thread safety: all public methods are {@code static} and intended to be called from a
 * single thread (the test runner thread). The result directory fields are mutable class-level
 * state and should not be accessed concurrently.
 */
public class AllureManager {
    private static final String allureReportPath = "allure-report";
    private static final String allureConfigFileName = "allurerc.yaml";
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
     * <p>Allure 3 CLI must be installed globally via npm ({@code npm install -g allure}) or available
     * as {@code npx allure} before report generation is attempted.
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
        openAllureReport(newFileName);
    }

    private static String renameAllureReport() {
        String newFileName = "AllureReport.html";
        if (SHAFT.Properties.allure.accumulateReports())
            newFileName = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")) + "_AllureReport.html";
        String sourceFile = System.getProperty("user.dir") + File.separator + allureReportPath + File.separator + "index.html";
        if (!internalFileSession.doesFileExist(sourceFile)) {
            // Allure report was not generated (CLI not installed or generation failed); skip renaming
            ReportManager.logDiscrete("Allure report 'index.html' not found. Ensure the Allure 3 CLI is installed via 'npm install -g allure'.");
            return newFileName;
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

    private static void writeGenerateReportShellFilesToProjectDirectory() {
        String resultsPath = allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1);
        // create generate_allure_report.sh or generate_allure_report.bat
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            // create windows batch file
            commandsToServeAllureReport = Arrays.asList("@echo off",
                    "allure serve \"" + resultsPath + "\" -h localhost",
                    "pause", "exit");
            internalFileSession.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            // create Unix-based sh file
            commandsToServeAllureReport = Arrays.asList("#!/bin/bash",
                    "allure serve '" + resultsPath + "' -h localhost",
                    "exit");
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

        // Generate the report using Allure 3 CLI (npm-based `allure` command)
        internalTerminalSession.performTerminalCommand(getCommandToCreateAllureReport());
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
        String resultsPath = allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1);
        if (SystemUtils.IS_OS_WINDOWS) {
            return "allure generate \"" + resultsPath + "\" -o \"" + allureOutPutDirectory + "\" --clean";
        } else {
            return "allure generate " + resultsPath + " -o " + allureOutPutDirectory + " --clean";
        }
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
