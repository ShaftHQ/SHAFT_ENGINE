package com.shaft.tools.io.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.net.URL;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

public class AllureManager {
    private static final String allureExtractionLocation = System.getProperty("user.home") + File.separator + ".m2"
            + File.separator + "repository" + File.separator + "allure" + File.separator;
    private static final String allureReportPath = "allure-report";
    private static String allureResultsFolderPath = "";
    private static String allureBinaryPath = "";
    private static String allureOutPutDirectory = "";

    private static final TerminalActions internalTerminalSession = TerminalActions.getInstance(false,false,true);
    private static final FileActions internalFileSession = FileActions.getInstance(true);

    public static void initializeAllureReportingEnvironment() {
        ReportManager.logDiscrete("Initializing Allure Reporting Environment...");
        /*
         * Force screenshot link to be shown in the results as a link not text
         */
        System.setProperty("org.uncommons.reportng.escape-output", "false");
        allureResultsFolderPath = SHAFT.Properties.paths.allureResults();
        cleanAllureReportDirectory();
        cleanAllureResultsDirectory();
        downloadAndExtractAllureBinaries();
        overrideAllurePluginConfiguration();
        writeGenerateReportShellFilesToProjectDirectory();
        writeEnvironmentVariablesToAllureResultsDirectory();
    }

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
        internalFileSession.renameFile(System.getProperty("user.dir") + File.separator + allureReportPath + File.separator + "index.html", newFileName);
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

    public static void generateAllureReportArchive() {
        if (Boolean.TRUE.equals(SHAFT.Properties.allure.generateArchive())) {
            ReportManager.logDiscrete("Generating Allure Report Archive...");
            ReportHelper.disableLogging();
            writeAllureReport();
            createAllureReportArchive();
            ReportHelper.enableLogging();
        }
    }

    private static void downloadAndExtractAllureBinaries() {
        // extract allure from jar file to src/main/resources directory if it doesn't
        // already exist
        String allureVersion = SHAFT.Properties.internal.allureVersion();
        allureBinaryPath = allureExtractionLocation + "allure-" + allureVersion + File.separator + "bin" + File.separator + "allure";
        if (!internalFileSession.doesFileExist(allureBinaryPath)) {
            try {
                internalFileSession.deleteFolder(allureExtractionLocation);
            } catch (AssertionError e) {
                ReportManager.logDiscrete("Couldn't clear the allure extraction directory. Kindly terminate any running java process or restart your machine to fix this issue.");
                ReportManagerHelper.logDiscrete(e);
            }
            // download allure binary
            URL allureArchive = internalFileSession.downloadFile(
                    "https://repo.maven.apache.org/maven2/io/qameta/allure/allure-commandline/" + allureVersion
                            + "/allure-commandline-" + allureVersion + ".zip",
                    "target" + File.separator + "allureBinary.zip");
            internalFileSession.unpackArchive(allureArchive, allureExtractionLocation);

            if (!SystemUtils.IS_OS_WINDOWS) {
                // make allure executable on Unix-based shells
                internalTerminalSession.performTerminalCommand("chmod u+x " + allureBinaryPath);
            }
        }
    }

    private static void writeGenerateReportShellFilesToProjectDirectory() {
        String allureVersion = SHAFT.Properties.internal.allureVersion();
        // create generate_allure_report.sh or generate_allure_report.bat
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            // create windows batch file
            commandsToServeAllureReport = Arrays.asList("@echo off",
                    ":: If you already have a valid JAVA_HOME environment variable set, feel free to comment the below two lines",
                    "set JAVA_HOME=" + System.getProperty("java.home"),
                    "set path=%JAVA_HOME%\\bin;%path%",
                    "set path=" + allureExtractionLocation + "allure-" + allureVersion + "\\bin;%path%",
                    "allure serve " + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1) + " -h localhost",
                    "pause", "exit");
            internalFileSession.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            // create Unix-based sh file
            commandsToServeAllureReport = Arrays
                    .asList("#!/bin/bash", "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
                            "cd '" + allureExtractionLocation + "allure-" + allureVersion + "/bin/'",
                            "bash allure serve $parent_path'/"
                                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1) + "'" + " -h localhost",
                            "exit"

                    );
            internalFileSession.writeToFile("", "generate_allure_report.sh", commandsToServeAllureReport);
            // make allure executable on Unix-based shells
            internalTerminalSession.performTerminalCommand("chmod u+x generate_allure_report.sh");
        }
    }

    private static void cleanAllureResultsDirectory() {
        // clean allure-results directory before execution
        if (!SHAFT.Properties.allure.accumulateHistory()) {
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

    private static void overrideAllurePluginConfiguration() {
        String allureVersion = SHAFT.Properties.internal.allureVersion();
        // extract allure from SHAFT_Engine jar
        URL allureSHAFTConfigArchive = ReportManagerHelper.class.getResource("/resources/allure/allureBinary_SHAFTEngineConfigFiles.zip");
        internalFileSession.unpackArchive(allureSHAFTConfigArchive,
                allureExtractionLocation + "allure-" + allureVersion + File.separator);
        // deleting custom-logo.svg to avoid generating extra folder with report in single mode
        internalFileSession.deleteFile(allureExtractionLocation + "allure-" + allureVersion + File.separator + "plugins" + File.separator + "custom-logo-plugin" + File.separator + "static" + File.separator + "custom-logo.svg");

        // edit defaults and input custom logo path
        var styleCssFilePath = allureExtractionLocation + "allure-" + allureVersion + File.separator + "plugins" + File.separator + "custom-logo-plugin" + File.separator + "static" + File.separator + "styles.css";
        var desiredStyle = internalFileSession.readFile(styleCssFilePath)
                .replace("https://github.com/ShaftHQ/SHAFT_ENGINE/blob/main/src/main/resources/images/shaft_white.png?raw=true"
                        , SHAFT.Properties.allure.customLogo());
        internalFileSession.writeToFile(styleCssFilePath, desiredStyle);
    }

    private static void writeAllureReport() {
        String commandToCreateAllureReport;
        allureBinaryPath = allureExtractionLocation + "allure-" + SHAFT.Properties.internal.allureVersion()
                + "/bin/allure";
        allureOutPutDirectory = System.getProperty("user.dir") + File.separator + "target" + File.separator + allureReportPath;
        var customReportName = SHAFT.Properties.allure.customTitle();
        if (SystemUtils.IS_OS_WINDOWS) {
            commandToCreateAllureReport = allureBinaryPath + ".bat" + " generate --single-file --clean '"
                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1)
                    + "' -o '" + allureOutPutDirectory + "' --report-name '"+customReportName+"'";
        } else {
            commandToCreateAllureReport = allureBinaryPath + " generate --single-file --clean "
                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1)
                    + " -o " + allureOutPutDirectory + " --report-name "+customReportName;
        }
        internalFileSession.createFolder(allureOutPutDirectory);
        internalTerminalSession.performTerminalCommand(commandToCreateAllureReport);
    }

    private static void createAllureReportArchive() {
        internalFileSession.zipFiles(allureReportPath + "/", "generatedReport_" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")) + ".zip");
    }

    private static void writeEnvironmentVariablesToAllureResultsDirectory() {
        // reads all environment variables and then formats and writes them to be read
        // by the Allure report
        var props = System.getProperties();
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
