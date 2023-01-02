package com.shaft.tools.io.helpers;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

public class ReportHelper {
    public static void attachEngineLog() {
        String executionEndTimestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
        ReportManagerHelper.attachEngineLog(executionEndTimestamp);
    }

    public static void attachIssuesLog() {
        String executionEndTimestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
        ReportManagerHelper.attachIssuesLog(executionEndTimestamp);
    }

    public static void attachImportantLinks() {
        ReportManager.logDiscrete("Initializing Important Links...");
        System.setProperty("disableLogging", "true");
        String importantLinks = "#SHAFT: Important Links" +
                System.lineSeparator() +
                "===========================" +
                System.lineSeparator() +
                "#\uD83D\uDC68\u200D\uD83D\uDCBB️ GitHub - Home" +
                System.lineSeparator() +
                "https://github.com/ShaftHQ/SHAFT_ENGINE" +
                System.lineSeparator() +
                "#⚙️ Configuration Manager:" +
                System.lineSeparator() +
                "https://shafthq.github.io/SHAFT_ENGINE/" +
                System.lineSeparator() +
                "#\uD83D\uDC64 User Guide:" +
                System.lineSeparator() +
                "https://shafthq.github.io/SHAFT_Engine_Docusaurus/" +
                System.lineSeparator() +
                "#\uD83D\uDCDA Javadocs" +
                System.lineSeparator() +
                "https://shafthq.github.io/SHAFT_ENGINE/apidocs/index.html";

        ReportManagerHelper.attach("SHAFT Links", "Important", importantLinks);
        System.setProperty("disableLogging", "false");
    }

    public static void attachPropertyFiles() {
        ReportManager.logDiscrete("Initializing Custom Properties...");
        System.setProperty("disableLogging", "true");
        if (FileActions.getInstance().doesFileExist(System.getProperty("propertiesFolderPath"))) {
            var propertyFiles = Arrays.asList(FileActions.getInstance().listFilesInDirectory(System.getProperty("propertiesFolderPath")).split(System.lineSeparator()));
            propertyFiles.forEach(file -> ReportManagerHelper.attach("Properties", file.replace(".properties", ""), FileActions.getInstance().readFile(System.getProperty("propertiesFolderPath") + File.separator + file)));
        }
        System.setProperty("disableLogging", "false");
    }

    public static void attachCucumberReport() {
        if (FileActions.getInstance().doesFileExist("allure-results/cucumberReport.html")) {
            ReportManagerHelper.attach("HTML", "Cucumber Execution Report", FileActions.getInstance().readFile("allure-results/cucumberReport.html"));
        }
    }

    public static void attachExtentReport() {
        ReportManagerHelper.extentReportsFlush();
        if (Boolean.parseBoolean(System.getProperty("generateExtentReports").trim()) && FileActions.getInstance().doesFileExist(ReportManagerHelper.getExtentReportFileName())) {
            ReportManagerHelper.attach("HTML", "Extent Emailable Execution Report", FileActions.getInstance().readFile(ReportManagerHelper.getExtentReportFileName()));
        }
    }
}