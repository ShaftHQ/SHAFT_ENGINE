package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

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
        disableLogging();
        String importantLinks = """
                <ul>
                    <li>👨‍💻️ <a href="https://github.com/ShaftHQ/SHAFT_ENGINE" target=”_blank”>GitHub - Home</a></li>
                    <li>👤 <a href="https://shafthq.github.io/SHAFT_Engine_Docusaurus/" target=”_blank”>User Guide</a></li>
                    <li>⚙️ <a href="https://shafthq.github.io/SHAFT_ENGINE/" target=”_blank”>Configuration Manager</a></li>
                    <li>📚 <a href="https://shafthq.github.io/SHAFT_ENGINE/apidocs/index.html" target=”_blank”>Javadocs</a></li>
                </ul>""";

        ReportManagerHelper.attach("HTML", "Important Links", importantLinks);
        enableLogging();
    }

    public static void enableLogging() {
        SHAFT.Properties.reporting.set().disableLogging(false);
    }

    public static void disableLogging() {
        SHAFT.Properties.reporting.set().disableLogging(true);
    }

    public static void attachPropertyFiles() {
        ReportManager.logDiscrete("Initializing Properties...");
        disableLogging();
        if (FileActions.getInstance(true).doesFileExist(SHAFT.Properties.paths.properties())) {
            List<List<Object>> attachments = new ArrayList<>();

            var propertyFiles = Arrays.asList(FileActions.getInstance(true).listFilesInDirectory(SHAFT.Properties.paths.properties(), null).replaceAll("default" + System.lineSeparator(), "").replaceAll(".*json", "").trim().split(System.lineSeparator()));
            propertyFiles.forEach(file -> attachments.add(Arrays.asList("Properties", file.replace(".properties", ""), FileActions.getInstance(true).readFile(SHAFT.Properties.paths.properties() + File.separator + file))));

            var jsonFiles = Arrays.asList(FileActions.getInstance(true).listFilesInDirectory(SHAFT.Properties.paths.properties(), null).replaceAll("default" + System.lineSeparator(), "").replaceAll(".*properties", "").trim().split(System.lineSeparator()));
            jsonFiles.forEach(file -> attachments.add(Arrays.asList("JSON", file.replace(".json", ""), FileActions.getInstance(true).readFile(SHAFT.Properties.paths.properties() + File.separator + file))));

            ReportManagerHelper.logNestedSteps("Property Files", attachments);
        }
        enableLogging();
    }

    public static void attachCucumberReport() {
        if (FileActions.getInstance(true).doesFileExist("allure-results/cucumberReport.html")) {
            ReportManagerHelper.attach("HTML", "Cucumber Execution Report", FileActions.getInstance(true).readFile("allure-results/cucumberReport.html"));
        }
    }

    public static void attachExtentReport() {
        ReportManagerHelper.extentReportsFlush();
        if (SHAFT.Properties.reporting.attachExtentReportsToAllureReport()) {
            if (SHAFT.Properties.reporting.generateExtentReports() && FileActions.getInstance(true).doesFileExist(ReportManagerHelper.getExtentReportFileName())) {
                ReportManagerHelper.attach("HTML", "Extent Emailable Execution Report", FileActions.getInstance(true).readFile(ReportManagerHelper.getExtentReportFileName()));
            }
        }
    }
}