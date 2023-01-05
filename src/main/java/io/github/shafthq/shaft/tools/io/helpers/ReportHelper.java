package io.github.shafthq.shaft.tools.io.helpers;

import com.shaft.cli.FileActions;
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
        System.setProperty("disableLogging", "true");
        String importantLinks = """
                <ul>
                    <li>👨‍💻️ <a href="https://github.com/ShaftHQ/SHAFT_ENGINE" target=”_blank”>GitHub - Home</a></li>
                    <li>👤 <a href="https://shafthq.github.io/SHAFT_Engine_Docusaurus/" target=”_blank”>User Guide</a></li>
                    <li>⚙️ <a href="https://shafthq.github.io/SHAFT_ENGINE/" target=”_blank”>Configuration Manager</a></li>
                    <li>📚 <a href="https://shafthq.github.io/SHAFT_ENGINE/apidocs/index.html" target=”_blank”>Javadocs</a></li>
                </ul>""";

        ReportManagerHelper.attach("HTML", "Important Links", importantLinks);
        System.setProperty("disableLogging", "false");
    }

    public static void attachPropertyFiles() {
        ReportManager.logDiscrete("Initializing Properties...");
        System.setProperty("disableLogging", "true");
        if (FileActions.getInstance().doesFileExist(System.getProperty("propertiesFolderPath"))) {
            List<List<Object>> attachments = new ArrayList<>();

            var propertyFiles = Arrays.asList(FileActions.getInstance().listFilesInDirectory(System.getProperty("propertiesFolderPath"), null).split(System.lineSeparator()));
            propertyFiles.forEach(file -> attachments.add(Arrays.asList("Properties", file.replace(".properties", ""), FileActions.getInstance().readFile(System.getProperty("propertiesFolderPath") + File.separator + file))));

            ReportManagerHelper.logNestedSteps("Property Files", attachments);
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