package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.tools.security.GoogleTink;
import org.testng.ITestContext;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.BeforeSuite;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

public class LogsHelper {
    public static void attachFullLogs() {
        String executionEndTimestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
        ReportManagerHelper.attachIssuesLog(executionEndTimestamp);
        ReportManagerHelper.attachFullLog(executionEndTimestamp);
    }

    public static void closeAllDriversAndattachBrowserLogs() {
//        if (Boolean.FALSE.equals(DriverFactoryHelper.isDriversListEmpty())) {
            DriverFactory.closeAllDrivers();
//        }
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

    //TODO: migrate invokedMethodListener, SuiteListener to annotations here?
    @BeforeSuite
    public void setupActivities(ITestContext testContext) {
        ReportManagerHelper.initializeAllureReportingEnvironment();
        ReportManagerHelper.initializeExtentReportingEnvironment();
        attachImportantLinks();
        attachPropertyFiles();
//        ReportManagerHelper.generateJDKShellFilesToProjectDirectory();
        var suite = testContext.getSuite();
        if (!(suite.getAllMethods().size() == 1 && suite.getAllMethods().get(0).getMethodName().equals("runScenario"))) {
            // not cucumber test runner
            ReportManagerHelper.setTotalNumberOfTests(suite.getAllMethods().size());
        }
        ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
        ReportManagerHelper.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));

    }

    @AfterSuite
    public void teardownActivities() {
        closeAllDriversAndattachBrowserLogs();
        attachFullLogs();
        attachCucumberReport();
        attachExtentReport();
        ReportManagerHelper.setDiscreteLogging(true);
        GoogleTink.encrypt();
        ReportManagerHelper.generateAllureReportArchive();
        ReportManagerHelper.openAllureReportAfterExecution();
    }

}