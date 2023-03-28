package com.shaft.listeners.internal;

import com.shaft.driver.DriverFactory;
import com.shaft.internal.gui.image.ImageProcessingActions;
import com.shaft.internal.properties.PropertiesHelper;
import com.shaft.internal.tools.io.CheckpointCounter;
import com.shaft.internal.tools.io.ProjectStructureManager;
import com.shaft.internal.tools.io.ReportHelper;
import com.shaft.internal.tools.io.ReportManagerHelper;
import com.shaft.internal.tools.security.GoogleTink;
import org.testng.Reporter;

public class CucumberHelper {

    public static void shaftSetup() {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            System.setProperty("disableLogging", "true");
            PropertiesHelper.initialize();
            ProjectStructureManager.initialize();
            TestNGListenerHelper.configureJVMProxy();
            GoogleTink.initialize();
            GoogleTink.decrypt();
            System.setProperty("disableLogging", "false");

            ReportManagerHelper.logEngineVersion();
            ImageProcessingActions.loadOpenCV();

            ReportManagerHelper.initializeAllureReportingEnvironment();
            ReportManagerHelper.initializeExtentReportingEnvironment();

            ReportHelper.attachImportantLinks();
            ReportHelper.attachPropertyFiles();

            ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
            ReportManagerHelper.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
        }
    }

    public static void shaftTeardown() {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            DriverFactory.closeAllDrivers();

            ReportHelper.attachEngineLog();
            ReportHelper.attachExtentReport();
            ReportHelper.attachCucumberReport();
            CheckpointCounter.attach();
            ReportHelper.attachIssuesLog();

            ReportManagerHelper.setDiscreteLogging(true);
            JiraHelper.reportExecutionStatusToJira();
            GoogleTink.encrypt();
            ReportManagerHelper.generateAllureReportArchive();
            ReportManagerHelper.openAllureReportAfterExecution();
            ReportManagerHelper.logEngineClosure();
        }
    }
}
