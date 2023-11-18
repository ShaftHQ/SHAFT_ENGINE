package com.shaft.listeners.internal;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.ProjectStructureManager;
import com.shaft.tools.io.internal.ReportHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.testng.Reporter;
import org.testng.xml.XmlSuite;

import java.util.List;

public class CucumberHelper {

    public static void configureCucumberProperties(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            // override test suite parameters adding cucumber options
            var params = suite.getParameters();
            params.put("cucumber.ansi-colors.disabled", String.valueOf(SHAFT.Properties.cucumber.cucumberAnsiColorsDisabled()));
            params.put("cucumber.execution.dry-run", String.valueOf(SHAFT.Properties.cucumber.cucumberExecutionDryRun()));
            params.put("cucumber.execution.limit", SHAFT.Properties.cucumber.cucumberExecutionLimit());
            params.put("cucumber.execution.order", SHAFT.Properties.cucumber.cucumberExecutionOrder());
            params.put("cucumber.execution.wip", String.valueOf(SHAFT.Properties.cucumber.cucumberExecutionWip()));
            params.put("cucumber.features", SHAFT.Properties.cucumber.cucumberFeatures());
            params.put("cucumber.filter.name", SHAFT.Properties.cucumber.cucumberFilterName());
            params.put("cucumber.filter.tags", SHAFT.Properties.cucumber.cucumberFilterTags());
            params.put("cucumber.glue", SHAFT.Properties.cucumber.cucumberGlue());
            params.put("cucumber.plugin", SHAFT.Properties.cucumber.cucumberPlugin());
            params.put("cucumber.object-factory", SHAFT.Properties.cucumber.cucumberObjectFactory());
            params.put("cucumber.snippet-type", SHAFT.Properties.cucumber.cucumberSnippetType());
            suite.setParameters(params);
        });
    }

    public static void shaftSetup() {
        PropertiesHelper.initialize();
        SHAFT.Properties.reporting.set().disableLogging(true);
        ProjectStructureManager.initialize(ProjectStructureManager.RunType.CUCUMBER);
        TestNGListenerHelper.configureJVMProxy();
        GoogleTink.initialize();
        GoogleTink.decrypt();
        SHAFT.Properties.reporting.set().disableLogging(false);

        ReportManagerHelper.logEngineVersion();
        UpdateChecker.check();
        ImageProcessingActions.loadOpenCV();

        ReportManagerHelper.initializeAllureReportingEnvironment();
        ReportManagerHelper.initializeExtentReportingEnvironment();

        ReportHelper.attachImportantLinks();
        ReportHelper.attachPropertyFiles();

        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
        ReportManagerHelper.setDebugMode(SHAFT.Properties.reporting.debugMode());
        //set cucumber options
        System.setProperty("cucumber.options",
                " --dry-run " + SHAFT.Properties.cucumber.cucumberExecutionDryRun() +
                        " --features " + SHAFT.Properties.cucumber.cucumberFeatures() +
                        " --name " + SHAFT.Properties.cucumber.cucumberFilterName() +
                        " --tags " + SHAFT.Properties.cucumber.cucumberFilterTags() +
                        " --glue " + SHAFT.Properties.cucumber.cucumberGlue() +
                        " --plugin " + SHAFT.Properties.cucumber.cucumberPlugin() +
                        " --quiet " + SHAFT.Properties.cucumber.cucumberPublishQuiet() +
                        " --publish " + !SHAFT.Properties.cucumber.cucumberPublishQuiet()
        );
    }

    public static void shaftTeardown() {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            if (SHAFT.Properties.flags.autoCloseDriverInstance())
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
