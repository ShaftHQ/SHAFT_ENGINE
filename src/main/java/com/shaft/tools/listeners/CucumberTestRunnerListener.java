package com.shaft.tools.listeners;

import com.shaft.cli.FileActions;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.*;
import com.shaft.tools.security.GoogleTink;
import io.cucumber.core.feature.FeatureParser;
import io.cucumber.core.gherkin.Feature;
import io.cucumber.core.resource.Resource;
import io.cucumber.plugin.ConcurrentEventListener;
import io.cucumber.plugin.event.*;
import org.testng.Reporter;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;
import java.util.UUID;

public class CucumberTestRunnerListener implements ConcurrentEventListener {

    private static String lastStartedScenarioName;

    private static String lastStartedStepName;
    private static Boolean isLastFinishedStepOK;

    public static String getLastStartedStepName() {
        return lastStartedStepName;
    }

    public static Boolean getIsLastFinishedStepOK() {
        return isLastFinishedStepOK;
    }

    @Override
    public void setEventPublisher(EventPublisher publisher) {
        //https://github.com/cucumber/cucumber-jvm/issues/1901
        //https://github.com/cucumber/cucumber-jvm/issues/1901#issuecomment-600494342
        publisher.registerHandlerFor(TestRunStarted.class, this::handleTestRunStarted);
        publisher.registerHandlerFor(TestRunFinished.class, this::handleTestRunFinished);
        publisher.registerHandlerFor(TestCaseStarted.class, this::caseStartedHandler);
        publisher.registerHandlerFor(TestCaseFinished.class, this::caseFinishedHandler);
        publisher.registerHandlerFor(TestStepStarted.class, this::stepStartedHandler);
        publisher.registerHandlerFor(TestStepFinished.class, this::stepFinishedHandler);
        publisher.registerHandlerFor(TestSourceParsed.class, this::handleTestSourceParsed);
    }

    private void handleTestSourceParsed(TestSourceParsed event) {
        event.getNodes().forEach(node -> {
            Optional<Feature> feature = getFeature(event.getUri());
            if (feature.isPresent()) {
                if (ReportManagerHelper.getTotalNumberOfTests() == 0) {
                    ReportManagerHelper.setTotalNumberOfTests(feature.get().getPickles().size());
                } else {
                    ReportManagerHelper.setTotalNumberOfTests(ReportManagerHelper.getTotalNumberOfTests() + feature.get().getPickles().size());
                }
            }
        });
    }

    private void handleTestRunStarted(TestRunStarted event) {
        shaftSetup();
    }

    private void handleTestRunFinished(TestRunFinished event) {
        shaftTeardown();
    }


    private void shaftSetup() {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            System.setProperty("disableLogging", "true");
            PropertyFileManager.readPropertyFiles();
            ProjectStructureManager.initialize();
            GoogleTink.initialize();
            GoogleTink.decrypt();
            System.setProperty("disableLogging", "false");
            ReportManagerHelper.logEngineVersion();
            ImageProcessingActions.loadOpenCV();
            ReportManagerHelper.initializeAllureReportingEnvironment();
            ReportManagerHelper.initializeExtentReportingEnvironment();
            LogsHelper.attachImportantLinks();
            LogsHelper.attachPropertyFiles();
            ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
            ReportManagerHelper.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
        }
    }

    private void shaftTeardown() {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            LogsHelper.closeAllDriversAndattachBrowserLogs();
            LogsHelper.attachFullLogs();
            LogsHelper.attachCucumberReport();
            LogsHelper.attachExtentReport();
            ReportManagerHelper.setDiscreteLogging(true);
            GoogleTink.encrypt();
            ReportManagerHelper.generateAllureReportArchive();
            ReportManagerHelper.openAllureReportAfterExecution();
            AlterSuiteListener.reportExecutionStatusToJira();
        }
    }

    private void caseStartedHandler(TestCaseStarted event) {
        var testCase = event.getTestCase();
        var scenarioSteps = new StringBuilder();
        var cleanScenarioSteps = new StringBuilder();
        testCase.getTestSteps().forEach(testStep -> {
            if (testStep instanceof PickleStepTestStep pickleStepTestStep) {
                scenarioSteps.append("<b style=\"color:ForestGreen;\">")
                        .append(pickleStepTestStep.getStep().getKeyword())
                        .append("</b>")
                        .append(pickleStepTestStep.getStep().getText())
                        .append("<br/>");
                cleanScenarioSteps.append(pickleStepTestStep.getStep().getKeyword())
                        .append(pickleStepTestStep.getStep().getText())
                        .append(System.lineSeparator());
            }
        });
        Optional<Feature> feature = getFeature(testCase.getUri());
        if (feature.isPresent() && feature.get().getName().isPresent()) {
            ReportManagerHelper.setFeatureName(feature.get().getName().get());
        }
        ReportManagerHelper.setTestCaseName(testCase.getName());
        ReportManagerHelper.setTestCaseDescription(scenarioSteps.toString());
        if (Boolean.parseBoolean(System.getProperty("generateExtentReports").trim())) {
            ReportManagerHelper.extentReportsCreateTest(testCase.getName(), scenarioSteps.toString());
        }
        lastStartedScenarioName = testCase.getName();
        ReportManagerHelper.logScenarioInformation(testCase.getKeyword(), lastStartedScenarioName, cleanScenarioSteps.toString());
    }

    private void caseFinishedHandler(TestCaseFinished event) {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            if (System.getProperty("videoParams_scope").trim().equals("TestMethod")) {
                RecordManager.attachVideoRecording();
            }
            ScreenshotManager.attachAnimatedGif();
            // configuration method attachment is not added to the report (Allure ->
            // threadContext.getCurrent(); -> empty)
            ReportManagerHelper.attachTestLog(lastStartedScenarioName,
                    InvokedMethodListener.createTestLog(Reporter.getOutput()));
        }
        // resetting scope and config
//        if (!DriverFactoryHelper.isMobileNativeExecution()) {
//            ElementActions.switchToDefaultContent();
//        }
    }

    private Optional<Feature> getFeature(URI uri) {
        var featureParser = new FeatureParser(() -> new UUID(10, 1));
        return featureParser.parseResource(new Resource() {
            @Override
            public URI getUri() {
                return uri;
            }

            @Override
            public InputStream getInputStream() {
                return new ByteArrayInputStream(FileActions.getInstance().readFromFile(uri.getPath()).getBytes());
            }
        });
    }

    private void stepStartedHandler(TestStepStarted event) {
        var testStep = event.getTestStep();

        if (testStep instanceof HookTestStep hookTestStep) {
            ReportManager.logDiscrete("Scenario Hook: " + hookTestStep.getHookType().name());
            lastStartedStepName = hookTestStep.getHookType().name();
        }

        if (testStep instanceof PickleStepTestStep pickleStepTestStep) {
            ReportManager.logDiscrete("Scenario Step: " + pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText());
            lastStartedStepName = pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText();
        }
    }

    private void stepFinishedHandler(TestStepFinished event) {
        isLastFinishedStepOK = event.getResult().getStatus().isOk();
    }
}
