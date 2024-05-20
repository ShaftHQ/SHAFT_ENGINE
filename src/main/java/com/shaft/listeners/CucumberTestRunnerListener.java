package com.shaft.listeners;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.cucumber.core.feature.FeatureParser;
import io.cucumber.core.gherkin.Feature;
import io.cucumber.core.resource.Resource;
import io.cucumber.plugin.event.*;
import io.qameta.allure.cucumber7jvm.AllureCucumber7Jvm;
import org.testng.Reporter;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;
import java.util.UUID;

@SuppressWarnings("unused")
public class CucumberTestRunnerListener extends AllureCucumber7Jvm {
    private static String lastStartedScenarioName;

    @Override
    public void setEventPublisher(EventPublisher publisher) {
        publisher.registerHandlerFor(TestSourceParsed.class, this::handleTestSourceParsed);
        publisher.registerHandlerFor(TestCaseStarted.class, this::caseStartedHandler);
        publisher.registerHandlerFor(TestCaseFinished.class, this::caseFinishedHandler);
        publisher.registerHandlerFor(TestStepStarted.class, this::stepStartedHandler);
        super.setEventPublisher(publisher);
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
        lastStartedScenarioName = testCase.getName();
        ReportManagerHelper.logScenarioInformation(testCase.getKeyword(), lastStartedScenarioName, cleanScenarioSteps.toString());
    }

    private void caseFinishedHandler(TestCaseFinished event) {
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            if (SHAFT.Properties.visuals.videoParamsScope().equals("TestMethod")) {
                RecordManager.attachVideoRecording();
            }
            AnimatedGifManager.attachAnimatedGif();
            ReportManagerHelper.attachTestLog(lastStartedScenarioName,
                    TestNGListenerHelper.createTestLog(Reporter.getOutput()));
        }
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
                return new ByteArrayInputStream(FileActions.getInstance(true).readFile(uri.getPath()).getBytes());
            }
        });
    }

    private void stepStartedHandler(TestStepStarted event) {
        var testStep = event.getTestStep();

        if (testStep instanceof HookTestStep hookTestStep) {
            ReportManager.logDiscrete("Scenario Hook: " + hookTestStep.getHookType().name());
        }

        if (testStep instanceof PickleStepTestStep pickleStepTestStep) {
            ReportManager.logDiscrete("Scenario Step: " + pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText());
        }
    }
}
