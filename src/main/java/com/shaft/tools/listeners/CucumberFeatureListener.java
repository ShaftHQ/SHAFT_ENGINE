package com.shaft.tools.listeners;

import com.shaft.cli.FileActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.cucumber.core.feature.FeatureParser;
import io.cucumber.core.gherkin.Feature;
import io.cucumber.core.resource.Resource;
import io.cucumber.plugin.ConcurrentEventListener;
import io.cucumber.plugin.event.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;
import java.util.UUID;

public class CucumberFeatureListener implements ConcurrentEventListener {

    @Override
    public void setEventPublisher(EventPublisher publisher) {
        //https://github.com/cucumber/cucumber-jvm/issues/1901
        //https://github.com/cucumber/cucumber-jvm/issues/1901#issuecomment-600494342
        publisher.registerHandlerFor(TestRunStarted.class, this::handleTestRunStarted);
        publisher.registerHandlerFor(TestRunFinished.class, this::handleTestRunFinished);
        publisher.registerHandlerFor(TestCaseStarted.class, this::handleTestCaseStarted);
        publisher.registerHandlerFor(TestStepStarted.class, this::handleTestStepStarted);
        publisher.registerHandlerFor(TestSourceParsed.class, this::handleTestSourceParsed);
    }

    private void handleTestRunStarted(TestRunStarted event) {
        PropertyFileManager.readPropertyFiles();
    }

    private void handleTestRunFinished(TestRunFinished event) {
        ElementActions.switchToDefaultContent();
        if(Boolean.parseBoolean(System.getProperty("generateExtentReports").trim())){
            ReportManagerHelper.extentReportsReset();
        }
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
        PropertyFileManager.readPropertyFiles();
    }

    private void handleTestCaseStarted(TestCaseStarted event) {
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
        if (Boolean.parseBoolean(System.getProperty("generateExtentReports").trim())){
            ReportManagerHelper.extentReportsCreateTest(testCase.getName(), scenarioSteps.toString());
        }
        ReportManagerHelper.logScenarioInformation(testCase.getKeyword(), testCase.getName(), cleanScenarioSteps.toString());
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
                return new ByteArrayInputStream(FileActions.readFromFile(uri.getPath()).getBytes());
            }
        });
    }

    private void handleTestStepStarted(TestStepStarted event) {
    	var testStep = event.getTestStep();

        if (testStep instanceof HookTestStep hookTestStep) {
            ReportManager.logDiscrete("Scenario Hook: " + hookTestStep.getHookType().name());
        }

        if (testStep instanceof PickleStepTestStep pickleStepTestStep) {
            ReportManager.logDiscrete("Scenario Step: " + pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText());
        }
    }
}
