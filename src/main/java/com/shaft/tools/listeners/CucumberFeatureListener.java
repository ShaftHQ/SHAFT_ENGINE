package com.shaft.tools.listeners;

import com.shaft.cli.FileActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.PropertiesFileManager;
import com.shaft.tools.io.ReportManager;
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
        PropertiesFileManager.readPropertyFiles();
    }

    private void handleTestRunFinished(TestRunFinished event) {
        ElementActions.switchToDefaultContent();
    }

    private void handleTestSourceParsed(TestSourceParsed event) {
        event.getNodes().forEach(node -> {
            Optional<Feature> feature = getFeature(event.getUri());
            if (feature.isPresent()) {
                if (ReportManager.getTotalNumberOfTests() == 0) {
                    ReportManager.setTotalNumberOfTests(feature.get().getPickles().size());
                } else {
                    ReportManager.setTotalNumberOfTests(ReportManager.getTotalNumberOfTests() + feature.get().getPickles().size());
                }
            }
        });
        PropertiesFileManager.readPropertyFiles();
    }

    private void handleTestCaseStarted(TestCaseStarted event) {
        TestCase testCase = event.getTestCase();
        StringBuilder scenarioSteps = new StringBuilder();
        String lineSeparator = "<br>";
        testCase.getTestSteps().forEach(testStep -> {
            if (testStep instanceof HookTestStep) {
                scenarioSteps.append(lineSeparator).append(((HookTestStep) testStep).getHookType().name());
            }

            if (testStep instanceof PickleStepTestStep) {
                PickleStepTestStep pickleStepTestStep = (PickleStepTestStep) testStep;
                scenarioSteps.append(lineSeparator)
                        .append("<b style=\"color:ForestGreen;\">")
                        .append(pickleStepTestStep.getStep().getKeyword())
                        .append("</b>")
                        .append(pickleStepTestStep.getStep().getText());
            }
        });
        Optional<Feature> feature = getFeature(testCase.getUri());
        if (feature.isPresent() && feature.get().getName().isPresent()) {
            ReportManager.setFeatureName(feature.get().getName().get());
        }
        ReportManager.setTestCaseName(testCase.getName());
        ReportManager.setTestCaseDescription(scenarioSteps.toString());
        ReportManager.logScenarioInformation(testCase.getKeyword(), testCase.getName(), scenarioSteps.toString());
    }

    private Optional<Feature> getFeature(URI uri) {
        FeatureParser featureParser = new FeatureParser(() -> new UUID(10, 1));
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
        TestStep testStep = event.getTestStep();

        if (testStep instanceof HookTestStep) {
            HookTestStep hookTestStep = (HookTestStep) testStep;
            ReportManager.logDiscrete("Scenario Hook: " + hookTestStep.getHookType().name());
        }

        if (testStep instanceof PickleStepTestStep) {
            PickleStepTestStep pickleStepTestStep = (PickleStepTestStep) testStep;
            ReportManager.logDiscrete("Scenario Step: " + pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText());
        }
    }
}
