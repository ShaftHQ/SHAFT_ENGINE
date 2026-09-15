package com.shaft.listeners;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.listeners.internal.CucumberHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.cucumber.core.feature.FeatureParser;
import io.cucumber.core.resource.Resource;
import io.cucumber.messages.types.Feature;
import io.cucumber.messages.types.Scenario;
import io.cucumber.plugin.event.*;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.cucumber7jvm.AllureCucumber7Jvm;
import io.qameta.allure.cucumber7jvm.testsourcemodel.TestSourcesModelProxy;
import lombok.Getter;
import org.testng.Reporter;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;
import java.util.UUID;

import static com.shaft.listeners.internal.CucumberHelper.recordScenarioResult;
import static com.shaft.listeners.internal.CucumberHelper.shaftTearDown;

/**
 * SHAFT Cucumber plugin: delegates Allure 3 reporting to {@link AllureCucumber7Jvm}
 * and overlays SHAFT-specific scenario bookkeeping, attachments, and teardown.
 */
@SuppressWarnings({
        "ClassDataAbstractionCoupling",
        "ClassFanOutComplexity",
        "PMD.ExcessiveImports",
})
public class CucumberFeatureListener extends AllureCucumber7Jvm {

    /** The name of the most recently started Cucumber scenario, used for report labelling. */
    @Getter
    private static String lastStartedScenarioName;
    /** {@code true} if the last finished step passed; {@code false} otherwise. */
    @Getter
    private static Boolean isLastFinishedStepOK;

    private final TestSourcesModelProxy testSources = new TestSourcesModelProxy();

    /**
     * Creates a feature listener using the default Allure lifecycle singleton.
     */
    @SuppressWarnings("unused")
    public CucumberFeatureListener() {
        this(Allure.getLifecycle());
    }

    /**
     * Creates a feature listener using the provided Allure lifecycle.
     *
     * @param lifecycle the Allure lifecycle instance that receives Cucumber events
     */
    public CucumberFeatureListener(final AllureLifecycle lifecycle) {
        super(lifecycle);
        CucumberHelper.engineSetup();
    }

    @Override
    public void setEventPublisher(final EventPublisher publisher) {
        super.setEventPublisher(publisher);
        publisher.registerHandlerFor(TestSourceRead.class, this::handleFeatureStartedHandler);
        publisher.registerHandlerFor(TestSourceParsed.class, this::handleTestSourceParsed);
        publisher.registerHandlerFor(TestCaseStarted.class, this::handleShaftTestCaseStarted);
        publisher.registerHandlerFor(TestCaseFinished.class, this::handleShaftTestCaseFinished);
        publisher.registerHandlerFor(TestStepFinished.class, this::handleShaftTestStepFinished);
        publisher.registerHandlerFor(TestRunFinished.class, this::handleFeatureFinishedHandler);
    }

    private void handleFeatureStartedHandler(final TestSourceRead event) {
        testSources.addTestSourceReadEvent(event.getUri(), event);
    }

    @SuppressWarnings("unused")
    private void handleFeatureFinishedHandler(final TestRunFinished event) {
        shaftTearDown();
    }

    private void handleShaftTestCaseStarted(final TestCaseStarted event) {
        final Feature feature = testSources.getFeature(event.getTestCase().getUri());
        if (feature == null) {
            return;
        }
        final Scenario scenarioDefinition = testSources.getScenarioDefinition(
                event.getTestCase().getUri(),
                event.getTestCase().getLocation().getLine()
        );
        ReportManagerHelper.setFeatureName(feature.getName());
        lastStartedScenarioName = scenarioDefinition != null ? scenarioDefinition.getName() : event.getTestCase().getName();
        ReportManagerHelper.setTestCaseName(lastStartedScenarioName);
        if (scenarioDefinition != null) {
            ReportManagerHelper.setTestCaseDescription(scenarioDefinition.getDescription());
        }
        var cleanScenarioSteps = new StringBuilder();
        event.getTestCase().getTestSteps().forEach(testStep -> {
            if (testStep instanceof PickleStepTestStep pickleStepTestStep) {
                cleanScenarioSteps.append(pickleStepTestStep.getStep().getKeyword())
                        .append(pickleStepTestStep.getStep().getText())
                        .append(System.lineSeparator());
            }
        });
        String keyword = scenarioDefinition != null ? scenarioDefinition.getKeyword() : "Scenario";
        ReportManagerHelper.logScenarioInformation(keyword, lastStartedScenarioName, cleanScenarioSteps.toString());
    }

    private void handleShaftTestCaseFinished(final TestCaseFinished event) {
        String scenarioId = event.getTestCase().getUri().toString() + ":" + event.getTestCase().getLine();
        recordScenarioResult(event.getResult().getStatus(), scenarioId);
        if (Reporter.getCurrentTestResult() == null) {
            if (SHAFT.Properties.visuals.videoParamsScope().equals("TestMethod")) {
                RecordManager.attachVideoRecording();
            }
            AnimatedGifManager.attachAnimatedGif();
            ReportManagerHelper.attachTestLog(lastStartedScenarioName,
                    TestNGListenerHelper.createTestLog(Reporter.getOutput()));
        } else {
            ReportManagerHelper.attachTestLog(lastStartedScenarioName,
                    TestNGListenerHelper.createTestLog(Reporter.getOutput()));
        }
    }

    private void handleShaftTestStepFinished(final TestStepFinished event) {
        isLastFinishedStepOK = event.getResult().getStatus().isOk();
    }

    private void handleTestSourceParsed(TestSourceParsed event) {
        event.getNodes().forEach(node -> {
            Optional<io.cucumber.core.gherkin.Feature> feature = getFeature(event.getUri());
            if (feature.isPresent()) {
                if (ReportManagerHelper.getTotalNumberOfTests() == 0) {
                    ReportManagerHelper.setTotalNumberOfTests(feature.get().getPickles().size());
                } else {
                    ReportManagerHelper.setTotalNumberOfTests(ReportManagerHelper.getTotalNumberOfTests() + feature.get().getPickles().size());
                }
            }
        });
    }

    private Optional<io.cucumber.core.gherkin.Feature> getFeature(URI uri) {
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
}
