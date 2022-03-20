/*
 *  Copyright 2019 Qameta Software OÜ
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.shaft.tools.listeners;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.LogsHelper;
import com.shaft.tools.io.ProjectStructureManager;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.cucumber.core.feature.FeatureParser;
import io.cucumber.core.resource.Resource;
import io.cucumber.messages.types.Examples;
import io.cucumber.messages.types.Feature;
import io.cucumber.messages.types.Scenario;
import io.cucumber.messages.types.TableRow;
import io.cucumber.plugin.ConcurrentEventListener;
import io.cucumber.plugin.event.*;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.cucumber7jvm.testsourcemodel.TestSourcesModelProxy;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.*;
import org.testng.Reporter;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static io.qameta.allure.util.ResultsUtils.*;

/**
 * Allure plugin for Cucumber JVM 7.0.
 */
@SuppressWarnings({
        "ClassDataAbstractionCoupling",
        "ClassFanOutComplexity",
        "PMD.ExcessiveImports",
        "PMD.GodClass",
})
public class CucumberFeatureListener implements ConcurrentEventListener {

    private final AllureLifecycle lifecycle;

    private final ConcurrentHashMap<String, String> scenarioUuids = new ConcurrentHashMap<>();
    private final TestSourcesModelProxy testSources = new TestSourcesModelProxy();

    private final ThreadLocal<Feature> currentFeature = new InheritableThreadLocal<>();
    private final ThreadLocal<URI> currentFeatureFile = new InheritableThreadLocal<>();
    private final ThreadLocal<TestCase> currentTestCase = new InheritableThreadLocal<>();
    private final ThreadLocal<String> currentContainer = new InheritableThreadLocal<>();
    private final ThreadLocal<Boolean> forbidTestCaseStatusChange = new InheritableThreadLocal<>();

    private final EventHandler<TestSourceRead> featureStartedHandler = this::handleFeatureStartedHandler;
    private final EventHandler<TestCaseStarted> caseStartedHandler = this::handleTestCaseStarted;
    private final EventHandler<TestCaseFinished> caseFinishedHandler = this::handleTestCaseFinished;
    private final EventHandler<TestStepStarted> stepStartedHandler = this::handleTestStepStarted;
    private final EventHandler<TestStepFinished> stepFinishedHandler = this::handleTestStepFinished;
    private final EventHandler<WriteEvent> writeEventHandler = this::handleWriteEvent;
    private final EventHandler<EmbedEvent> embedEventHandler = this::handleEmbedEvent;

    private static final String TXT_EXTENSION = ".txt";
    private static final String TEXT_PLAIN = "text/plain";

    private static String lastStartedScenarioName;
    public static String getLastStartedScenarioName(){
        return lastStartedScenarioName;
    }

    private static Boolean isLastFinishedStepOK;
    public static Boolean getIsLastFinishedStepOK(){
        return isLastFinishedStepOK;
    }

    @SuppressWarnings("unused")
    public CucumberFeatureListener() {
        this(Allure.getLifecycle());
    }

    public CucumberFeatureListener(final AllureLifecycle lifecycle) {
        this.lifecycle = lifecycle;
    }

    /*
    Event Handlers
     */
    @Override
    public void setEventPublisher(final EventPublisher publisher) {
        publisher.registerHandlerFor(TestSourceRead.class, featureStartedHandler);

        publisher.registerHandlerFor(TestCaseStarted.class, caseStartedHandler);
        publisher.registerHandlerFor(TestCaseFinished.class, caseFinishedHandler);

        publisher.registerHandlerFor(TestStepStarted.class, stepStartedHandler);
        publisher.registerHandlerFor(TestStepFinished.class, stepFinishedHandler);

        publisher.registerHandlerFor(WriteEvent.class, writeEventHandler);
        publisher.registerHandlerFor(EmbedEvent.class, embedEventHandler);

        // custom handlers
        publisher.registerHandlerFor(TestSourceParsed.class, this::handleTestSourceParsed);
    }

    private void handleFeatureStartedHandler(final TestSourceRead event) {
        testSources.addTestSourceReadEvent(event.getUri(), event);
    }

    private void handleTestCaseStarted(final TestCaseStarted event) {
        // custom code
        shaftSetup();
        // end of custom code
        currentFeatureFile.set(event.getTestCase().getUri());
        currentFeature.set(testSources.getFeature(currentFeatureFile.get()));
        currentTestCase.set(event.getTestCase());
        currentContainer.set(UUID.randomUUID().toString());
        forbidTestCaseStatusChange.set(false);

        final Deque<String> tags = new LinkedList<>(currentTestCase.get().getTags());

        final Feature feature = currentFeature.get();

        final String name = currentTestCase.get().getName();
        final String featureName = feature.getName();

        final TestResult result = new TestResult()
                .setUuid(getTestCaseUuid(currentTestCase.get()))
                .setHistoryId(getHistoryId(currentTestCase.get()))
                .setFullName(featureName + ": " + name)
                .setName(name);

        final Scenario scenarioDefinition =
                testSources.getScenarioDefinition(
                        currentFeatureFile.get(),
                        currentTestCase.get().getLocation().getLine()
                );

        if (scenarioDefinition.getExamples() != null) {
            result.setParameters(
                    getExamplesAsParameters(scenarioDefinition, currentTestCase.get())
            );
        }

        final String description = Stream.of(feature.getDescription(), scenarioDefinition.getDescription())
                .filter(Objects::nonNull)
                .filter(s -> !s.isEmpty())
                .collect(Collectors.joining("\n"));

        if (!description.isEmpty()) {
            result.setDescription(description);
        }

        final TestResultContainer resultContainer = new TestResultContainer()
                .setName(String.format("%s: %s", scenarioDefinition.getKeyword(), scenarioDefinition.getName()))
                .setUuid(getTestContainerUuid())
                .setChildren(Collections.singletonList(getTestCaseUuid(currentTestCase.get())));

        lifecycle.scheduleTestCase(result);
        lifecycle.startTestContainer(getTestContainerUuid(), resultContainer);
        lifecycle.startTestCase(getTestCaseUuid(currentTestCase.get()));

        // custom code
        ReportManagerHelper.setFeatureName(featureName);
        lastStartedScenarioName = scenarioDefinition.getName();
        ReportManagerHelper.setTestCaseName(lastStartedScenarioName);
        ReportManagerHelper.setTestCaseDescription(scenarioDefinition.getDescription());
        if (Boolean.parseBoolean(System.getProperty("generateExtentReports").trim())){
            ReportManagerHelper.extentReportsCreateTest(feature.getName(), feature.getDescription());
        }
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
        ReportManagerHelper.logScenarioInformation(scenarioDefinition.getKeyword(), lastStartedScenarioName, cleanScenarioSteps.toString());
    }

    private void handleTestCaseFinished(final TestCaseFinished event) {
        //custom code
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
        } else {
            ReportManagerHelper.attachTestLog(lastStartedScenarioName,
                    InvokedMethodListener.createTestLog(Reporter.getOutput()));
        }
        // resetting scope and config
        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            ElementActions.switchToDefaultContent();
        }
        shaftTeardown();
        // end of custom code

        final String uuid = getTestCaseUuid(event.getTestCase());
        final Optional<StatusDetails> details = getStatusDetails(event.getResult().getError());
        details.ifPresent(statusDetails -> lifecycle.updateTestCase(
                uuid,
                testResult -> testResult.setStatusDetails(statusDetails)
        ));
        lifecycle.stopTestCase(uuid);
        lifecycle.stopTestContainer(getTestContainerUuid());
        lifecycle.writeTestCase(uuid);
        lifecycle.writeTestContainer(getTestContainerUuid());
    }

    private void handleTestStepStarted(final TestStepStarted event) {
        if (event.getTestStep() instanceof PickleStepTestStep) {
            final PickleStepTestStep pickleStep = (PickleStepTestStep) event.getTestStep();
            final String stepKeyword = Optional.ofNullable(
                    testSources.getKeywordFromSource(currentFeatureFile.get(), pickleStep.getStep().getLine())
            ).orElse("UNDEFINED");

            final StepResult stepResult = new StepResult()
                    .setName(String.format("%s %s", stepKeyword, pickleStep.getStep().getText()))
                    .setStart(System.currentTimeMillis());

            lifecycle.startStep(getTestCaseUuid(currentTestCase.get()), getStepUuid(pickleStep), stepResult);

            final StepArgument stepArgument = pickleStep.getStep().getArgument();
            if (stepArgument instanceof DataTableArgument) {
                final DataTableArgument dataTableArgument = (DataTableArgument) stepArgument;
                createDataTableAttachment(dataTableArgument);
            }
        } else if (event.getTestStep() instanceof HookTestStep) {
            initHook((HookTestStep) event.getTestStep());
        }

//        //custom code
//        var testStep = event.getTestStep();
//
//        if (testStep instanceof HookTestStep hookTestStep) {
//            ReportManager.logDiscrete("Scenario Hook: " + hookTestStep.getHookType().name());
//            lastStartedStepName = hookTestStep.getHookType().name();
//        }
//
//        if (testStep instanceof PickleStepTestStep pickleStepTestStep) {
//            ReportManager.logDiscrete("Scenario Step: " + pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText());
//            lastStartedStepName = pickleStepTestStep.getStep().getKeyword() + pickleStepTestStep.getStep().getText();
//        }
    }

    private void initHook(final HookTestStep hook) {

        final FixtureResult hookResult = new FixtureResult()
                .setName(hook.getCodeLocation())
                .setStart(System.currentTimeMillis());

        if (hook.getHookType() == HookType.BEFORE) {
            lifecycle.startPrepareFixture(getTestContainerUuid(), getHookStepUuid(hook), hookResult);
        } else {
            lifecycle.startTearDownFixture(getTestContainerUuid(), getHookStepUuid(hook), hookResult);
        }

    }

    private void handleTestStepFinished(final TestStepFinished event) {
        if (event.getTestStep() instanceof HookTestStep) {
            handleHookStep(event);
        } else {
            handlePickleStep(event);
        }

        //custom code
        isLastFinishedStepOK = event.getResult().getStatus().isOk();
    }

    private void handleWriteEvent(final WriteEvent event) {
        lifecycle.addAttachment(
                "Text output",
                TEXT_PLAIN,
                TXT_EXTENSION,
                Objects.toString(event.getText()).getBytes(StandardCharsets.UTF_8)
        );
    }

    private void handleEmbedEvent(final EmbedEvent event) {
        lifecycle.addAttachment(event.name, event.getMediaType(), null, new ByteArrayInputStream(event.getData()));
    }

    /*
    Utility Methods
     */

    private String getTestContainerUuid() {
        return currentContainer.get();
    }

    private String getTestCaseUuid(final TestCase testCase) {
        return scenarioUuids.computeIfAbsent(getHistoryId(testCase), it -> UUID.randomUUID().toString());
    }

    private String getStepUuid(final PickleStepTestStep step) {
        return currentFeature.get().getName() + getTestCaseUuid(currentTestCase.get())
                + step.getStep().getText() + step.getStep().getLine();
    }

    private String getHookStepUuid(final HookTestStep step) {
        return currentFeature.get().getName() + getTestCaseUuid(currentTestCase.get())
                + step.getHookType().toString() + step.getCodeLocation();
    }

    private String getHistoryId(final TestCase testCase) {
        final String testCaseLocation = testCase.getUri().toString()
                .substring(testCase.getUri().toString().lastIndexOf('/') + 1)
                + ":" + testCase.getLocation().getLine();
        return md5(testCaseLocation);
    }

    private Status translateTestCaseStatus(final Result testCaseResult) {
        switch (testCaseResult.getStatus()) {
            case FAILED:
                return getStatus(testCaseResult.getError())
                        .orElse(Status.FAILED);
            case PASSED:
                return Status.PASSED;
            case SKIPPED:
            case PENDING:
                return Status.SKIPPED;
            case AMBIGUOUS:
            case UNDEFINED:
            default:
                return null;
        }
    }

    private List<Parameter> getExamplesAsParameters(
            final Scenario scenario, final TestCase localCurrentTestCase
    ) {
        final Optional<Examples> maybeExample =
                scenario.getExamples().stream()
                        .filter(example -> example.getTableBody().stream()
                                .anyMatch(row -> row.getLocation().getLine()
                                        == localCurrentTestCase.getLocation().getLine())
                        )
                        .findFirst();

        if (!maybeExample.isPresent()) {
            return Collections.emptyList();
        }

        final Examples examples = maybeExample.get();

        final Optional<TableRow> maybeRow = examples.getTableBody().stream()
                .filter(example -> example.getLocation().getLine() == localCurrentTestCase.getLocation().getLine())
                .findFirst();

        if (!maybeRow.isPresent()) {
            return Collections.emptyList();
        }

        final TableRow row = maybeRow.get();

        return IntStream.range(0, examples.getTableHeader().getCells().size())
                .mapToObj(index -> {
                    final String name = examples.getTableHeader().getCells().get(index).getValue();
                    final String value = row.getCells().get(index).getValue();
                    return createParameter(name, value);
                })
                .collect(Collectors.toList());
    }

    private void createDataTableAttachment(final DataTableArgument dataTableArgument) {
        final List<List<String>> rowsInTable = dataTableArgument.cells();
        final StringBuilder dataTableCsv = new StringBuilder();
        for (List<String> columns : rowsInTable) {
            if (!columns.isEmpty()) {
                for (int i = 0; i < columns.size(); i++) {
                    if (i == columns.size() - 1) {
                        dataTableCsv.append(columns.get(i));
                    } else {
                        dataTableCsv.append(columns.get(i));
                        dataTableCsv.append('\t');
                    }
                }
                dataTableCsv.append('\n');
            }
        }
        final String attachmentSource = lifecycle
                .prepareAttachment("Data table", "text/tab-separated-values", "csv");
        lifecycle.writeAttachment(attachmentSource,
                new ByteArrayInputStream(dataTableCsv.toString().getBytes(StandardCharsets.UTF_8)));
    }

    private void handleHookStep(final TestStepFinished event) {
        final HookTestStep hookStep = (HookTestStep) event.getTestStep();
        final String uuid = getHookStepUuid(hookStep);
        final FixtureResult fixtureResult = new FixtureResult().setStatus(translateTestCaseStatus(event.getResult()));

        if (!Status.PASSED.equals(fixtureResult.getStatus())) {
            final TestResult testResult = new TestResult().setStatus(translateTestCaseStatus(event.getResult()));
            final StatusDetails statusDetails = getStatusDetails(event.getResult().getError())
                    .orElseGet(StatusDetails::new);

            final String errorMessage = event.getResult().getError() == null ? hookStep.getHookType()
                    .name() + " is failed." : hookStep.getHookType()
                    .name() + " is failed: " + event.getResult().getError().getLocalizedMessage();
            statusDetails.setMessage(errorMessage);

            if (hookStep.getHookType() == HookType.BEFORE) {
                testResult.setStatus(Status.SKIPPED);
                updateTestCaseStatus(testResult.getStatus());
                forbidTestCaseStatusChange.set(true);
            } else {
                testResult.setStatus(Status.BROKEN);
                updateTestCaseStatus(testResult.getStatus());
            }
            fixtureResult.setStatusDetails(statusDetails);
        }

        lifecycle.updateFixture(uuid, result -> result.setStatus(fixtureResult.getStatus())
                .setStatusDetails(fixtureResult.getStatusDetails()));
        lifecycle.stopFixture(uuid);
    }

    private void handlePickleStep(final TestStepFinished event) {

        final Status stepStatus = translateTestCaseStatus(event.getResult());
        final StatusDetails statusDetails;
        if (event.getResult().getStatus() == io.cucumber.plugin.event.Status.UNDEFINED) {
            updateTestCaseStatus(Status.PASSED);

            statusDetails =
                    getStatusDetails(new IllegalStateException("Undefined Step. Please add step definition"))
                            .orElse(new StatusDetails());
            lifecycle.updateTestCase(getTestCaseUuid(currentTestCase.get()), scenarioResult ->
                    scenarioResult
                            .setStatusDetails(statusDetails));
        } else {
            statusDetails =
                    getStatusDetails(event.getResult().getError())
                            .orElse(new StatusDetails());
            updateTestCaseStatus(stepStatus);
        }

        if (!Status.PASSED.equals(stepStatus) && stepStatus != null) {
            forbidTestCaseStatusChange.set(true);
        }

        lifecycle.updateStep(getStepUuid((PickleStepTestStep) event.getTestStep()),
                stepResult -> stepResult.setStatus(stepStatus).setStatusDetails(statusDetails));
        lifecycle.stopStep(getStepUuid((PickleStepTestStep) event.getTestStep()));
    }

    private void updateTestCaseStatus(final Status status) {
        if (!forbidTestCaseStatusChange.get()) {
            lifecycle.updateTestCase(getTestCaseUuid(currentTestCase.get()),
                    result -> result.setStatus(status));
        }
    }

    // custom code
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
                return new ByteArrayInputStream(FileActions.readFromFile(uri.getPath()).getBytes());
            }
        });
    }

    private void shaftSetup(){
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            System.setProperty("disableLogging", "true");
            PropertyFileManager.readPropertyFiles();
            ImageProcessingActions.loadOpenCV();
            System.setProperty("disableLogging", "false");
            ReportManagerHelper.logEngineVersion();
            ProjectStructureManager.initialize();
            ReportManagerHelper.initializeAllureReportingEnvironment();
            ReportManagerHelper.initializeExtentReportingEnvironment();
            LogsHelper.attachImportantLinks();
            LogsHelper.attachPropertyFiles();
            ReportManagerHelper.generateJDKShellFilesToProjectDirectory();
            ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
            ReportManagerHelper.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
        }
    }

    private void shaftTeardown(){
        if (Reporter.getCurrentTestResult() == null) {
            // running in native Cucumber mode
            LogsHelper.closeAllDriversAndattachBrowserLogs();
            LogsHelper.attachFullLogs();
            LogsHelper.attachCucumberReport();
            LogsHelper.attachExtentReport();
            ReportManagerHelper.setDiscreteLogging(true);
            ReportManagerHelper.generateAllureReportArchive();
            ReportManagerHelper.openAllureReportAfterExecution();
            AlterSuiteListener.reportExecutionStatusToJira();
        }
    }
}
