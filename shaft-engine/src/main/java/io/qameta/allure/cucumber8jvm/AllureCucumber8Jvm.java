/*
 *  Copyright 2016-2026 Qameta Software Inc
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
package io.qameta.allure.cucumber8jvm;

import io.cucumber.messages.types.Envelope;
import io.cucumber.messages.types.Examples;
import io.cucumber.messages.types.Feature;
import io.cucumber.messages.types.Scenario;
import io.cucumber.messages.types.TableCell;
import io.cucumber.messages.types.TableRow;
import io.cucumber.messages.types.TestRunHookFinished;
import io.cucumber.messages.types.TestStepResultStatus;
import io.cucumber.plugin.ConcurrentEventListener;
import io.cucumber.plugin.event.DataTableArgument;
import io.cucumber.plugin.event.EmbedEvent;
import io.cucumber.plugin.event.EventHandler;
import io.cucumber.plugin.event.EventPublisher;
import io.cucumber.plugin.event.HookTestStep;
import io.cucumber.plugin.event.HookType;
import io.cucumber.plugin.event.PickleStepTestStep;
import io.cucumber.plugin.event.Result;
import io.cucumber.plugin.event.Step;
import io.cucumber.plugin.event.StepArgument;
import io.cucumber.plugin.event.TestCase;
import io.cucumber.plugin.event.TestCaseFinished;
import io.cucumber.plugin.event.TestCaseStarted;
import io.cucumber.plugin.event.TestRunFinished;
import io.cucumber.plugin.event.TestRunStarted;
import io.cucumber.plugin.event.TestSourceRead;
import io.cucumber.plugin.event.TestStepFinished;
import io.cucumber.plugin.event.TestStepStarted;
import io.cucumber.plugin.event.WriteEvent;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureExternalKey;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.AttachmentOptions;
import io.qameta.allure.cucumber8jvm.testsourcemodel.TestSourcesModelProxy;
import io.qameta.allure.model.FixtureResult;
import io.qameta.allure.model.Parameter;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static io.qameta.allure.util.ResultsUtils.createGlobalError;
import static io.qameta.allure.util.ResultsUtils.createParameter;
import static io.qameta.allure.util.ResultsUtils.createTitlePath;
import static io.qameta.allure.util.ResultsUtils.createTitlePathFromSourcePath;
import static io.qameta.allure.util.ResultsUtils.getStatus;
import static io.qameta.allure.util.ResultsUtils.getStatusDetails;
import static io.qameta.allure.util.ResultsUtils.md5;

/**
 * Reports Cucumber JVM 8 execution to Allure.
 *
 * <p>Add this plugin to the Cucumber runtime so feature, scenario, step, hook, and attachment events are converted into Allure results. Use the default lifecycle for normal runs or pass one explicitly for embedded runners and tests.</p>
 */
@SuppressWarnings(
    {
            "ClassDataAbstractionCoupling",
            "ClassFanOutComplexity",
            "PMD.TooManyMethods",
            "PMD.GodClass",
    }
)
public class AllureCucumber8Jvm implements ConcurrentEventListener {

    private static final String COLON = ":";
    private static final String CSV_DELIMITER = ",";
    private static final String CSV_QUOTE = "\"";
    private static final String CSV_ESCAPED_QUOTE = CSV_QUOTE + CSV_QUOTE;
    private static final String NEW_LINE = "\n";
    private static final String CARRIAGE_RETURN = "\r";
    private static final String UNDEFINED_STEP_MESSAGE = "Undefined Step. Please add step definition";

    private final AllureLifecycle lifecycle;

    private final TestSourcesModelProxy testSources = new TestSourcesModelProxy();

    private final AtomicBoolean scenarioStarted = new AtomicBoolean();

    private final EventHandler<TestRunStarted> runStartedHandler = event -> scenarioStarted.set(false);
    private final EventHandler<TestSourceRead> featureStartedHandler = this::handleFeatureStartedHandler;
    private final EventHandler<TestCaseStarted> caseStartedHandler = this::handleTestCaseStarted;
    private final EventHandler<TestCaseFinished> caseFinishedHandler = this::handleTestCaseFinished;
    private final EventHandler<TestStepStarted> stepStartedHandler = this::handleTestStepStarted;
    private final EventHandler<TestStepFinished> stepFinishedHandler = this::handleTestStepFinished;
    private final EventHandler<WriteEvent> writeEventHandler = this::handleWriteEvent;
    private final EventHandler<EmbedEvent> embedEventHandler = this::handleEmbedEvent;
    private final EventHandler<TestRunFinished> runFinishedHandler = this::handleTestRunFinished;
    private final EventHandler<Envelope> envelopeHandler = this::handleEnvelope;

    private final Map<UUID, String> hookStepContainerUuid = new ConcurrentHashMap<>();

    private static final String TEXT_PLAIN = "text/plain";
    private static final String CUCUMBER_WORKING_DIR = Paths.get("").toUri().getSchemeSpecificPart();

    /**
     * Creates an Allure cucumber8 jvm with default configuration.
     */
    @SuppressWarnings("unused")
    public AllureCucumber8Jvm() {
        this(Allure.getLifecycle());
    }

    /**
     * Creates an Allure cucumber8 jvm with the supplied values.
     *
     * @param lifecycle the Allure lifecycle to use
     */
    public AllureCucumber8Jvm(final AllureLifecycle lifecycle) {
        this.lifecycle = lifecycle;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setEventPublisher(final EventPublisher publisher) {
        publisher.registerHandlerFor(TestRunStarted.class, runStartedHandler);
        publisher.registerHandlerFor(TestSourceRead.class, featureStartedHandler);

        publisher.registerHandlerFor(TestCaseStarted.class, caseStartedHandler);
        publisher.registerHandlerFor(TestCaseFinished.class, caseFinishedHandler);

        publisher.registerHandlerFor(TestStepStarted.class, stepStartedHandler);
        publisher.registerHandlerFor(TestStepFinished.class, stepFinishedHandler);

        publisher.registerHandlerFor(WriteEvent.class, writeEventHandler);
        publisher.registerHandlerFor(EmbedEvent.class, embedEventHandler);
        publisher.registerHandlerFor(TestRunFinished.class, runFinishedHandler);
        // Cucumber 8 emits @BeforeAll and @AfterAll failures as messages. The
        // plugin TestRunFinished result has no throwable for those hooks.
        publisher.registerHandlerFor(Envelope.class, envelopeHandler);
    }

    private static AllureExternalKey scopeKey(final String uuid) {
        return AllureExternalKey.of(AllureCucumber8Jvm.class, "scope", uuid);
    }

    private static AllureExternalKey testKey(final String uuid) {
        return AllureExternalKey.of(AllureCucumber8Jvm.class, "test", uuid);
    }

    private static AllureExternalKey fixtureKey(final String uuid) {
        return AllureExternalKey.of(AllureCucumber8Jvm.class, "fixture", uuid);
    }

    private static AllureExternalKey stepKey(final String uuid) {
        return AllureExternalKey.of(AllureCucumber8Jvm.class, "step", uuid);
    }

    private void handleFeatureStartedHandler(final TestSourceRead event) {
        testSources.addTestSourceReadEvent(event.getUri(), event);
    }

    private void handleTestCaseStarted(final TestCaseStarted event) {
        scenarioStarted.set(true);
        final TestCase testCase = event.getTestCase();
        final Feature feature = testSources.getFeature(testCase.getUri());

        final Deque<String> tags = new LinkedList<>(testCase.getTags());
        final LabelBuilder labelBuilder = new LabelBuilder(feature, testCase, tags);

        final String name = testCase.getName();

        // the same way full name is generated for
        // org.junit.platform.engine.support.descriptor.ClasspathResourceSource
        // to support io.qameta.allure.junitplatform.AllurePostDiscoveryFilter
        final String fullName = getTestCaseLocation(testCase);

        final String testCaseUuid = testCase.getId().toString();

        final List<String> titlePath = createTitlePathFromSourcePath(getTestCaseUri(testCase));
        titlePath.addAll(createTitlePath(feature.getName()));

        final TestResult result = new TestResult()
                .setUuid(testCaseUuid)
                .setTestCaseId(getTestCaseId(testCase))
                .setFullName(fullName)
                .setTitlePath(titlePath)
                .setName(name)
                .setLabels(labelBuilder.getScenarioLabels())
                .setLinks(labelBuilder.getScenarioLinks());

        final Scenario scenarioDefinition = testSources.getScenarioDefinition(
                testCase.getUri(),
                testCase.getLocation().getLine()
        );

        if (scenarioDefinition.getExamples() != null) {
            result.getParameters().addAll(getExamplesAsParameters(scenarioDefinition, testCase));
        }

        final String description = Stream.of(feature.getDescription(), scenarioDefinition.getDescription())
                .filter(Objects::nonNull)
                .filter(s -> !s.isEmpty())
                .collect(Collectors.joining(NEW_LINE));

        if (!description.isEmpty()) {
            result.setDescription(description);
        }

        final AllureExternalKey testKey = testKey(testCaseUuid);
        lifecycle.scheduleTest(testKey, result);
        lifecycle.addDefaultLabels(testKey, labelBuilder.getDefaultLabels());
        lifecycle.startTest(testKey);
    }

    /**
     * Reports an error outside an individual scenario as an error of the run.
     *
     * <p>A failed scenario also makes the run-finished result unsuccessful, but its error stays on the
     * {@link TestCaseFinished} event and the run-finished result has no throwable. Only a throwable here represents
     * a failure such as a run-level hook that has no scenario result of its own.</p>
     */
    private void handleTestRunFinished(final TestRunFinished event) {
        final Result result = event.getResult();
        if (Objects.isNull(result) || Objects.isNull(result.getError())) {
            return;
        }
        writeRunLevelError(result.getError());
    }

    /**
     * Cucumber 8 returns a failed {@code @BeforeAll} or {@code @AfterAll} from the runner and emits
     * {@link TestRunHookFinished} instead of attaching that throwable to {@link TestRunFinished}.
     */
    private void handleEnvelope(final Envelope envelope) {
        envelope.getTestRunHookFinished().ifPresent(finished -> {
            if (finished.getResult().getStatus() != TestStepResultStatus.FAILED) {
                return;
            }
            final String detail = finished.getResult().getMessage()
                    .or(
                            () -> finished.getResult().getException().flatMap(
                                    io.cucumber.messages.types.Exception::getMessage
                            )
                    )
                    .orElse("failed");
            writeRunLevelError(new IllegalStateException(detail));
        });
    }

    private void writeRunLevelError(final Throwable failure) {
        final String context = scenarioStarted.get()
                ? "Cucumber test run failed"
                : "Cucumber test run failed, scenarios did not run";
        lifecycle.writeGlobals(createGlobalError(context, failure));
    }

    private void handleTestCaseFinished(final TestCaseFinished event) {
        final TestCase testCase = event.getTestCase();
        final Feature feature = testSources.getFeature(testCase.getUri());
        final String uuid = testCase.getId().toString();
        final Result result = event.getResult();
        final Status status = translateTestCaseStatus(result);
        final StatusDetails statusDetails = translateStatusDetails(result);

        final TagParser tagParser = new TagParser(feature, testCase);
        statusDetails
                .setFlaky(tagParser.isFlaky())
                .setMuted(tagParser.isMuted())
                .setKnown(tagParser.isKnown());

        final AllureExternalKey testKey = testKey(uuid);
        lifecycle.updateTest(
                testKey, testResult -> testResult
                        .setStatus(status)
                        .setStatusDetails(statusDetails)
        );

        lifecycle.stopTest(testKey);
        lifecycle.writeTest(testKey);
    }

    private void handleTestStepStarted(final TestStepStarted event) {
        final TestCase testCase = event.getTestCase();
        if (event.getTestStep() instanceof HookTestStep) {
            final HookTestStep hook = (HookTestStep) event.getTestStep();

            if (isFixtureHook(hook)) {
                handleStartFixtureHook(testCase, hook);
            } else {
                handleStartStepHook(testCase, hook);
            }
        } else if (event.getTestStep() instanceof PickleStepTestStep) {
            handleStartPickleStep(testCase, (PickleStepTestStep) event.getTestStep());
        }
    }

    private void handleStartPickleStep(final TestCase testCase,
                                       final PickleStepTestStep pickleStep) {
        final String uuid = testCase.getId().toString();
        final Step step = pickleStep.getStep();

        final StepResult stepResult = new StepResult()
                .setName(step.getKeyword() + step.getText())
                .setStart(System.currentTimeMillis());

        final AllureExternalKey stepKey = stepKey(pickleStep.getId().toString());
        lifecycle.setCurrent(testKey(uuid));
        lifecycle.startStep(stepKey, stepResult);

        final StepArgument stepArgument = step.getArgument();
        if (stepArgument instanceof DataTableArgument) {
            final DataTableArgument dataTableArgument = (DataTableArgument) stepArgument;
            createDataTableAttachment(stepKey, dataTableArgument);
        }
    }

    private void handleStartStepHook(final TestCase testCase,
                                     final HookTestStep hook) {
        final String uuid = testCase.getId().toString();
        final StepResult stepResult = new StepResult()
                .setName(hook.getCodeLocation())
                .setStart(System.currentTimeMillis());

        lifecycle.setCurrent(testKey(uuid));
        lifecycle.startStep(stepKey(hook.getId().toString()), stepResult);
    }

    private void handleStartFixtureHook(final TestCase testCase,
                                        final HookTestStep hook) {
        final String uuid = testCase.getId().toString();

        final UUID hookId = hook.getId();
        final String containerUuid = hookStepContainerUuid
                .computeIfAbsent(hookId, unused -> UUID.randomUUID().toString());

        final AllureExternalKey scopeKey = scopeKey(containerUuid);
        lifecycle.registerScope(scopeKey);
        lifecycle.addTestToScope(scopeKey, testKey(uuid));

        final FixtureResult hookResult = new FixtureResult()
                .setName(hook.getCodeLocation());

        final String fixtureUuid = hookId.toString();
        final AllureExternalKey fixtureKey = fixtureKey(fixtureUuid);
        if (hook.getHookType() == HookType.BEFORE) {
            lifecycle.startBeforeFixture(scopeKey, fixtureKey, hookResult);
        } else {
            lifecycle.startAfterFixture(scopeKey, fixtureKey, hookResult);
        }
    }

    private void handleTestStepFinished(final TestStepFinished event) {
        if (event.getTestStep() instanceof HookTestStep) {
            final HookTestStep hook = (HookTestStep) event.getTestStep();
            if (isFixtureHook(hook)) {
                handleStopHookStep(event.getResult(), hook);
            } else {
                handleStopStep(event.getTestCase(), event.getResult(), hook.getId());
            }
        } else if (event.getTestStep() instanceof PickleStepTestStep) {
            final PickleStepTestStep pickleStep = (PickleStepTestStep) event.getTestStep();
            handleStopStep(event.getTestCase(), event.getResult(), pickleStep.getId());
        }
    }

    private static boolean isFixtureHook(final HookTestStep hook) {
        return hook.getHookType() == HookType.BEFORE || hook.getHookType() == HookType.AFTER;
    }

    private void handleWriteEvent(final WriteEvent event) {
        // user output is genuinely ambient: it lands under whatever executable is current,
        // and is silently skipped (unsupported executables) — no key to address it by
        lifecycle.getCurrentExecutableKey().ifPresent(
                key -> lifecycle.addAttachment(
                        key,
                        "Text output",
                        TEXT_PLAIN,
                        new ByteArrayInputStream(Objects.toString(event.getText()).getBytes(StandardCharsets.UTF_8)),
                        AttachmentOptions.empty()
                )
        );
    }

    private void handleEmbedEvent(final EmbedEvent event) {
        lifecycle.getCurrentExecutableKey().ifPresent(
                key -> lifecycle.addAttachment(
                        key,
                        event.name,
                        event.getMediaType(),
                        new ByteArrayInputStream(event.getData()),
                        AttachmentOptions.empty()
                )
        );
    }

    private String getTestCaseLocation(final TestCase testCase) {
        return getTestCaseUri(testCase) + COLON + testCase.getLocation().getLine();
    }

    private String getTestCaseId(final TestCase testCase) {
        final String testCaseId = getTestCaseUri(testCase) + COLON + testCase.getName();
        return md5(testCaseId);
    }

    private String getTestCaseUri(final TestCase testCase) {
        final String testCaseUri = testCase.getUri().getSchemeSpecificPart();
        if (testCaseUri.startsWith(CUCUMBER_WORKING_DIR)) {
            return testCaseUri.substring(CUCUMBER_WORKING_DIR.length());
        }
        return testCaseUri;
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
                return Status.BROKEN;
            default:
                return null;
        }
    }

    private StatusDetails translateStatusDetails(final Result result) {
        if (result.getStatus() == io.cucumber.plugin.event.Status.UNDEFINED) {
            return new StatusDetails().setMessage(UNDEFINED_STEP_MESSAGE);
        }
        return getStatusDetails(result.getError())
                .orElseGet(StatusDetails::new);
    }

    private List<Parameter> getExamplesAsParameters(
                                                    final Scenario scenario,
                                                    final TestCase localCurrentTestCase) {

        final Optional<Examples> maybeExample = scenario.getExamples().stream()
                .filter(
                        example -> example.getTableBody().stream()
                                .anyMatch(row -> row.getLocation().getLine() == localCurrentTestCase.getLocation().getLine())
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
        final int size = row.getCells().size();

        final List<String> headerNames = examples.getTableHeader()
                .map(TableRow::getCells)
                .map(
                        rows -> rows.stream()
                                .map(TableCell::getValue)
                                .collect(Collectors.toList())
                )
                .orElse(null);

        return IntStream.range(0, size)
                .mapToObj(index -> {
                    final String name = Objects.nonNull(headerNames)
                            ? headerNames.get(index)
                            : "arg" + index;
                    final String value = row.getCells().get(index).getValue();
                    return createParameter(name, value);
                })
                .collect(Collectors.toList());
    }

    private void createDataTableAttachment(final AllureExternalKey stepKey,
                                           final DataTableArgument dataTableArgument) {
        // the data table belongs to the step this adapter just started — address it by key
        lifecycle.addAttachment(
                stepKey,
                "Data table",
                "text/csv",
                new ByteArrayInputStream(toCsv(dataTableArgument).getBytes(StandardCharsets.UTF_8)),
                AttachmentOptions.empty()
        );
    }

    private static String toCsv(final DataTableArgument dataTableArgument) {
        final List<List<String>> rowsInTable = dataTableArgument.cells();
        final StringBuilder dataTableCsv = new StringBuilder();
        for (List<String> columns : rowsInTable) {
            if (!columns.isEmpty()) {
                final String rowValue = columns.stream()
                        .map(value -> {
                            final String text = Objects.toString(value, "");
                            if (text.contains(CSV_DELIMITER)
                                    || text.contains(CSV_QUOTE)
                                    || text.contains(NEW_LINE)
                                    || text.contains(CARRIAGE_RETURN)) {
                                return CSV_QUOTE + text.replace(CSV_QUOTE, CSV_ESCAPED_QUOTE) + CSV_QUOTE;
                            }
                            return text;
                        })
                        .collect(Collectors.joining(CSV_DELIMITER, "", NEW_LINE));
                dataTableCsv.append(rowValue);
            }
        }
        return dataTableCsv.toString();
    }

    private void handleStopHookStep(final Result eventResult,
                                    final HookTestStep hook) {
        final String containerUuid = hookStepContainerUuid.get(hook.getId());
        if (Objects.isNull(containerUuid)) {
            // maybe throw an exception?
            return;
        }

        final String uuid = hook.getId().toString();

        final Status status = translateTestCaseStatus(eventResult);
        final StatusDetails statusDetails = getStatusDetails(eventResult.getError())
                .orElseGet(StatusDetails::new);

        final AllureExternalKey fixtureKey = fixtureKey(uuid);
        lifecycle.updateFixture(
                fixtureKey, result -> result
                        .setStatus(status)
                        .setStatusDetails(statusDetails)
        );
        lifecycle.stopFixture(fixtureKey);

        lifecycle.writeScope(scopeKey(containerUuid));
    }

    private void handleStopStep(final TestCase testCase,
                                final Result eventResult,
                                final UUID stepId) {
        final Feature feature = testSources.getFeature(testCase.getUri());

        final Status stepStatus = translateTestCaseStatus(eventResult);

        final StatusDetails statusDetails = translateStatusDetails(eventResult);

        final TagParser tagParser = new TagParser(feature, testCase);
        statusDetails
                .setFlaky(tagParser.isFlaky())
                .setMuted(tagParser.isMuted())
                .setKnown(tagParser.isKnown());

        final AllureExternalKey stepKey = stepKey(stepId.toString());
        lifecycle.updateStep(
                stepKey,
                stepResult -> stepResult
                        .setStatus(stepStatus)
                        .setStatusDetails(statusDetails)
        );
        lifecycle.stopStep(stepKey);
    }
}
