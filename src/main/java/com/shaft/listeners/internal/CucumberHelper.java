package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import com.shaft.tools.internal.FirestoreRestClient;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.*;
import io.cucumber.plugin.event.Status;
import org.testng.Reporter;
import org.testng.xml.XmlSuite;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Internal helper class providing shared setup and teardown logic for Cucumber-based
 * test runs managed by the SHAFT framework.
 *
 * <p>This class is not intended for direct use in test code; it is invoked by
 * SHAFT's Cucumber listeners.
 */
public class CucumberHelper {
    static long executionStartTime;
    static long executionEndTime;

    private static final AtomicInteger skippedScenarios = new AtomicInteger(0);

    // Scenario-ID sets used to detect and count flaky/passed/failed scenarios.
    // A scenario ID is derived from its feature file URI and line number, which remains stable
    // across retry attempts for the same scenario definition.
    // Counts are derived purely from these sets so that retried scenarios are never double-counted.
    private static final Set<String> passedScenarioIds = Collections.synchronizedSet(new HashSet<>());
    private static final Set<String> failedScenarioIds = Collections.synchronizedSet(new HashSet<>());

    /**
     * Utility class — do not instantiate.
     */
    private CucumberHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Applies SHAFT Cucumber property values as parameters on each TestNG suite so that
     * the Cucumber runner can pick them up at execution time.
     *
     * @param suites the list of TestNG {@link org.testng.xml.XmlSuite} objects to configure
     */
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

    /**
     * Initialises the SHAFT engine for a Cucumber run: records the execution start time,
     * triggers engine setup, and configures legacy {@code cucumber.options} system property.
     * The scenario counters are reset here so that a single JVM running multiple Cucumber
     * suites sequentially never accumulates stale counts from a prior run.
     */
    public static void engineSetup() {
        executionStartTime = System.currentTimeMillis();
        // Reset counters/sets for the new run to avoid accumulation across sequential suite runs.
        skippedScenarios.set(0);
        passedScenarioIds.clear();
        failedScenarioIds.clear();
        TestNGListener.engineSetup(ProjectStructureManager.RunType.CUCUMBER);
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

    /**
     * Records the outcome of a completed Cucumber scenario for telemetry.
     * This must be called from the test-case-finished handler in the Cucumber listener
     * so that the actual scenario counts are captured before {@link #shaftTearDown()}.
     * <p>
     * The {@code scenarioId} should be a stable string that uniquely identifies the
     * scenario definition across retry attempts (e.g. {@code "featureUri:line"}).
     * It is used to detect flaky scenarios: those that failed on at least one attempt
     * but eventually passed.
     *
     * @param status     the Cucumber {@link Status} of the finished scenario attempt
     * @param scenarioId a stable identifier for the scenario (e.g. feature URI + line number)
     */
    public static void recordScenarioResult(Status status, String scenarioId) {
        switch (status) {
            case PASSED -> passedScenarioIds.add(scenarioId);
            case FAILED, AMBIGUOUS, UNDEFINED -> failedScenarioIds.add(scenarioId);
            // SKIPPED, PENDING, and any future Status values are all treated as skipped
            // because they represent scenarios that did not run to completion successfully.
            default -> skippedScenarios.incrementAndGet();
        }
    }

    /**
     * Performs SHAFT engine teardown at the end of a native Cucumber run (i.e. when there is
     * no active TestNG {@link org.testng.Reporter} result). Attaches logs, generates the
     * Allure report archive, and reports results to Jira.
     */
    public static void shaftTearDown() {
        if (Reporter.getCurrentTestResult() == null) {
            executionEndTime = System.currentTimeMillis();
            // running in native Cucumber mode
            ReportHelper.attachEngineLog();
            ReportHelper.attachCucumberReport();
            CheckpointCounter.attach();
            ReportHelper.attachIssuesLog();

            ReportManagerHelper.setDiscreteLogging(true);
            JiraHelper.reportExecutionStatusToJira();
            GoogleTink.encrypt();
            AllureManager.generateAllureReportArchive();
            AllureManager.openAllureReportAfterExecution();
            Thread.ofVirtual().start(() -> {
                // Derive all counts from the per-scenario-ID sets so retried scenarios are
                // never double-counted regardless of how many times they were attempted.
                // Categories are mutually exclusive: passed | failed | skipped | flaky.
                // Flaky = scenarios that appear in both the failed and passed sets.
                Set<String> flakyIds = new HashSet<>(failedScenarioIds);
                flakyIds.retainAll(passedScenarioIds);
                int flakyCount = flakyIds.size();
                // Passed = ultimately passed scenarios, minus those that also failed (flaky).
                int uniquePassed = passedScenarioIds.size() - flakyCount;
                // Failed = scenarios that only ever failed (never made it to passed).
                int uniqueFailed = failedScenarioIds.size() - flakyCount;
                FirestoreRestClient.sendTelemetry(
                        executionStartTime, executionEndTime,
                        uniquePassed, uniqueFailed, skippedScenarios.get(), flakyCount);
            });
            ReportManagerHelper.logEngineClosure();
        }
    }
}
