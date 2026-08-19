package com.shaft.tools.io.internal;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.driver.SHAFT;
import io.qameta.allure.Allure;
import io.qameta.allure.FileSystemResultsWriter;
import org.testng.Assert;
import org.testng.annotations.Test;
import testPackage.TestPageServer;
import testPackage.Tests;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Comparator;

/**
 * Locks Allure action-step reporting for {@code navigateToURL} (#5208).
 * The test snapshots the current Allure uuid to a {@code *-result.json} file
 * and asserts that file, not the in-memory lifecycle view alone.
 *
 * <p>Inventory of report sinks after the shared-path fix:
 * <ul>
 *   <li>{@code ReportManager.log} / {@code ReportManagerHelper.log} / {@code writeStepToReport}
 *       — one Allure step per call (browser, Playwright, API/CLI helpers that already report).</li>
 *   <li>{@code BrowserActionsHelper.reportActionResult} and {@code ElementActionsHelper.reportActionResult}
 *       — same owner; screenshot stays an attachment inside the step.</li>
 *   <li>{@code Actions} public {@code @Step} methods — already start a step; locator metadata
 *       ({@code decision.element-actions-normalize-allure-locator-metadata}) stays intact.</li>
 *   <li>{@code ReportManager.logDiscrete} — not an action; must not become a step.</li>
 *   <li>{@code attachAsStep} — attachment, not a second action step.</li>
 * </ul>
 */
public class AllureActionStepReportingTest extends Tests {

    @Test
    public void navigateToLocalFixtureWritesExactlyOneAllureNavigateStep() throws IOException {
        String url = TestPageServer.url("coverageTestPage.html");
        driver.get().browser().navigateToURL(url);

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> navigateSteps = navigateActionSteps(result, url);

        Assert.assertEquals(navigateSteps.size(), 1,
                "Allure result " + result.get("uuid") + " should contain exactly one navigate step for "
                        + url + " but was " + summarizeSteps(result));
        Assert.assertFalse(isFailedOrBroken(navigateSteps.getFirst()),
                "Successful navigate must not be reported as failed/broken: " + navigateSteps.getFirst());
    }

    @Test
    public void failedNavigateStillWritesFailedOrBrokenAllureStep() throws IOException {
        String url = TestPageServer.url("navigationErrorFixture.html");
        SHAFT.Properties.flags.set().forceCheckNavigationWasSuccessful(true);
        RuntimeException thrown = null;
        try {
            driver.get().browser().navigateToURL(url);
        } catch (RuntimeException exception) {
            thrown = exception;
        }
        Assert.assertNotNull(thrown, "Forced navigation check against navigationErrorFixture.html must fail navigateToURL.");

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> navigateSteps = navigateActionSteps(result, url);
        Assert.assertFalse(navigateSteps.isEmpty(),
                "Failed navigate must still produce an Allure step identifying the URL. Steps: "
                        + summarizeSteps(result));
        Assert.assertTrue(navigateSteps.stream().anyMatch(AllureActionStepReportingTest::isFailedOrBroken),
                "Failed navigate step must be failed or broken, not omitted or passed: " + navigateSteps);
    }

    private static JsonObject snapshotCurrentAllureResult() throws IOException {
        String uuid = Allure.getLifecycle().getCurrentTestCase()
                .orElseThrow(() -> new AssertionError("Allure has no current test case uuid to snapshot."));
        Path snapshotDirectory = Files.createTempDirectory("shaft-5208-allure-" + uuid);
        Allure.getLifecycle().updateTestCase(new FileSystemResultsWriter(snapshotDirectory)::write);
        Path resultFile = snapshotDirectory.resolve(uuid + "-result.json");
        Assert.assertTrue(Files.isRegularFile(resultFile),
                "Expected Allure result JSON at " + resultFile.toAbsolutePath());
        return JsonParser.parseString(Files.readString(resultFile)).getAsJsonObject();
    }

    private static List<JsonObject> navigateActionSteps(JsonObject result, String url) {
        List<JsonObject> matches = new ArrayList<>();
        for (JsonObject step : allSteps(result.get("steps"))) {
            if (isNavigateActionStep(step, url)) {
                matches.add(step);
            }
        }
        return matches;
    }

    private static boolean isNavigateActionStep(JsonObject step, String url) {
        String name = stepName(step);
        String normalized = name.toLowerCase(Locale.ROOT);
        if (normalized.startsWith("attachment:")) {
            return false;
        }
        boolean identifiesNavigate = normalized.contains("navigate to url")
                || normalized.contains("navigatetourl");
        return identifiesNavigate && name.contains(url);
    }

    private static List<JsonObject> allSteps(JsonElement stepsElement) {
        List<JsonObject> steps = new ArrayList<>();
        collectSteps(stepsElement, steps);
        return steps;
    }

    private static void collectSteps(JsonElement stepsElement, List<JsonObject> sink) {
        if (stepsElement == null || !stepsElement.isJsonArray()) {
            return;
        }
        JsonArray steps = stepsElement.getAsJsonArray();
        for (JsonElement element : steps) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject step = element.getAsJsonObject();
            sink.add(step);
            collectSteps(step.get("steps"), sink);
        }
    }

    private static String stepName(JsonObject step) {
        return step.has("name") && !step.get("name").isJsonNull() ? step.get("name").getAsString() : "";
    }

    private static boolean isFailedOrBroken(JsonObject step) {
        if (!step.has("status") || step.get("status").isJsonNull()) {
            return false;
        }
        String status = step.get("status").getAsString();
        return "failed".equals(status) || "broken".equals(status);
    }

    private static String summarizeSteps(JsonObject result) {
        List<String> names = new ArrayList<>();
        for (JsonObject step : allSteps(result.get("steps"))) {
            names.add(stepName(step) + "[" + (step.has("status") ? step.get("status").getAsString() : "none") + "]");
        }
        names.sort(Comparator.naturalOrder());
        return names.toString();
    }
}
