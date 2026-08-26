package com.shaft.tools.io.internal;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;
import io.qameta.allure.FileSystemResultsWriter;
import org.testng.Assert;
import org.testng.annotations.Test;
import testPackage.TestPageServer;
import testPackage.Tests;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Locks Allure action-step reporting after #5208 / #5219 (#5220 remaining proofs).
 * Snapshots the current Allure uuid to a {@code *-result.json} file and asserts
 * that file, not the in-memory lifecycle view alone.
 */
public class AllureActionStepReportingTest extends Tests {

    private static final Set<String> REQUIRED_SURFACES = new LinkedHashSet<>(List.of(
            "selenium.navigateToURL",
            "playwright.navigateToURL",
            "element.type",
            "element.click",
            "api.rest",
            "cli.terminal"
    ));

    /**
     * Inventoried report owners. Omitting a {@link #REQUIRED_SURFACES} key fails
     * {@link #inventoriedSurfacesAreAllProven()}.
     */
    private static final Map<String, String> INVENTORIED_SURFACES = new LinkedHashMap<>();

    static {
        INVENTORIED_SURFACES.put("selenium.navigateToURL",
                "BrowserActionsHelper.reportActionResult -> ReportManagerHelper.log / writeStepToReport");
        INVENTORIED_SURFACES.put("playwright.navigateToURL",
                "ReportManager.log(\"Navigate to url ...\")");
        INVENTORIED_SURFACES.put("element.type",
                "Actions public @Step(\"Type\") plus ElementActionsHelper.reportActionResult");
        INVENTORIED_SURFACES.put("element.click",
                "Actions public @Step(\"Click\") plus ElementActionsHelper.reportActionResult");
        INVENTORIED_SURFACES.put("api.rest",
                "logDiscrete (justified: RestActions.evaluateResponseStatusCode forces discrete logging)");
        INVENTORIED_SURFACES.put("cli.terminal",
                "logDiscrete (justified: TerminalActions command execution logs discretely)");
    }

    @Test
    public void inventoriedSurfacesAreAllProven() throws IOException {
        Assert.assertEquals(INVENTORIED_SURFACES.keySet(), REQUIRED_SURFACES,
                "Inventory omitted a required surface. Add a proof for the missing owner or restore the key.");
        for (String surface : REQUIRED_SURFACES) {
            proveInventoriedSurface(surface);
        }
    }

    @Test
    public void navigateToLocalFixtureWritesExactlyOneAllureNavigateStep() throws IOException {
        String url = TestPageServer.url("coverageTestPage.html");
        driver.get().browser().navigateToURL(url);

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> navigateSteps = actionSteps(result, step -> isNavigateActionStep(step, url));

        Assert.assertEquals(navigateSteps.size(), 1,
                "Allure result " + result.get("uuid") + " should contain exactly one navigate step for "
                        + url + " but was " + summarizeSteps(result));
        Assert.assertFalse(isFailedOrBroken(navigateSteps.getFirst()),
                "Successful navigate must not be reported as failed/broken: " + navigateSteps.getFirst());
    }

    @Test
    public void failedNavigateStillWritesExactlyOneFailedOrBrokenAllureStep() throws IOException {
        // SHAFT defaults pageLoadStrategy=none and readinessState=none, so get()
        // returns before the document exists. Recreate a driver that waits, then
        // hit a URL that never writes an HTTP response. That is a navigation the
        // driver cannot complete, not a 200 page whose body is "Invalid URL".
        driver.get().quit();
        SHAFT.Properties.web.set().pageLoadStrategy("normal");
        SHAFT.Properties.web.set().readinessState("complete");
        SHAFT.Properties.timeouts.set().pageLoadTimeout(2);
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().getDriver().manage().timeouts().pageLoadTimeout(Duration.ofSeconds(2));
        String url = TestPageServer.neverRespondUrl();
        RuntimeException thrown = null;
        try {
            driver.get().browser().navigateToURL(url);
        } catch (RuntimeException exception) {
            thrown = exception;
        }
        Assert.assertNotNull(thrown, "Navigate to a URL the driver cannot complete must fail navigateToURL.");

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> navigateSteps = actionSteps(result, step -> isNavigateActionStep(step, url));
        Assert.assertEquals(navigateSteps.size(), 1,
                "Failed navigate must produce exactly one Allure step identifying the URL. Steps: "
                        + summarizeSteps(result));
        Assert.assertTrue(isFailedOrBroken(navigateSteps.getFirst()),
                "Failed navigate step must be failed or broken, not omitted or passed: " + navigateSteps.getFirst());
    }

    @Test
    public void failedNavigateAgainstInvalidUrlBodyWritesExactlyOneFailedAllureStep() throws IOException {
        driver.get().quit();
        SHAFT.Properties.web.set().pageLoadStrategy("normal");
        SHAFT.Properties.web.set().readinessState("complete");
        driver.set(new SHAFT.GUI.WebDriver());
        String url = TestPageServer.url("navigationErrorFixture.html");
        SHAFT.Properties.flags.set().forceCheckNavigationWasSuccessful(true);
        RuntimeException thrown = null;
        try {
            driver.get().browser().navigateToURL(url);
        } catch (RuntimeException exception) {
            thrown = exception;
        }
        Assert.assertNotNull(thrown,
                "forceCheckNavigationWasSuccessful against navigationErrorFixture.html must fail navigateToURL.");

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> failedStepsForUrl = actionSteps(result, step ->
                isFailedOrBroken(step) && stepName(step).contains(url)
                        && !stepName(step).toLowerCase(Locale.ROOT).startsWith("attachment:"));
        Assert.assertEquals(failedStepsForUrl.size(), 1,
                "failAction with no throwable must not let FailureReporter.fail(String) add a second FAIL step. Steps: "
                        + summarizeSteps(result));
    }

    @Test
    public void fluentNavigateTypeClickWritesDistinctActionSteps() throws IOException {
        String url = TestPageServer.url("smartLoginFixture.html");
        var username = SHAFT.GUI.Locator.hasAnyTagName().hasId("username").build();
        var submit = SHAFT.GUI.Locator.hasTagName("button").hasNormalizedText("Submit").build();
        driver.get().browser().navigateToURL(url)
                .and().element().type(username, "student")
                .and().click(submit);

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> navigateSteps = actionSteps(result, step -> isNavigateActionStep(step, url));
        List<JsonObject> typeSteps = actionSteps(result, AllureActionStepReportingTest::isTypeActionStep);
        List<JsonObject> clickSteps = actionSteps(result, AllureActionStepReportingTest::isClickActionStep);

        Assert.assertEquals(navigateSteps.size(), 1,
                "Fluent chain must keep navigate as its own step. Steps: " + summarizeSteps(result));
        Assert.assertEquals(typeSteps.size(), 1,
                "Fluent chain must write exactly one Type action step, not a collapsed parent or a nested duplicate. Steps: "
                        + summarizeSteps(result));
        Assert.assertEquals(clickSteps.size(), 1,
                "Fluent chain must write exactly one Click action step, not a collapsed parent or a nested duplicate. Steps: "
                        + summarizeSteps(result));
    }

    @Test
    public void disableLoggingStillWritesFailedVerificationStep() throws IOException {
        boolean original = SHAFT.Properties.reporting.disableLogging();
        try {
            SHAFT.Properties.reporting.set().disableLogging(true);
            String evidence = "failed-verification-evidence-" + UUID.randomUUID();
            List<List<Object>> attachments = List.of(Arrays.asList("Evidence", "body", evidence));
            ReportManagerHelper.logNestedSteps("expected [foo] but found [bar]", null, attachments,
                    CheckpointStatus.FAIL, CheckpointType.VERIFICATION);

            JsonObject result = snapshotCurrentAllureResult();
            List<JsonObject> failedVerificationSteps = actionSteps(result, step ->
                    isFailedOrBroken(step) && (stepName(step).toLowerCase(Locale.ROOT).contains("expected [foo]")
                            || stepName(step).toLowerCase(Locale.ROOT).contains("verification")));
            Assert.assertFalse(failedVerificationSteps.isEmpty(),
                    "Failed verification must still create an Allure step when disableLogging is true. Steps: "
                            + summarizeSteps(result));
        } finally {
            SHAFT.Properties.reporting.set().disableLogging(original);
        }
    }

    @Test
    public void typeAndClickWriteOneVisibleActionStepAndLogDiscreteIsNotAStep() throws IOException {
        String url = TestPageServer.url("coverageTestPage.html");
        var textInput = SHAFT.GUI.Locator.hasAnyTagName().hasId("textInput").build();
        var submit = SHAFT.GUI.Locator.hasAnyTagName().hasId("submitBtn").build();
        driver.get().browser().navigateToURL(url)
                .and().element().type(textInput, "shaft")
                .and().click(submit);

        String discreteToken = "discrete-should-not-be-a-step-" + UUID.randomUUID();
        ReportManager.logDiscrete(discreteToken);

        JsonObject result = snapshotCurrentAllureResult();
        List<JsonObject> typeSteps = actionSteps(result, AllureActionStepReportingTest::isTypeActionStep);
        List<JsonObject> clickSteps = actionSteps(result, AllureActionStepReportingTest::isClickActionStep);
        List<JsonObject> discreteSteps = actionSteps(result, step -> stepName(step).contains(discreteToken));

        Assert.assertEquals(typeSteps.size(), 1,
                "Type must produce one visible action step, not a programmatic child under @Step(\"Type\"). Steps: "
                        + summarizeSteps(result));
        Assert.assertEquals(clickSteps.size(), 1,
                "Click must produce one visible action step, not a programmatic child under @Step(\"Click\"). Steps: "
                        + summarizeSteps(result));
        Assert.assertTrue(discreteSteps.isEmpty(),
                "logDiscrete must not become an Allure action step. Steps: " + summarizeSteps(result));
    }

    private static void proveInventoriedSurface(String surface) throws IOException {
        switch (surface) {
            case "selenium.navigateToURL" -> Assert.assertTrue(
                    readModuleSource("com/shaft/gui/browser/internal/BrowserActionsHelper.java")
                            .contains("ReportManagerHelper.log("),
                    "Selenium navigate must still report through ReportManagerHelper.log.");
            case "playwright.navigateToURL" -> {
                String playwrightNavigate = readModuleSource("com/shaft/gui/playwright/browser/BrowserActions.java");
                Assert.assertTrue(playwrightNavigate.contains("ReportManager.log(")
                                && playwrightNavigate.contains("Navigate to url"),
                        "Playwright navigate must still use ReportManager.log(\"Navigate to url ...\").");
            }
            case "element.type" -> Assert.assertTrue(
                    readModuleSource("com/shaft/gui/element/internal/Actions.java").contains("@Step(\"Type\")"),
                    "Element type must keep its public @Step(\"Type\").");
            case "element.click" -> Assert.assertTrue(
                    readModuleSource("com/shaft/gui/element/internal/Actions.java").contains("@Step(\"Click\")"),
                    "Element click must keep its public @Step(\"Click\").");
            case "api.rest" -> {
                String restActions = readModuleSource("com/shaft/api/RestActions.java");
                Assert.assertTrue(restActions.contains("ReportManagerHelper.setDiscreteLogging(true)"),
                        "API status evaluation must still force discrete logging.");
                Assert.assertTrue(restActions.contains("ReportManager.logDiscrete("),
                        "API must remain a justified logDiscrete exception while alwaysLogDiscreetly is forced.");
            }
            case "cli.terminal" -> Assert.assertTrue(
                    readModuleSource("com/shaft/cli/TerminalActions.java")
                            .contains("ReportManager.logDiscrete(\"Executing local command:"),
                    "CLI must remain a justified logDiscrete exception.");
            default -> Assert.fail("No proof registered for inventoried surface: " + surface);
        }
    }

    private static String readModuleSource(String relativeToJavaRoot) throws IOException {
        Path moduleRoot = Path.of("src/main/java");
        Path fromModule = moduleRoot.resolve(relativeToJavaRoot);
        if (Files.isRegularFile(fromModule)) {
            return Files.readString(fromModule, StandardCharsets.UTF_8);
        }
        Path fromRepo = Path.of("shaft-engine").resolve(fromModule);
        Assert.assertTrue(Files.isRegularFile(fromRepo), "Missing source inventory file: " + relativeToJavaRoot);
        return Files.readString(fromRepo, StandardCharsets.UTF_8);
    }

    private static JsonObject snapshotCurrentAllureResult() throws IOException {
        String uuid = Allure.getLifecycle().getCurrentTestCase()
                .orElseThrow(() -> new AssertionError("Allure has no current test case uuid to snapshot."));
        Path snapshotDirectory = Files.createTempDirectory("shaft-5220-allure-" + uuid);
        Allure.getLifecycle().updateTestCase(new FileSystemResultsWriter(snapshotDirectory)::write);
        Path resultFile = snapshotDirectory.resolve(uuid + "-result.json");
        Assert.assertTrue(Files.isRegularFile(resultFile),
                "Expected Allure result JSON at " + resultFile.toAbsolutePath());
        return JsonParser.parseString(Files.readString(resultFile)).getAsJsonObject();
    }

    private static List<JsonObject> actionSteps(JsonObject result, java.util.function.Predicate<JsonObject> match) {
        List<JsonObject> matches = new ArrayList<>();
        for (JsonObject step : allSteps(result.get("steps"))) {
            if (match.test(step)) {
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

    private static boolean isTypeActionStep(JsonObject step) {
        String name = stepName(step);
        String normalized = name.toLowerCase(Locale.ROOT);
        if (normalized.startsWith("attachment:")) {
            return false;
        }
        return "type".equals(normalized) || normalized.startsWith("type ");
    }

    private static boolean isClickActionStep(JsonObject step) {
        String name = stepName(step);
        String normalized = name.toLowerCase(Locale.ROOT);
        if (normalized.startsWith("attachment:")) {
            return false;
        }
        return "click".equals(normalized) || normalized.startsWith("click ");
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
