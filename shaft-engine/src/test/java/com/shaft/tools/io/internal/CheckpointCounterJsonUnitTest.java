package com.shaft.tools.io.internal;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.listeners.internal.TestExecutionInfo;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Covers the machine-readable checkpoint JSON emitter added for issue #3516 (D), plus the stable
 * per-test checkpoint ids and per-test attribution added for issues #3532 (D) / #3534 (P3).
 */
public class CheckpointCounterJsonUnitTest {

    /**
     * Returns the {@code id} of the first checkpoint row whose {@code message} equals the marker,
     * or {@code -1} when no such row exists in the payload.
     */
    private static int idForMessage(String json, String marker) {
        JsonArray rows = JsonParser.parseString(json).getAsJsonObject().getAsJsonArray("checkpoints");
        for (JsonElement element : rows) {
            JsonObject row = element.getAsJsonObject();
            if (marker.equals(row.get("message").getAsString())) {
                return row.get("id").getAsInt();
            }
        }
        return -1;
    }

    /**
     * Returns the first checkpoint row whose {@code message} equals the marker, or {@code null}.
     */
    private static JsonObject rowForMessage(String json, String marker) {
        JsonArray rows = JsonParser.parseString(json).getAsJsonObject().getAsJsonArray("checkpoints");
        for (JsonElement element : rows) {
            JsonObject row = element.getAsJsonObject();
            if (marker.equals(row.get("message").getAsString())) {
                return row;
            }
        }
        return null;
    }

    @Test(description = "checkpointsJson emits structured, machine-readable checkpoint rows (#3516 D)")
    public void checkpointsJsonEmitsStructuredRows() {
        // Unique markers so the assertions hold regardless of the process-wide cumulative state.
        String uniquePass = "unit-json-checkpoint-pass-marker";
        String uniqueFail = "unit-json-checkpoint-fail-marker";
        CheckpointCounter.increment(CheckpointType.ASSERTION, uniquePass, CheckpointStatus.PASS);
        CheckpointCounter.increment(CheckpointType.VERIFICATION, uniqueFail, CheckpointStatus.FAIL);

        String json = CheckpointCounter.checkpointsJson();

        // Structured envelope for Doctor / MCP / plugin widgets.
        Assert.assertTrue(json.contains("\"checkpoints\""), json);
        Assert.assertTrue(json.contains("\"total\""), json);
        Assert.assertTrue(json.contains("\"passed\""), json);
        Assert.assertTrue(json.contains("\"failed\""), json);
        Assert.assertTrue(json.contains("\"id\""), json);
        // Both recorded checkpoints and their type/status are present.
        Assert.assertTrue(json.contains(uniquePass), json);
        Assert.assertTrue(json.contains(uniqueFail), json);
        Assert.assertTrue(json.contains("ASSERTION"), json);
        Assert.assertTrue(json.contains("VERIFICATION"), json);
        Assert.assertTrue(json.contains("PASS"), json);
        Assert.assertTrue(json.contains("FAIL"), json);
    }

    @Test(description = "checkpointsJson exposes a per-type breakdown, and typeStatusCount tallies it (#3523)")
    public void checkpointsJsonExposesPerTypeBreakdown() {
        long assertionPassBefore = CheckpointCounter.typeStatusCount(
                CheckpointType.ASSERTION.toString(), CheckpointStatus.PASS.toString());
        long verificationFailBefore = CheckpointCounter.typeStatusCount(
                CheckpointType.VERIFICATION.toString(), CheckpointStatus.FAIL.toString());

        CheckpointCounter.increment(CheckpointType.ASSERTION, "breakdown-assert-pass", CheckpointStatus.PASS);
        CheckpointCounter.increment(CheckpointType.VERIFICATION, "breakdown-verify-fail", CheckpointStatus.FAIL);

        // typeStatusCount reflects the new checkpoints.
        Assert.assertEquals(CheckpointCounter.typeStatusCount(
                CheckpointType.ASSERTION.toString(), CheckpointStatus.PASS.toString()), assertionPassBefore + 1);
        Assert.assertEquals(CheckpointCounter.typeStatusCount(
                CheckpointType.VERIFICATION.toString(), CheckpointStatus.FAIL.toString()), verificationFailBefore + 1);

        String json = CheckpointCounter.checkpointsJson();
        Assert.assertTrue(json.contains("\"byType\""), json);
        Assert.assertTrue(json.contains("\"assertion\""), json);
        Assert.assertTrue(json.contains("\"verification\""), json);
    }

    @Test(description = "checkpoints get stable, strictly-increasing, distinct ids stamped at capture (#3532 D / #3534 P3)")
    public void checkpointsGetStableDistinctIds() {
        String firstMarker = "stable-id-marker-first";
        String secondMarker = "stable-id-marker-second";
        CheckpointCounter.increment(CheckpointType.ASSERTION, firstMarker, CheckpointStatus.PASS);
        int firstId = idForMessage(CheckpointCounter.checkpointsJson(), firstMarker);

        // A later, unrelated checkpoint must not renumber the already-captured one (the old
        // per-attach ordinal was regenerated on every attach; the stable id must not move).
        CheckpointCounter.increment(CheckpointType.VERIFICATION, secondMarker, CheckpointStatus.FAIL);
        String json = CheckpointCounter.checkpointsJson();
        int firstIdAfter = idForMessage(json, firstMarker);
        int secondId = idForMessage(json, secondMarker);

        Assert.assertTrue(firstId > 0, "id must be a positive, process-wide value: " + firstId);
        Assert.assertEquals(firstIdAfter, firstId, "an already-captured checkpoint id must stay stable");
        Assert.assertTrue(secondId > firstId, "ids must strictly increase: " + firstId + " -> " + secondId);
    }

    @Test(description = "checkpoint-browser details HTML surfaces the owning test id per row (#3534 checkpoint browser)")
    public void checkpointDetailsHtmlSurfacesOwningTestPerRow() {
        String marker = "browser-attribution-marker";
        String className = "com.example.CheckoutTest";
        String methodName = "appliesDiscount";
        ReportContext.start(new TestExecutionInfo(
                className + "#" + methodName, className, methodName, methodName, null, null, null, false));
        try {
            CheckpointCounter.increment(CheckpointType.ASSERTION, marker, CheckpointStatus.PASS);
        } finally {
            ReportContext.clear();
        }
        // A suite-level checkpoint (no test identity) captured after clearing the context.
        CheckpointCounter.increment(CheckpointType.VERIFICATION, "browser-suite-level-marker", CheckpointStatus.PASS);

        String html = CheckpointCounter.checkpointDetailsHtml();

        // The attributed checkpoint carries its test both as a filterable data-test attribute and a visible cell.
        Assert.assertTrue(html.contains("data-test=\"" + className + "#" + methodName + "\""), html);
        Assert.assertTrue(html.contains(">" + className + "#" + methodName + "<"), html);
        // A checkpoint with no captured identity renders the suite-level placeholder, never a blank test.
        Assert.assertTrue(html.contains("data-test=\"\""), html);
        Assert.assertTrue(html.contains(">(suite)<"), html);
    }

    @Test(description = "checkpoint-browser test filter lists one distinct option per owning test, none for suite-level (#3534)")
    public void checkpointTestFilterOptionsListDistinctTests() {
        String classA = "com.example.AlphaTest";
        String classB = "com.example.BetaTest";
        // Two checkpoints for AlphaTest (must dedupe to a single option) and one for BetaTest.
        ReportContext.start(new TestExecutionInfo(classA + "#one", classA, "one", "one", null, null, null, false));
        try {
            CheckpointCounter.increment(CheckpointType.ASSERTION, "opt-a1", CheckpointStatus.PASS);
            CheckpointCounter.increment(CheckpointType.ASSERTION, "opt-a2", CheckpointStatus.PASS);
        } finally {
            ReportContext.clear();
        }
        ReportContext.start(new TestExecutionInfo(classB + "#two", classB, "two", "two", null, null, null, false));
        try {
            CheckpointCounter.increment(CheckpointType.VERIFICATION, "opt-b1", CheckpointStatus.FAIL);
        } finally {
            ReportContext.clear();
        }
        // A suite-level checkpoint must not produce an option.
        CheckpointCounter.increment(CheckpointType.ASSERTION, "opt-suite", CheckpointStatus.PASS);

        String options = CheckpointCounter.checkpointTestFilterOptions();

        Assert.assertEquals(countOccurrences(options, "<option value=\"" + classA + "#one\">"), 1,
                "AlphaTest must appear exactly once despite two checkpoints: " + options);
        Assert.assertTrue(options.contains("<option value=\"" + classB + "#two\">"), options);
        // Suite-level checkpoints have no test id, so no empty-value option is emitted here.
        Assert.assertFalse(options.contains("value=\"\""), options);
    }

    private static int countOccurrences(String haystack, String needle) {
        int count = 0;
        for (int i = haystack.indexOf(needle); i >= 0; i = haystack.indexOf(needle, i + needle.length())) {
            count++;
        }
        return count;
    }

    @Test(description = "checkpoints are attributed to the owning test (class/method/testId) (#3532 D / #3534 P3)")
    public void checkpointsAttributeToOwningTest() {
        String marker = "attribution-marker";
        String className = "com.example.OrderServiceTest";
        String methodName = "placesOrder";
        ReportContext.start(new TestExecutionInfo(
                className + "#" + methodName, className, methodName, methodName, null, null, null, false));
        try {
            CheckpointCounter.increment(CheckpointType.ASSERTION, marker, CheckpointStatus.PASS);
        } finally {
            // never leak identity into sibling tests sharing the cumulative counter
            ReportContext.clear();
        }

        JsonObject row = rowForMessage(CheckpointCounter.checkpointsJson(), marker);
        Assert.assertNotNull(row, "attributed checkpoint row must be present");
        Assert.assertEquals(row.get("testClass").getAsString(), className);
        Assert.assertEquals(row.get("testMethod").getAsString(), methodName);
        Assert.assertEquals(row.get("testId").getAsString(), className + "#" + methodName);
    }
}
