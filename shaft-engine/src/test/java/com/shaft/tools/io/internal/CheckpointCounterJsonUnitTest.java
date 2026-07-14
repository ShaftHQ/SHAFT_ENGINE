package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Covers the machine-readable checkpoint JSON emitter added for issue #3516 (D).
 */
public class CheckpointCounterJsonUnitTest {

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
}
