package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test(singleThreaded = true)
public class FlakeProfilerUnitTest {
    private static final TestExecutionInfo TEST_INFO = new TestExecutionInfo(
            "example.FlakeProfilerUnitTest.shouldProfile",
            "example.FlakeProfilerUnitTest",
            "shouldProfile",
            "shouldProfile",
            "",
            null,
            null,
            false);

    @BeforeMethod(alwaysRun = true)
    public void setup() {
        Properties.clearForCurrentThread();
        FlakeProfiler.reset();
        SHAFT.Properties.reporting.set()
                .flakeProfilerEnabled(true)
                .flakeProfilerAttachPerTest(true)
                .flakeProfilerFailOnSevereFlakeRisk(false)
                .flakeProfilerSlowActionThresholdMs(100);
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        ValidationsHelper.resetVerificationStateAfterFailing();
        FlakeProfiler.reset();
        Properties.clearForCurrentThread();
    }

    @Test(description = "Flake profiler aggregates action, wait, evidence, locator, healing, and retry signals")
    public void flakeProfilerAggregatesCoreSignals() {
        FlakeProfiler.startTest(TEST_INFO);
        FlakeProfiler.recordAction(FlakeProfiler.ActionSample.of("action", "CLICK")
                .target("By.id: submit")
                .durationMillis(250)
                .locatorLookupCount(2)
                .matchCount(1)
                .staleElementRetries(1)
                .healingAttempts(1)
                .waitLoopCount(3)
                .successful(true));
        FlakeProfiler.recordEvidenceCapture("screenshot", "CLICK", 80);
        FlakeProfiler.recordRetryAttempt("shouldProfile", 1, 2, true);
        FlakeProfiler.finishTest(TEST_INFO, "Failed");

        String json = FlakeProfiler.buildSummaryJson();

        Assert.assertTrue(json.contains("\"test\" : \"example.FlakeProfilerUnitTest.shouldProfile\""));
        Assert.assertTrue(json.contains("\"topSlowActions\""));
        Assert.assertTrue(json.contains("\"waitHeavyActions\""));
        Assert.assertTrue(json.contains("\"category\" : \"action\""));
        Assert.assertTrue(json.contains("\"name\" : \"CLICK\""));
        Assert.assertTrue(json.contains("\"durationMillis\" : 250"));
        Assert.assertTrue(json.contains("\"locatorLookupCount\" : 2"));
        Assert.assertTrue(json.contains("\"matchCount\" : 1"));
        Assert.assertTrue(json.contains("\"staleElementRetries\" : 1"));
        Assert.assertTrue(json.contains("\"healingAttempts\" : 1"));
        Assert.assertTrue(json.contains("\"waitLoopCount\" : 3"));
        Assert.assertTrue(json.contains("\"evidenceCaptureMillis\" : 80"));
        Assert.assertTrue(json.contains("\"retryAttempts\" : 1"));
        Assert.assertTrue(json.contains("\"supportingEvidenceEnabled\" : true"));
    }

    @Test(description = "Flake profiler stays off by default and ignores events while disabled")
    public void flakeProfilerDoesNotRecordWhenDisabled() {
        SHAFT.Properties.reporting.set().flakeProfilerEnabled(false);

        FlakeProfiler.startTest(TEST_INFO);
        FlakeProfiler.recordAction(FlakeProfiler.ActionSample.of("action", "CLICK")
                .target("By.id: disabled")
                .durationMillis(250)
                .locatorLookupCount(1)
                .matchCount(1)
                .waitLoopCount(1)
                .successful(true));
        FlakeProfiler.finishTest(TEST_INFO, "Passed");

        String json = FlakeProfiler.buildSummaryJson();

        Assert.assertTrue(json.contains("\"tests\" : [ ]"));
        Assert.assertTrue(json.contains("\"totalActions\" : 0"));
    }

    @Test(description = "Flake profiler records standalone assertion duration as an assertion action")
    public void flakeProfilerRecordsStandaloneAssertionDuration() {
        FlakeProfiler.startTest(TEST_INFO);

        SHAFT.Validations.assertThat().object("actual").isEqualTo("actual").perform();
        FlakeProfiler.finishTest(TEST_INFO, "Passed");

        String json = FlakeProfiler.buildSummaryJson();

        Assert.assertTrue(json.contains("\"category\" : \"assertion\""));
        Assert.assertTrue(json.contains("\"name\" : \"objectsAreEqual\""));
        Assert.assertTrue(json.contains("\"successful\" : true"));
    }

    @Test(description = "Flake profiler records failed soft verifications as unsuccessful verification actions")
    public void flakeProfilerRecordsSoftVerificationFailure() {
        FlakeProfiler.startTest(TEST_INFO);

        SHAFT.Validations.verifyThat().object("actual").isEqualTo("expected").perform();
        FlakeProfiler.finishTest(TEST_INFO, "Failed");

        String json = FlakeProfiler.buildSummaryJson();

        Assert.assertTrue(json.contains("\"category\" : \"verification\""));
        Assert.assertTrue(json.contains("\"name\" : \"objectsAreEqual\""));
        Assert.assertTrue(json.contains("\"successful\" : false"));
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "Flake profiler proposed reporting configuration keys can be set programmatically")
    public void flakeProfilerConfigurationKeysAreSupported() {
        SHAFT.Properties.reporting.set()
                .flakeProfilerEnabled(true)
                .flakeProfilerAttachPerTest(false)
                .flakeProfilerFailOnSevereFlakeRisk(true)
                .flakeProfilerSlowActionThresholdMs(321);

        Assert.assertTrue(SHAFT.Properties.reporting.flakeProfilerEnabled());
        Assert.assertFalse(SHAFT.Properties.reporting.flakeProfilerAttachPerTest());
        Assert.assertTrue(SHAFT.Properties.reporting.flakeProfilerFailOnSevereFlakeRisk());
        Assert.assertEquals(SHAFT.Properties.reporting.flakeProfilerSlowActionThresholdMs(), 321);
    }
}
