package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.lang.reflect.Method;

@Test(singleThreaded = true)
public class PropertiesHelperEvidenceLevelUnitTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void evidenceLevelShouldDefaultToFailureOnly() {
        Assert.assertEquals(SHAFT.Properties.reporting.evidenceLevel(), "FAILURE_ONLY");
    }

    @DataProvider
    public Object[][] evidenceProfiles() {
        return new Object[][]{
                {"FAILURE_ONLY", "FailuresOnly", "FailuresOnly", false, false, false, false, true, true, "auto"},
                {"BALANCED", "ValidationPointsOnly", "FailuresOnly", false, false, false, false, true, true, "auto"},
                {"FAST", "Never", "Never", false, false, false, false, false, false, "auto"},
                {"FULL", "Always", "Always", true, true, true, true, true, true, "always"}
        };
    }

    @Test(dataProvider = "evidenceProfiles")
    public void evidenceLevelProfilesShouldOverrideGranularControls(String evidenceLevel,
                                                                    String screenshots,
                                                                    String pageSource,
                                                                    boolean gif,
                                                                    boolean video,
                                                                    boolean webDriverLogs,
                                                                    boolean fullLog,
                                                                    boolean diagnostics,
                                                                    boolean trace,
                                                                    String traceMode) throws Exception {
        setOppositeGranularControls(screenshots, pageSource, gif, video, webDriverLogs, fullLog, diagnostics,
                trace, traceMode);
        SHAFT.Properties.reporting.set().evidenceLevel(evidenceLevel);

        invokeEvidenceLevelOverride();

        Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot(), screenshots);
        Assert.assertEquals(SHAFT.Properties.visuals.whenToTakePageSourceSnapshot(), pageSource);
        Assert.assertEquals(SHAFT.Properties.visuals.createAnimatedGif(), gif);
        Assert.assertEquals(SHAFT.Properties.visuals.videoParamsRecordVideo(), video);
        Assert.assertEquals(SHAFT.Properties.reporting.captureWebDriverLogs(), webDriverLogs);
        Assert.assertEquals(SHAFT.Properties.reporting.attachFullLog(), fullLog);
        Assert.assertEquals(SHAFT.Properties.reporting.diagnosticsBundleEnabled(), diagnostics);
        Assert.assertEquals(SHAFT.Properties.reporting.traceEnabled(), trace);
        Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), traceMode);
    }

    @Test
    public void customEvidenceLevelShouldLeaveGranularControlsUnchanged() throws Exception {
        SHAFT.Properties.reporting.set()
                .evidenceLevel("CUSTOM")
                .captureWebDriverLogs(true)
                .attachFullLog(true)
                .diagnosticsBundleEnabled(false)
                .traceEnabled(false)
                .traceMode("always");
        SHAFT.Properties.visuals.set()
                .screenshotParamsWhenToTakeAScreenshot("Always")
                .whenToTakePageSourceSnapshot("Never")
                .createAnimatedGif(true)
                .videoParamsRecordVideo(true);

        invokeEvidenceLevelOverride();

        Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot(), "Always");
        Assert.assertEquals(SHAFT.Properties.visuals.whenToTakePageSourceSnapshot(), "Never");
        Assert.assertTrue(SHAFT.Properties.visuals.createAnimatedGif());
        Assert.assertTrue(SHAFT.Properties.visuals.videoParamsRecordVideo());
        Assert.assertTrue(SHAFT.Properties.reporting.captureWebDriverLogs());
        Assert.assertTrue(SHAFT.Properties.reporting.attachFullLog());
        Assert.assertFalse(SHAFT.Properties.reporting.diagnosticsBundleEnabled());
        Assert.assertFalse(SHAFT.Properties.reporting.traceEnabled());
        Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "always");
    }

    @Test
    public void evidenceLevelSetterShouldApplyProfileImmediately() {
        SHAFT.Properties.reporting.set()
                .captureWebDriverLogs(true)
                .attachFullLog(true)
                .diagnosticsBundleEnabled(true)
                .traceEnabled(true)
                .traceMode("always");
        SHAFT.Properties.visuals.set()
                .screenshotParamsWhenToTakeAScreenshot("Always")
                .whenToTakePageSourceSnapshot("Always")
                .createAnimatedGif(true)
                .videoParamsRecordVideo(true);

        SHAFT.Properties.reporting.set().evidenceLevel("FAST");

        Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot(), "Never");
        Assert.assertEquals(SHAFT.Properties.visuals.whenToTakePageSourceSnapshot(), "Never");
        Assert.assertFalse(SHAFT.Properties.visuals.createAnimatedGif());
        Assert.assertFalse(SHAFT.Properties.visuals.videoParamsRecordVideo());
        Assert.assertFalse(SHAFT.Properties.reporting.captureWebDriverLogs());
        Assert.assertFalse(SHAFT.Properties.reporting.attachFullLog());
        Assert.assertFalse(SHAFT.Properties.reporting.diagnosticsBundleEnabled());
        Assert.assertFalse(SHAFT.Properties.reporting.traceEnabled());
        Assert.assertEquals(SHAFT.Properties.reporting.traceMode(), "auto");
    }

    private void setOppositeGranularControls(String screenshots, String pageSource, boolean gif, boolean video,
                                             boolean webDriverLogs, boolean fullLog, boolean diagnostics,
                                             boolean trace, String traceMode) {
        SHAFT.Properties.reporting.set()
                .captureWebDriverLogs(!webDriverLogs)
                .attachFullLog(!fullLog)
                .diagnosticsBundleEnabled(!diagnostics)
                .traceEnabled(!trace)
                .traceMode(traceMode.equals("always") ? "failure" : "always");
        SHAFT.Properties.visuals.set()
                .screenshotParamsWhenToTakeAScreenshot(screenshots.equals("Always") ? "Never" : "Always")
                .whenToTakePageSourceSnapshot(pageSource.equals("Always") ? "Never" : "Always")
                .createAnimatedGif(!gif)
                .videoParamsRecordVideo(!video);
    }

    private void invokeEvidenceLevelOverride() throws Exception {
        Method method = PropertiesHelper.class.getDeclaredMethod("overridePropertiesForEvidenceLevel");
        method.setAccessible(true);
        method.invoke(null);
    }
}
