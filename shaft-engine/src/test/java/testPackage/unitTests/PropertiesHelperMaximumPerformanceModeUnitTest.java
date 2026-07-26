package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;

@Test(singleThreaded = true)
public class PropertiesHelperMaximumPerformanceModeUnitTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void maximumPerformanceModeShouldDisableWebDriverLogsCaptureAndPerformanceReport() throws Exception {
        int originalMode = SHAFT.Properties.flags.maximumPerformanceMode();
        boolean originalCaptureWebDriverLogs = SHAFT.Properties.reporting.captureWebDriverLogs();
        boolean originalGeneratePerformanceReport = SHAFT.Properties.performance.isEnablePerformanceReport();

        try {
            SHAFT.Properties.flags.set().maximumPerformanceMode(1);
            SHAFT.Properties.reporting.set().captureWebDriverLogs(true);
            SHAFT.Properties.performance.set().generatePerformanceReport(true);
            invokeMaximumPerformanceModeOverride();
            Assert.assertFalse(SHAFT.Properties.reporting.captureWebDriverLogs(),
                    "maximumPerformanceMode should disable WebDriver log capture.");
            Assert.assertFalse(SHAFT.Properties.performance.isEnablePerformanceReport(),
                    "maximumPerformanceMode should disable API performance report generation.");
        } finally {
            SHAFT.Properties.flags.set().maximumPerformanceMode(originalMode);
            SHAFT.Properties.reporting.set().captureWebDriverLogs(originalCaptureWebDriverLogs);
            SHAFT.Properties.performance.set().generatePerformanceReport(originalGeneratePerformanceReport);
        }
    }

    @Test
    public void maximumPerformanceModeShouldDisableScreenshotHighlightingAndTelemetry() throws Exception {
        int originalMode = SHAFT.Properties.flags.maximumPerformanceMode();
        boolean originalScreenshotHighlightElements = SHAFT.Properties.visuals.screenshotParamsHighlightElements();
        boolean originalTelemetryEnabled = SHAFT.Properties.flags.telemetryEnabled();

        try {
            SHAFT.Properties.flags.set().maximumPerformanceMode(1);
            SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(true);
            SHAFT.Properties.flags.set().telemetryEnabled(true);
            invokeMaximumPerformanceModeOverride();
            Assert.assertFalse(SHAFT.Properties.visuals.screenshotParamsHighlightElements(),
                    "maximumPerformanceMode should disable screenshot highlighting to reduce overhead.");
            Assert.assertFalse(SHAFT.Properties.flags.telemetryEnabled(),
                    "maximumPerformanceMode should disable telemetry to avoid complementary overhead.");
        } finally {
            SHAFT.Properties.flags.set().maximumPerformanceMode(originalMode);
            SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(originalScreenshotHighlightElements);
            SHAFT.Properties.flags.set().telemetryEnabled(originalTelemetryEnabled);
        }
    }

    /**
     * Settles the ordering unknown flagged in issue #4161: {@code TestNGListener.onStart(ISuite)}
     * calls {@code DriverFactoryHelper.preflightRemoteGridIfConfigured()} -&gt;
     * {@code PropertiesHelper.postProcessing()} at SUITE start, strictly before any
     * {@code @BeforeMethod}. A generated test's {@code setUp()} runs later, on the same
     * (single, non-parallel) replay thread. Since both paths write {@code healing.strategy}
     * through the same thread-local {@code ThreadLocalPropertiesManager} map, the later write on
     * that shared thread wins -- so a generated test's setUp() that sets {@code healing.strategy}
     * would silently defeat maximumPerformanceMode's forced "disabled", not the other way round.
     */
    @Test
    public void laterHealingStrategyOverrideShouldWinOverMaximumPerformanceModeDisable() throws Exception {
        String originalStrategy = SHAFT.Properties.healing.strategy();
        int originalMode = SHAFT.Properties.flags.maximumPerformanceMode();
        try {
            SHAFT.Properties.flags.set().maximumPerformanceMode(1);
            invokeMaximumPerformanceModeOverride();
            Assert.assertEquals(SHAFT.Properties.healing.strategy(), "disabled",
                    "maximumPerformanceMode should force-disable healing when it runs first.");

            // Simulates a generated test's setUp() running strictly after suite-start
            // postProcessing() on the same replay thread.
            SHAFT.Properties.healing.set().strategy("shaft-heal");

            Assert.assertEquals(SHAFT.Properties.healing.strategy(), "shaft-heal",
                    "A later same-thread override must win: setUp() running after suite start "
                            + "silently defeats maximumPerformanceMode's healing-disable.");
        } finally {
            SHAFT.Properties.flags.set().maximumPerformanceMode(originalMode);
            SHAFT.Properties.healing.set().strategy(originalStrategy);
        }
    }

    private void invokeMaximumPerformanceModeOverride() throws Exception {
        Method method = PropertiesHelper.class.getDeclaredMethod("overridePropertiesForMaximumPerformanceMode");
        method.setAccessible(true);
        method.invoke(null);
    }
}
