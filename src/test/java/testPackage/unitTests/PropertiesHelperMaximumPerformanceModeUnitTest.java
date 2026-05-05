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

    private void invokeMaximumPerformanceModeOverride() throws Exception {
        Method method = PropertiesHelper.class.getDeclaredMethod("overridePropertiesForMaximumPerformanceMode");
        method.setAccessible(true);
        method.invoke(null);
    }
}
