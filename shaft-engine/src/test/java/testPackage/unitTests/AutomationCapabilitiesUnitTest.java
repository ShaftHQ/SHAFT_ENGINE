package testPackage.unitTests;

import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.CapabilitySupport;
import com.shaft.gui.capabilities.UnsupportedAutomationFeatureException;
import org.testng.Assert;
import org.testng.annotations.Test;

public class AutomationCapabilitiesUnitTest {

    @Test
    public void capabilitySnapshotShouldExposeExplicitSupportWithoutLeakingBuilderMutations() {
        AutomationCapabilities.Builder builder = AutomationCapabilities.builder(AutomationBackend.SELENIUM_WEBDRIVER)
                .runtime("Chrome 140")
                .platform("Windows 11")
                .nativeFeature(AutomationFeature.NATIVE_DRIVER_ACCESS, "Selenium WebDriver")
                .adaptedFeature(AutomationFeature.TRACE, "SHAFT unified trace");

        AutomationCapabilities snapshot = builder.build();
        builder.nativeFeature(AutomationFeature.MOBILE_AUTOMATION, "late mutation");

        Assert.assertTrue(snapshot.supports(AutomationFeature.NATIVE_DRIVER_ACCESS));
        Assert.assertEquals(snapshot.supportOf(AutomationFeature.NATIVE_DRIVER_ACCESS), CapabilitySupport.NATIVE);
        Assert.assertEquals(snapshot.supportOf(AutomationFeature.TRACE), CapabilitySupport.ADAPTED);
        Assert.assertFalse(snapshot.supports(AutomationFeature.MOBILE_AUTOMATION));
        Assert.assertEquals(snapshot.supportOf(AutomationFeature.MOBILE_AUTOMATION), CapabilitySupport.UNSUPPORTED);
        Assert.assertSame(snapshot.require(AutomationFeature.TRACE), snapshot);
    }

    @Test
    public void unsupportedCapabilityShouldFailWithActionableBackendContext() {
        AutomationCapabilities snapshot = AutomationCapabilities.builder(AutomationBackend.APPIUM)
                .runtime("UiAutomator2")
                .platform("Android")
                .unsupportedFeature(
                        AutomationFeature.WEBAUTHN,
                        "The active Appium driver does not expose browser virtual authenticators.",
                        "Use getNativeDriver() with a platform extension when one is installed.")
                .build();

        UnsupportedAutomationFeatureException exception = Assert.expectThrows(
                UnsupportedAutomationFeatureException.class,
                () -> snapshot.require(AutomationFeature.WEBAUTHN));

        Assert.assertTrue(exception.getMessage().contains("WEBAUTHN"));
        Assert.assertTrue(exception.getMessage().contains("APPIUM"));
        Assert.assertTrue(exception.getMessage().contains("UiAutomator2"));
        Assert.assertTrue(exception.getMessage().contains("Android"));
        Assert.assertTrue(exception.getMessage().contains("getNativeDriver()"));
    }

    @Test
    public void unknownCapabilitySnapshotShouldFailClosed() {
        AutomationCapabilities snapshot = AutomationCapabilities.unknown("No active GUI backend");

        Assert.assertEquals(snapshot.backend(), AutomationBackend.UNKNOWN);
        Assert.assertFalse(snapshot.supports(AutomationFeature.BROWSER_AUTOMATION));
        Assert.assertEquals(snapshot.supportOf(AutomationFeature.BROWSER_AUTOMATION),
                CapabilitySupport.UNSUPPORTED);
        Assert.expectThrows(UnsupportedAutomationFeatureException.class,
                () -> snapshot.require(AutomationFeature.BROWSER_AUTOMATION));
    }
}
