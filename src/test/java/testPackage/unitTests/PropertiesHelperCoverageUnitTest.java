package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import org.apache.commons.lang3.SystemUtils;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.shaft.listeners.internal.TestNGListenerHelper;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

@Test(singleThreaded = true)
public class PropertiesHelperCoverageUnitTest {
    private boolean originalAutoMaximizeBrowserWindow;
    private String originalClearBeforeTypingMode;
    private boolean originalClickUsingJavascriptWhenWebDriverClickFails;
    private boolean originalEnableTrueNativeMode;
    private boolean originalForceCheckTextWasTypedCorrectly;
    private boolean originalRespectBuiltInWaitsInNativeMode;
    private boolean originalHandleNonSelectDropDown;
    private boolean originalValidateSwipeToElement;
    private String originalScrollingMode;
    private int originalMaximumPerformanceMode;
    private boolean originalTelemetryEnabled;

    @BeforeMethod(alwaysRun = true)
    public void setup() {
        PropertiesHelper.initialize();
        originalAutoMaximizeBrowserWindow = SHAFT.Properties.flags.autoMaximizeBrowserWindow();
        originalClearBeforeTypingMode = SHAFT.Properties.flags.clearBeforeTypingMode();
        originalClickUsingJavascriptWhenWebDriverClickFails = SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails();
        originalEnableTrueNativeMode = SHAFT.Properties.flags.enableTrueNativeMode();
        originalForceCheckTextWasTypedCorrectly = SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly();
        originalRespectBuiltInWaitsInNativeMode = SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode();
        originalHandleNonSelectDropDown = SHAFT.Properties.flags.handleNonSelectDropDown();
        originalValidateSwipeToElement = SHAFT.Properties.flags.validateSwipeToElement();
        originalScrollingMode = SHAFT.Properties.flags.scrollingMode();
        originalMaximumPerformanceMode = SHAFT.Properties.flags.maximumPerformanceMode();
        originalTelemetryEnabled = SHAFT.Properties.flags.telemetryEnabled();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(originalAutoMaximizeBrowserWindow);
        SHAFT.Properties.flags.set().clearBeforeTypingMode(originalClearBeforeTypingMode);
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(originalClickUsingJavascriptWhenWebDriverClickFails);
        SHAFT.Properties.flags.set().enableTrueNativeMode(originalEnableTrueNativeMode);
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(originalForceCheckTextWasTypedCorrectly);
        SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(originalRespectBuiltInWaitsInNativeMode);
        SHAFT.Properties.flags.set().handleNonSelectDropDown(originalHandleNonSelectDropDown);
        SHAFT.Properties.flags.set().validateSwipeToElement(originalValidateSwipeToElement);
        SHAFT.Properties.flags.set().scrollingMode(originalScrollingMode);
        SHAFT.Properties.flags.set().maximumPerformanceMode(originalMaximumPerformanceMode);
        SHAFT.Properties.flags.set().telemetryEnabled(originalTelemetryEnabled);
        Properties.clearForCurrentThread();
    }

    @Test
    public void initializeAndSetKeySystemPropertiesShouldLoadPropertyStateAndSystemKeys() {
        PropertiesHelper.initialize();
        PropertiesHelper.setKeySystemProperties();

        Assert.assertTrue(Properties.isInitialized(), "Properties should be marked as initialized.");
        Assert.assertNotNull(System.getProperty("rp.properties.path"));
        Assert.assertEquals(System.getProperty("webdriver.http.factory"), "jdk-http-client");
        Assert.assertNotNull(System.getProperty("log4j.configurationFile"));
        Assert.assertEquals(System.getProperty("allure.testng.hide.configuration.failures"), "true");
        Assert.assertEquals(System.getProperty("allure.testng.hide.disabled.tests"), "true");
    }

    @Test
    public void allAlwaysSetExternalLibPropertiesMustBeInSystemPropertiesAfterSetup() {
        Map<String, String> saved = new HashMap<>();
        PropertiesHelper.JVM_SYSTEM_PROPERTIES_ALWAYS.forEach(key -> {
            saved.put(key, System.getProperty(key));
            System.clearProperty(key);
        });
        try {
            PropertiesHelper.setKeySystemProperties();
            for (String key : PropertiesHelper.JVM_SYSTEM_PROPERTIES_ALWAYS) {
                Assert.assertNotNull(System.getProperty(key),
                        "Key '" + key + "' must be set in System properties by setKeySystemProperties()");
            }
        } finally {
            saved.forEach((key, value) -> {
                if (value == null) System.clearProperty(key);
                else System.setProperty(key, value);
            });
        }
    }

    @Test
    public void proxyExternalLibPropertiesMustBeInSystemPropertiesWhenProxyIsEnabled() {
        Map<String, String> saved = new HashMap<>();
        PropertiesHelper.JVM_SYSTEM_PROPERTIES_PROXY.forEach(key -> saved.put(key, System.getProperty(key)));
        try {
            SHAFT.Properties.platform.set().jvmProxySettings(true);
            SHAFT.Properties.platform.set().proxySettings("proxy.example.com:8080");
            TestNGListenerHelper.configureJVMProxy();
            for (String key : PropertiesHelper.JVM_SYSTEM_PROPERTIES_PROXY) {
                Assert.assertNotNull(System.getProperty(key),
                        "Key '" + key + "' must be set in System properties after configureJVMProxy() with proxy enabled");
            }
        } finally {
            saved.forEach((key, value) -> {
                if (value == null) System.clearProperty(key);
                else System.setProperty(key, value);
            });
            SHAFT.Properties.platform.set().jvmProxySettings(false);
            SHAFT.Properties.platform.set().proxySettings("");
        }
    }

    @Test
    public void initializeAiAgentShouldRunForceDownloadInitializationPath() {
        PropertiesHelper.initializeAiAgent();

        Assert.assertTrue(Properties.isInitialized(), "AI-agent initialization should load properties.");
        Assert.assertTrue(new File("target" + File.separator + "temp" + File.separator + "properties").exists(),
                "AI-agent initialization should create the temporary properties directory.");
    }

    @Test
    public void postProcessingShouldApplyMobileRemoteAnimatedGifAndSafariOverrides() throws Exception {
        resetPostProcessingGuard();
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.web.set().targetBrowserName("safari");
        SHAFT.Properties.visuals.set().createAnimatedGif(true);
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("fullPage");
        SHAFT.Properties.flags.set().clearBeforeTypingMode("native");
        SHAFT.Properties.flags.set().enableTrueNativeMode(false);
        SHAFT.Properties.flags.set().scrollingMode("javascript");

        PropertiesHelper.postProcessing();

        Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsScreenshotType(), "VIEWPORT");
        Assert.assertEquals(SHAFT.Properties.flags.clearBeforeTypingMode(), "off");
        Assert.assertFalse(SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails());
        Assert.assertTrue(SHAFT.Properties.flags.enableTrueNativeMode());
        Assert.assertFalse(SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly());
        Assert.assertFalse(SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode());
        Assert.assertFalse(SHAFT.Properties.flags.handleNonSelectDropDown());
        Assert.assertFalse(SHAFT.Properties.flags.validateSwipeToElement());
        Assert.assertEquals(SHAFT.Properties.flags.scrollingMode(), "w3c");
        Assert.assertFalse(SHAFT.Properties.flags.autoMaximizeBrowserWindow());
        Assert.assertEquals(SHAFT.Properties.mobile.platformName(), "android");
    }

    @Test
    public void postProcessingShouldApplyLocalOsAndSkipSecondRunWhenGuardIsSet() throws Exception {
        resetPostProcessingGuard();
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.platform.set().targetPlatform("android");

        PropertiesHelper.postProcessing();

        String expectedPlatform;
        if (SystemUtils.IS_OS_WINDOWS) {
            expectedPlatform = org.openqa.selenium.Platform.WINDOWS.toString();
        } else if (SystemUtils.IS_OS_MAC) {
            expectedPlatform = org.openqa.selenium.Platform.MAC.toString();
        } else {
            expectedPlatform = org.openqa.selenium.Platform.LINUX.toString();
        }
        Assert.assertEquals(SHAFT.Properties.platform.targetPlatform(), expectedPlatform);

        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(true);
        PropertiesHelper.postProcessing();

        Assert.assertTrue(SHAFT.Properties.flags.autoMaximizeBrowserWindow(),
                "Second postProcessing call should not re-apply overrides when guard is set.");
    }

    @Test
    public void maximumPerformanceModeTwoShouldEnableHeadlessForNonSafariOnly() throws Exception {
        SHAFT.Properties.flags.set().maximumPerformanceMode(2);

        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.web.set().headlessExecution(false);
        invokeMaximumPerformanceModeOverride();
        Assert.assertTrue(SHAFT.Properties.web.headlessExecution(),
                "Mode 2 should enable headless execution for non-Safari browsers.");

        SHAFT.Properties.web.set().targetBrowserName("safari");
        SHAFT.Properties.web.set().headlessExecution(false);
        invokeMaximumPerformanceModeOverride();
        Assert.assertFalse(SHAFT.Properties.web.headlessExecution(),
                "Mode 2 should keep headless execution disabled for Safari.");
    }

    @Test
    public void maximumPerformanceModeZeroShouldNotOverrideExistingHeadlessValue() throws Exception {
        SHAFT.Properties.flags.set().maximumPerformanceMode(0);
        SHAFT.Properties.web.set().headlessExecution(true);

        invokeMaximumPerformanceModeOverride();

        Assert.assertTrue(SHAFT.Properties.web.headlessExecution(),
                "Mode 0 should leave existing headless value unchanged.");
    }

    @Test
    public void setMobilePlatformShouldIgnoreNonMobileTargetOperatingSystems() {
        SHAFT.Properties.platform.set().targetPlatform("linux");
        SHAFT.Properties.mobile.set().platformName("customPlatform");

        PropertiesHelper.setMobilePlatform();

        Assert.assertEquals(SHAFT.Properties.mobile.platformName(), "customPlatform");
    }

    @Test
    public void postProcessingShouldSetViewportScreenshotTypeForParallelExecution() throws Exception {
        try {
            System.setProperty("setParallel", "METHODS");
            PropertiesHelper.initialize();
            resetPostProcessingGuard();
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("FULLPAGE");

            PropertiesHelper.postProcessing();

            Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsScreenshotType(), "VIEWPORT");
        } finally {
            System.clearProperty("setParallel");
        }
    }

    @Test
    public void postProcessingShouldSetViewportScreenshotTypeForMacPlatform() throws Exception {
        resetPostProcessingGuard();
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.platform.set().targetPlatform("MAC");
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("FULLPAGE");

        PropertiesHelper.postProcessing();

        Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsScreenshotType(), "VIEWPORT");
    }

    @Test
    public void postProcessingShouldHandleWindowsScreenScalingBranchWithoutThrowing() throws Exception {
        resetPostProcessingGuard();
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.platform.set().targetPlatform("WINDOWS");

        try {
            PropertiesHelper.postProcessing();
        } catch (Throwable throwable) {
            Assert.fail("postProcessing should not throw for WINDOWS branch.", throwable);
        }
    }

    private void invokeMaximumPerformanceModeOverride() throws Exception {
        Method method = PropertiesHelper.class.getDeclaredMethod("overridePropertiesForMaximumPerformanceMode");
        method.setAccessible(true);
        method.invoke(null);
    }

    private void resetPostProcessingGuard() throws Exception {
        Field field = PropertiesHelper.class.getDeclaredField("postProcessingDone");
        field.setAccessible(true);
        ((AtomicBoolean) field.get(null)).set(false);
    }
}
