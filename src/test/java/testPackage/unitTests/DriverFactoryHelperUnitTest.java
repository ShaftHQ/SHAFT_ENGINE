package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.util.concurrent.atomic.AtomicReference;

public class DriverFactoryHelperUnitTest {
    private String savedExecutionAddress;
    private String savedTargetPlatform;
    private String savedMobileBrowserName;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (savedExecutionAddress != null) {
            SHAFT.Properties.platform.set().executionAddress(savedExecutionAddress);
            DriverFactoryHelper.initializeSystemProperties();
        }
        if (savedTargetPlatform != null) {
            SHAFT.Properties.platform.set().targetPlatform(savedTargetPlatform);
        }
        if (savedMobileBrowserName != null) {
            SHAFT.Properties.mobile.set().browserName(savedMobileBrowserName);
        }
    }

    @Test(description = "Launch screenshot attachment should only be enabled for native mobile automation")
    public void shouldAttachLaunchScreenshot_onlyForNativeMobileExecution() throws Exception {
        savedTargetPlatform = SHAFT.Properties.platform.targetPlatform();
        savedMobileBrowserName = SHAFT.Properties.mobile.browserName();

        Method shouldAttachLaunchScreenshotMethod = DriverFactoryHelper.class.getDeclaredMethod("shouldAttachLaunchScreenshot");
        shouldAttachLaunchScreenshotMethod.setAccessible(true);

        DriverFactoryHelper helper = new DriverFactoryHelper();

        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");
        boolean nativeMobileResult = (boolean) shouldAttachLaunchScreenshotMethod.invoke(helper);

        SHAFT.Properties.platform.set().targetPlatform("ios");
        SHAFT.Properties.mobile.set().browserName("");
        boolean iosNativeMobileResult = (boolean) shouldAttachLaunchScreenshotMethod.invoke(helper);

        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.mobile.set().browserName("chrome");
        boolean webResult = (boolean) shouldAttachLaunchScreenshotMethod.invoke(helper);

        SHAFT.Validations.assertThat().object(nativeMobileResult).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(iosNativeMobileResult).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(webResult).isEqualTo(false).perform();
    }

    @Test(description = "initializeSystemProperties should not append a trailing slash for browserstack keyword execution address")
    public void initializeSystemProperties_doesNotAppendTrailingSlashForBrowserStackKeyword() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("browserstack");

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl()).isEqualTo("http://browserstack").perform();
    }

    @Test(description = "initializeSystemProperties should keep explicit BrowserStack /wd/hub execution URL unchanged")
    public void initializeSystemProperties_keepsExplicitBrowserStackWdHubUrlUnchanged() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        String explicitBrowserStackUrl = "http://user:key@hub-cloud.browserstack.com/wd/hub";
        SHAFT.Properties.platform.set().executionAddress(explicitBrowserStackUrl);

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl()).isEqualTo(explicitBrowserStackUrl).perform();
    }

    @DataProvider(name = "remoteServerPingBaseUrls")
    public Object[][] remoteServerPingBaseUrls() {
        return new Object[][]{
                {"127.0.0.1:4723", "http://127.0.0.1:4723/"},
                {"http://127.0.0.1:4723", "http://127.0.0.1:4723/"},
                {"http://127.0.0.1:4723/wd/hub", "http://127.0.0.1:4723/wd/hub/"},
                {"https://127.0.0.1:4723", "https://127.0.0.1:4723/"},
                {"https://127.0.0.1:4723/wd/hub", "https://127.0.0.1:4723/wd/hub/"},
                {"://bad-url", "http://bad-url/"}
        };
    }

    @Test(description = "normalizeRemoteServerPingBaseUrl should normalize supported execution address formats for ping", dataProvider = "remoteServerPingBaseUrls")
    public void normalizeRemoteServerPingBaseUrl_handlesVariousFormats(String executionAddress, String expectedPingBaseUrl) throws Exception {
        Method normalizeRemoteServerPingBaseUrlMethod = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalizeRemoteServerPingBaseUrlMethod.setAccessible(true);

        String actualPingBaseUrl = (String) normalizeRemoteServerPingBaseUrlMethod.invoke(null, executionAddress);

        SHAFT.Validations.assertThat().object(actualPingBaseUrl).isEqualTo(expectedPingBaseUrl).perform();
    }

    @Test(description = "initializeSystemProperties should trim whitespace from execution address")
    public void initializeSystemProperties_trimsWhitespace() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("  http://localhost:4444  ");

        DriverFactoryHelper.initializeSystemProperties();

        String targetHubUrl = getTargetHubUrl();
        SHAFT.Validations.assertThat().object(targetHubUrl).isEqualTo("http://localhost:4444").perform();

        Method normalizeRemoteServerPingBaseUrlMethod = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalizeRemoteServerPingBaseUrlMethod.setAccessible(true);
        String normalizedPingBaseUrl = (String) normalizeRemoteServerPingBaseUrlMethod.invoke(null, targetHubUrl);

        SHAFT.Validations.assertThat().object(normalizedPingBaseUrl).isEqualTo("http://localhost:4444/").perform();
    }

    @Test(description = "initializeSystemProperties should preserve https scheme without corruption")
    public void initializeSystemProperties_preservesHttpsScheme() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("https://user:key@hub-cloud.browserstack.com/wd/hub");

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl())
                .isEqualTo("https://user:key@hub-cloud.browserstack.com/wd/hub").perform();
    }

    @Test(description = "redactUriCredentials should mask user-info in URL")
    public void redactUriCredentials_masksCredentials() throws Exception {
        Method redactMethod = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redactMethod.setAccessible(true);

        String redacted = (String) redactMethod.invoke(null, "https://myuser:mypass@hub-cloud.browserstack.com/wd/hub");

        SHAFT.Validations.assertThat().object(redacted)
                .isEqualTo("https://***:***@hub-cloud.browserstack.com/wd/hub").perform();
    }

    @Test(description = "redactUriCredentials should return URL unchanged when no credentials present")
    public void redactUriCredentials_noCredentials() throws Exception {
        Method redactMethod = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redactMethod.setAccessible(true);

        String result = (String) redactMethod.invoke(null, "http://localhost:4723/wd/hub");

        SHAFT.Validations.assertThat().object(result).isEqualTo("http://localhost:4723/wd/hub").perform();
    }

    @Test(description = "normalizeRemoteServerPingBaseUrl should throw MalformedURLException for an empty authority URL")
    public void normalizeRemoteServerPingBaseUrl_throwsForMalformedUrl() throws Exception {
        Method normalizeRemoteServerPingBaseUrlMethod = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalizeRemoteServerPingBaseUrlMethod.setAccessible(true);

        try {
            normalizeRemoteServerPingBaseUrlMethod.invoke(null, "http://");
            SHAFT.Validations.assertThat().object("no exception thrown").isEqualTo("MalformedURLException expected").perform();
        } catch (java.lang.reflect.InvocationTargetException e) {
            SHAFT.Validations.assertThat().object(e.getCause() instanceof MalformedURLException).isEqualTo(true).perform();
        }
    }

    @Test(description = "redactUriCredentials should return original input when URL is malformed")
    public void redactUriCredentials_malformedUrl() throws Exception {
        Method redactMethod = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redactMethod.setAccessible(true);

        String malformedUrl = "http://[invalid";
        String result = (String) redactMethod.invoke(null, malformedUrl);

        SHAFT.Validations.assertThat().object(result).isEqualTo(malformedUrl).perform();
    }

    @Test
    public void initEdgeDriverShouldNotLeakMirrorUrlToSubsequentBrowserInit() throws Exception {
        String savedMirrorUrl = System.getProperty("SE_DRIVER_MIRROR_URL");
        try {
            AtomicReference<String> duringEdge = new AtomicReference<>();

            Method initEdgeDriver = DriverFactoryHelper.class.getDeclaredMethod("initEdgeDriver", Runnable.class);
            initEdgeDriver.setAccessible(true);

            // Simulate Edge arm: initEdgeDriver sets SE_DRIVER_MIRROR_URL then runs the Runnable
            initEdgeDriver.invoke(null, (Runnable) () -> duringEdge.set(System.getProperty("SE_DRIVER_MIRROR_URL")));

            // Simulate Chrome arm: no setProperty call — Chrome just calls new ChromeDriver()
            // If SE_DRIVER_MIRROR_URL is still set here, Selenium Manager would pass it to the binary
            String afterEdge = System.getProperty("SE_DRIVER_MIRROR_URL");

            Assert.assertEquals(duringEdge.get(), "https://msedgedriver.microsoft.com",
                    "SE_DRIVER_MIRROR_URL must be set during Edge driver construction so Selenium Manager uses the correct CDN");
            Assert.assertNull(afterEdge,
                    "SE_DRIVER_MIRROR_URL must not be visible after Edge init — Chrome/Firefox Selenium Manager would otherwise use the Edge CDN URL");
        } finally {
            if (savedMirrorUrl == null) System.clearProperty("SE_DRIVER_MIRROR_URL");
            else System.setProperty("SE_DRIVER_MIRROR_URL", savedMirrorUrl);
        }
    }

    private static String getTargetHubUrl() throws NoSuchFieldException, IllegalAccessException {
        Field targetHubUrlField = DriverFactoryHelper.class.getDeclaredField("TARGET_HUB_URL");
        targetHubUrlField.setAccessible(true);
        return (String) targetHubUrlField.get(null);
    }
}
