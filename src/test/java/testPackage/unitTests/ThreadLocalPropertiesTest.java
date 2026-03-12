package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertyFileManager;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Map;

/**
 * Unit tests to verify that SHAFT configuration properties are correctly
 * resolved through {@link ThreadLocalPropertiesManager} rather than
 * relying solely on {@code System.getProperties()}.
 */
public class ThreadLocalPropertiesTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        // Clear thread-local overrides after each test to avoid cross-test contamination
        Properties.clearForCurrentThread();
    }

    @Test(description = "getEffectiveProperties merges system and thread-local properties with thread-local winning")
    public void testGetEffectivePropertiesMergesCorrectly() {
        String key = "shaft.test.effective.merge";
        String threadLocalValue = "threadLocalWins";

        // Set via System property
        System.setProperty(key, "systemValue");
        try {
            // Set via thread-local (should override)
            ThreadLocalPropertiesManager.setProperty(key, threadLocalValue);

            java.util.Properties effective = ThreadLocalPropertiesManager.getEffectiveProperties();
            Assert.assertEquals(effective.getProperty(key), threadLocalValue,
                    "Thread-local override should take precedence over system property");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "getEffectiveProperties returns system properties when no thread-local override exists")
    public void testGetEffectivePropertiesFallsBackToSystem() {
        String key = "shaft.test.effective.fallback";
        String systemValue = "fromSystem";

        System.setProperty(key, systemValue);
        try {
            java.util.Properties effective = ThreadLocalPropertiesManager.getEffectiveProperties();
            Assert.assertEquals(effective.getProperty(key), systemValue,
                    "Should fall back to system property when no thread-local override exists");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "getAppiumDesiredCapabilities picks up thread-local mobile_ properties")
    public void testGetAppiumDesiredCapabilitiesReadsThreadLocal() {
        String testAppUrl = "https://example.com/test-thread-local.apk";

        // Set a mobile_ property via thread-local only (not in System properties)
        ThreadLocalPropertiesManager.setProperty("mobile_app", testAppUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();
        Assert.assertEquals(caps.get("mobile_app"), testAppUrl,
                "getAppiumDesiredCapabilities should read mobile_ properties from thread-local overrides");
    }

    @Test(description = "getCustomWebDriverDesiredCapabilities picks up thread-local capabilities.* properties")
    public void testGetCustomWebDriverDesiredCapabilitiesReadsThreadLocal() {
        String capKey = "capabilities.goog:loggingPrefs";
        String capValue = "{\"browser\":\"ALL\"}";

        ThreadLocalPropertiesManager.setProperty(capKey, capValue);

        var caps = PropertyFileManager.getCustomWebDriverDesiredCapabilities();
        Assert.assertEquals(caps.getCapability("goog:loggingPrefs"), capValue,
                "getCustomWebDriverDesiredCapabilities should read capabilities.* from thread-local overrides");
    }

    @Test(description = "getCustomBrowserstackCapabilities picks up thread-local browserStack.* properties")
    public void testGetCustomBrowserstackCapabilitiesReadsThreadLocal() {
        String capKey = "browserStack.sessionName";
        String capValue = "ThreadLocalTest";

        ThreadLocalPropertiesManager.setProperty(capKey, capValue);

        var caps = PropertyFileManager.getCustomBrowserstackCapabilities();
        Assert.assertEquals(caps.get("sessionName"), capValue,
                "getCustomBrowserstackCapabilities should read browserStack.* from thread-local overrides");
    }

    @Test(description = "API swagger validation enabled property is accessible through SHAFT.Properties.api")
    public void testApiSwaggerValidationPropertyAccessible() {
        // Verify the default value is false
        boolean enabled = SHAFT.Properties.api.swaggerValidationEnabled();
        Assert.assertFalse(enabled, "swagger.validation.enabled should default to false");

        // Set via the SHAFT property API (thread-local)
        SHAFT.Properties.api.set().swaggerValidationEnabled(true);
        Assert.assertTrue(SHAFT.Properties.api.swaggerValidationEnabled(),
                "swagger.validation.enabled should be true after thread-local set");
    }

    @Test(description = "Thread-local mobile_ properties do not leak to other threads")
    public void testMobilePropertiesThreadIsolation() throws InterruptedException {
        String testAppUrl = "https://example.com/thread-isolation-test.apk";

        // Set mobile_app in current thread's thread-local
        ThreadLocalPropertiesManager.setProperty("mobile_app", testAppUrl);

        // Verify current thread sees it
        Map<String, String> currentThreadCaps = PropertyFileManager.getAppiumDesiredCapabilities();
        Assert.assertEquals(currentThreadCaps.get("mobile_app"), testAppUrl,
                "Current thread should see its own thread-local mobile_app");

        // Verify another thread does NOT see it
        final Map<String, String>[] otherThreadCaps = new Map[]{null};
        Thread otherThread = new Thread(() -> {
            otherThreadCaps[0] = PropertyFileManager.getAppiumDesiredCapabilities();
        });
        otherThread.start();
        otherThread.join(5000);

        String otherThreadApp = otherThreadCaps[0] != null ? otherThreadCaps[0].get("mobile_app") : null;
        Assert.assertNotEquals(otherThreadApp, testAppUrl,
                "Other thread should NOT see the current thread's thread-local mobile_app override");
    }
}
