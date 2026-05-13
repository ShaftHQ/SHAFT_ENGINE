package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for BrowserStack properties defaults and thread-local overrides.
 */
public class BrowserStackPropertiesUnitTest {
    private static final int THREAD_JOIN_TIMEOUT_MS = 5000;
    private static final int THREAD_INTERRUPT_TIMEOUT_MS = 1000;

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "Validate BrowserStack declared default property values")
    public void testBrowserStackDefaults() {
        Map<String, String> expectedDefaults = Map.ofEntries(
                Map.entry("platformVersion", ""),
                Map.entry("deviceName", ""),
                Map.entry("appUrl", ""),
                Map.entry("customID", ""),
                Map.entry("appName", ""),
                Map.entry("appRelativeFilePath", ""),
                Map.entry("osVersion", ""),
                Map.entry("browserVersion", ""),
                Map.entry("local", "false"),
                Map.entry("seleniumVersion", "4.40.0"),
                Map.entry("acceptInsecureCerts", "true"),
                Map.entry("debug", "false"),
                Map.entry("networkLogs", "false"),
                Map.entry("geoLocation", ""),
                Map.entry("appiumVersion", "3.1.0"),
                Map.entry("buildName", ""),
                Map.entry("projectName", ""),
                Map.entry("parallelsPerPlatform", "1"),
                Map.entry("browserstackAutomation", "true"),
                Map.entry("platformsList", ""),
                Map.entry("customBrowserStackYmlPath", "")
        );

        expectedDefaults.forEach((methodName, expectedDefault) -> assertEquals(getBrowserStackDefaultValue(methodName), expectedDefault));
        assertFalse(getBrowserStackDefaultValue("userName").isBlank());
        assertFalse(getBrowserStackDefaultValue("accessKey").isBlank());
    }

    @Test(description = "Validate BrowserStack fluent setters update values and support chaining")
    public void testBrowserStackSetterChainingAndReads() {
        SHAFT.Properties.browserStack.set()
                .userName("test-username")
                .accessKey("mock-access-key")
                .platformVersion("15")
                .deviceName("Pixel 9")
                .appUrl("bs://app-1")
                .customID("custom-1")
                .appName("my-app")
                .appRelativeFilePath("apps/my-app.apk")
                .osVersion("14")
                .browserVersion("latest-beta")
                .local(true)
                .seleniumVersion("4.41.0")
                .appiumVersion("3.2.0")
                .acceptInsecureCerts(false)
                .debug(true)
                .enableBiometric(true)
                .networkLogs(true)
                .geoLocation("FR")
                .buildName("build-1")
                .projectName("project-1")
                .parallelsPerPlatform(3)
                .browserstackAutomation(false)
                .platformsList("[{\"deviceName\":\"Pixel 9\"}]")
                .customBrowserStackYmlPath("config/browserstack.yml");

        assertEquals(SHAFT.Properties.browserStack.userName(), "test-username");
        assertEquals(SHAFT.Properties.browserStack.accessKey(), "mock-access-key");
        assertEquals(SHAFT.Properties.browserStack.platformVersion(), "15");
        assertEquals(SHAFT.Properties.browserStack.deviceName(), "Pixel 9");
        assertEquals(SHAFT.Properties.browserStack.appUrl(), "bs://app-1");
        assertEquals(SHAFT.Properties.browserStack.customID(), "custom-1");
        assertEquals(SHAFT.Properties.browserStack.appName(), "my-app");
        assertEquals(SHAFT.Properties.browserStack.appRelativeFilePath(), "apps/my-app.apk");
        assertEquals(SHAFT.Properties.browserStack.osVersion(), "14");
        assertEquals(SHAFT.Properties.browserStack.browserVersion(), "latest-beta");
        assertTrue(SHAFT.Properties.browserStack.local());
        assertEquals(SHAFT.Properties.browserStack.seleniumVersion(), "4.41.0");
        assertEquals(SHAFT.Properties.browserStack.appiumVersion(), "3.2.0");
        assertFalse(SHAFT.Properties.browserStack.acceptInsecureCerts());
        assertTrue(SHAFT.Properties.browserStack.debug());
        // BrowserStack exposes a setter for enableBiometric but no public getter; verify via thread-local effective value.
        assertEquals(ThreadLocalPropertiesManager.getProperty("browserStack.enableBiometric"), "true");
        assertTrue(SHAFT.Properties.browserStack.networkLogs());
        assertEquals(SHAFT.Properties.browserStack.geoLocation(), "FR");
        assertEquals(SHAFT.Properties.browserStack.buildName(), "build-1");
        assertEquals(SHAFT.Properties.browserStack.projectName(), "project-1");
        assertEquals(SHAFT.Properties.browserStack.parallelsPerPlatform(), 3);
        assertFalse(SHAFT.Properties.browserStack.browserstackAutomation());
        assertEquals(SHAFT.Properties.browserStack.platformsList(), "[{\"deviceName\":\"Pixel 9\"}]");
        assertEquals(SHAFT.Properties.browserStack.customBrowserStackYmlPath(), "config/browserstack.yml");
    }

    @Test(description = "Validate BrowserStack property overrides are isolated per thread")
    public void testBrowserStackThreadIsolation() throws InterruptedException {
        String defaultUserName = SHAFT.Properties.browserStack.userName();
        CountDownLatch threadASet = new CountDownLatch(1);
        CountDownLatch threadBRead = new CountDownLatch(1);
        AtomicReference<String> threadBObserved = new AtomicReference<>();

        // Platform threads are used here to keep deterministic join/teardown behavior in this isolation test.
        Thread threadA = Thread.ofPlatform().unstarted(() -> {
            try {
                SHAFT.Properties.browserStack.set().userName("thread-a-user");
                threadASet.countDown();
                threadBRead.await();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                Properties.clearForCurrentThread();
            }
        });

        Thread threadB = Thread.ofPlatform().unstarted(() -> {
            try {
                threadASet.await();
                threadBObserved.set(SHAFT.Properties.browserStack.userName());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                threadBRead.countDown();
                Properties.clearForCurrentThread();
            }
        });

        threadA.start();
        threadB.start();

        threadA.join(THREAD_JOIN_TIMEOUT_MS);
        threadB.join(THREAD_JOIN_TIMEOUT_MS);
        if (threadA.isAlive() || threadB.isAlive()) {
            threadA.interrupt();
            threadB.interrupt();
            try {
                threadA.join(THREAD_INTERRUPT_TIMEOUT_MS);
                threadB.join(THREAD_INTERRUPT_TIMEOUT_MS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("Interrupted while waiting for BrowserStack thread cleanup to complete.", e);
            }
            if (threadA.isAlive() || threadB.isAlive()) {
                throw new IllegalStateException("Timed out waiting for BrowserStack thread isolation test to complete.");
            }
        }
        assertEquals(threadBObserved.get(), defaultUserName);
    }

    @Test(description = "Validate clearForCurrentThread restores BrowserStack values to defaults")
    public void testClearForCurrentThreadRestoresDefaults() {
        String defaultUserName = SHAFT.Properties.browserStack.userName();
        boolean defaultLocal = SHAFT.Properties.browserStack.local();

        SHAFT.Properties.browserStack.set().userName("temp-user").local(!defaultLocal);
        assertEquals(SHAFT.Properties.browserStack.userName(), "temp-user");
        assertEquals(SHAFT.Properties.browserStack.local(), !defaultLocal);

        Properties.clearForCurrentThread();

        assertEquals(SHAFT.Properties.browserStack.userName(), defaultUserName);
        assertEquals(SHAFT.Properties.browserStack.local(), defaultLocal);
    }

    private String getBrowserStackDefaultValue(String methodName) {
        try {
            Method method = com.shaft.properties.internal.BrowserStack.class.getMethod(methodName);
            return method.getAnnotation(org.aeonbits.owner.Config.DefaultValue.class).value();
        } catch (NoSuchMethodException e) {
            throw new IllegalArgumentException("Unknown BrowserStack property method: " + methodName, e);
        }
    }

    private void assertEquals(Object actual, Object expected) {
        SHAFT.Validations.assertThat().object(actual).isEqualTo(expected).perform();
    }

    private void assertTrue(boolean actual) {
        SHAFT.Validations.assertThat().object(actual).isTrue().perform();
    }

    private void assertFalse(boolean actual) {
        SHAFT.Validations.assertThat().object(actual).isFalse().perform();
    }
}
