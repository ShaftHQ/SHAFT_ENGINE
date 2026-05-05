package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for BrowserStack properties defaults and thread-local overrides.
 */
public class BrowserStackPropertiesUnitTest {
    private static final int THREAD_JOIN_TIMEOUT_MS = 5000;

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "Validate BrowserStack default property values")
    public void testBrowserStackDefaults() {
        // BrowserStack credentials are bundled as shared non-production test defaults in this project; assert non-blank without embedding literal values.
        assertFalse(SHAFT.Properties.browserStack.userName().isBlank());
        assertFalse(SHAFT.Properties.browserStack.accessKey().isBlank());
        assertEquals(SHAFT.Properties.browserStack.platformVersion(), "");
        assertEquals(SHAFT.Properties.browserStack.deviceName(), "");
        assertEquals(SHAFT.Properties.browserStack.appUrl(), "");
        assertEquals(SHAFT.Properties.browserStack.customID(), "");
        assertEquals(SHAFT.Properties.browserStack.appName(), "");
        assertEquals(SHAFT.Properties.browserStack.appRelativeFilePath(), "");
        assertEquals(SHAFT.Properties.browserStack.osVersion(), "");
        assertEquals(SHAFT.Properties.browserStack.browserVersion(), "");
        assertFalse(SHAFT.Properties.browserStack.local());
        assertEquals(SHAFT.Properties.browserStack.seleniumVersion(), "4.40.0");
        assertTrue(SHAFT.Properties.browserStack.acceptInsecureCerts());
        assertFalse(SHAFT.Properties.browserStack.debug());
        assertFalse(SHAFT.Properties.browserStack.networkLogs());
        assertEquals(SHAFT.Properties.browserStack.geoLocation(), "");
        assertEquals(SHAFT.Properties.browserStack.appiumVersion(), "3.1.0");
        assertEquals(SHAFT.Properties.browserStack.buildName(), "");
        assertEquals(SHAFT.Properties.browserStack.projectName(), "");
        assertEquals(SHAFT.Properties.browserStack.parallelsPerPlatform(), 1);
        assertTrue(SHAFT.Properties.browserStack.browserstackAutomation());
        assertEquals(SHAFT.Properties.browserStack.platformsList(), "");
        assertEquals(SHAFT.Properties.browserStack.customBrowserStackYmlPath(), "");
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
            threadA.join(1000);
            threadB.join(1000);
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
