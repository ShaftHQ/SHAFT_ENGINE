package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for BrowserStack properties defaults and thread-local overrides.
 */
public class BrowserStackPropertiesUnitTest {

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "Validate BrowserStack default property values")
    public void testBrowserStackDefaults() {
        Assert.assertFalse(SHAFT.Properties.browserStack.userName().isBlank());
        Assert.assertFalse(SHAFT.Properties.browserStack.accessKey().isBlank());
        Assert.assertEquals(SHAFT.Properties.browserStack.platformVersion(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.deviceName(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.appUrl(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.customID(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.appName(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.appRelativeFilePath(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.osVersion(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.browserVersion(), "");
        Assert.assertFalse(SHAFT.Properties.browserStack.local());
        Assert.assertEquals(SHAFT.Properties.browserStack.seleniumVersion(), "4.40.0");
        Assert.assertTrue(SHAFT.Properties.browserStack.acceptInsecureCerts());
        Assert.assertFalse(SHAFT.Properties.browserStack.debug());
        Assert.assertFalse(SHAFT.Properties.browserStack.networkLogs());
        Assert.assertEquals(SHAFT.Properties.browserStack.geoLocation(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.appiumVersion(), "3.1.0");
        Assert.assertEquals(SHAFT.Properties.browserStack.buildName(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.projectName(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.parallelsPerPlatform(), 1);
        Assert.assertTrue(SHAFT.Properties.browserStack.browserstackAutomation());
        Assert.assertEquals(SHAFT.Properties.browserStack.platformsList(), "");
        Assert.assertEquals(SHAFT.Properties.browserStack.customBrowserStackYmlPath(), "");
    }

    @Test(description = "Validate BrowserStack fluent setters update values and support chaining")
    public void testBrowserStackSetterChainingAndReads() {
        SHAFT.Properties.browserStack.set()
                .userName("user-1")
                .accessKey("key-1")
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

        Assert.assertEquals(SHAFT.Properties.browserStack.userName(), "user-1");
        Assert.assertEquals(SHAFT.Properties.browserStack.accessKey(), "key-1");
        Assert.assertEquals(SHAFT.Properties.browserStack.platformVersion(), "15");
        Assert.assertEquals(SHAFT.Properties.browserStack.deviceName(), "Pixel 9");
        Assert.assertEquals(SHAFT.Properties.browserStack.appUrl(), "bs://app-1");
        Assert.assertEquals(SHAFT.Properties.browserStack.customID(), "custom-1");
        Assert.assertEquals(SHAFT.Properties.browserStack.appName(), "my-app");
        Assert.assertEquals(SHAFT.Properties.browserStack.appRelativeFilePath(), "apps/my-app.apk");
        Assert.assertEquals(SHAFT.Properties.browserStack.osVersion(), "14");
        Assert.assertEquals(SHAFT.Properties.browserStack.browserVersion(), "latest-beta");
        Assert.assertTrue(SHAFT.Properties.browserStack.local());
        Assert.assertEquals(SHAFT.Properties.browserStack.seleniumVersion(), "4.41.0");
        Assert.assertEquals(SHAFT.Properties.browserStack.appiumVersion(), "3.2.0");
        Assert.assertFalse(SHAFT.Properties.browserStack.acceptInsecureCerts());
        Assert.assertTrue(SHAFT.Properties.browserStack.debug());
        Assert.assertTrue(SHAFT.Properties.browserStack.networkLogs());
        Assert.assertEquals(SHAFT.Properties.browserStack.geoLocation(), "FR");
        Assert.assertEquals(SHAFT.Properties.browserStack.buildName(), "build-1");
        Assert.assertEquals(SHAFT.Properties.browserStack.projectName(), "project-1");
        Assert.assertEquals(SHAFT.Properties.browserStack.parallelsPerPlatform(), 3);
        Assert.assertFalse(SHAFT.Properties.browserStack.browserstackAutomation());
        Assert.assertEquals(SHAFT.Properties.browserStack.platformsList(), "[{\"deviceName\":\"Pixel 9\"}]");
        Assert.assertEquals(SHAFT.Properties.browserStack.customBrowserStackYmlPath(), "config/browserstack.yml");
    }

    @Test(description = "Validate BrowserStack property overrides are isolated per thread")
    public void testBrowserStackThreadIsolation() throws InterruptedException {
        String defaultUserName = SHAFT.Properties.browserStack.userName();
        CountDownLatch threadASet = new CountDownLatch(1);
        CountDownLatch threadBRead = new CountDownLatch(1);
        AtomicReference<String> threadBObserved = new AtomicReference<>();

        Thread threadA = Thread.ofPlatform().start(() -> {
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

        Thread threadB = Thread.ofPlatform().start(() -> {
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

        threadA.join(5000);
        threadB.join(5000);

        Assert.assertFalse(threadA.isAlive(), "Thread A should complete");
        Assert.assertFalse(threadB.isAlive(), "Thread B should complete");
        Assert.assertEquals(threadBObserved.get(), defaultUserName,
                "Thread B should observe default value, not Thread A override");
    }

    @Test(description = "Validate clearForCurrentThread restores BrowserStack values to defaults")
    public void testClearForCurrentThreadRestoresDefaults() {
        String defaultUserName = SHAFT.Properties.browserStack.userName();
        boolean defaultLocal = SHAFT.Properties.browserStack.local();

        SHAFT.Properties.browserStack.set().userName("temp-user").local(!defaultLocal);
        Assert.assertEquals(SHAFT.Properties.browserStack.userName(), "temp-user");
        Assert.assertEquals(SHAFT.Properties.browserStack.local(), !defaultLocal);

        Properties.clearForCurrentThread();

        Assert.assertEquals(SHAFT.Properties.browserStack.userName(), defaultUserName);
        Assert.assertEquals(SHAFT.Properties.browserStack.local(), defaultLocal);
    }
}
