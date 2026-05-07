package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Coverage tests for LambdaTest fluent SetProperty API.
 */
public class LambdaTestSetPropertyCoverageUnitTest {
    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "Validate LambdaTest fluent setters write expected effective values")
    public void testLambdaTestSetPropertyFluentSetters() {
        SHAFT.Properties.lambdaTest.set()
                .username("lt-user")
                .accessKey("lt-key")
                .platformVersion("14")
                .osVersion("Windows 11")
                .appUrl("lt://app")
                .appProfiling(true)
                .deviceName("Pixel 9")
                .visual(true)
                .video(true)
                .resolution("2560x1440")
                .headless(true)
                .timezone("UTC")
                .project("SHAFT Project")
                .build("Build 42")
                .tunnel(true)
                .tunnelName("Tunnel-A")
                .buildName("BuildName-A")
                .autoGrantPermissions(false)
                .autoAcceptAlerts(false)
                .acceptInsecureCerts(false)
                .isRealMobile(false)
                .debug(true)
                .console(true)
                .selenium_version("4.41.0")
                .browserVersion("latest")
                .appiumVersion("3.3.0")
                .networkLogs(true)
                .appRelativeFilePath("apps/my-app.apk")
                .appName("my-app")
                .driver_version("129")
                .w3c(false)
                .geoLocation("FR")
                .customID("custom-123");

        assertEquals(SHAFT.Properties.lambdaTest.username(), "lt-user");
        assertEquals(SHAFT.Properties.lambdaTest.accessKey(), "lt-key");
        assertEquals(SHAFT.Properties.lambdaTest.platformVersion(), "2560x1440");
        assertEquals(SHAFT.Properties.lambdaTest.osVersion(), "Windows 11");
        assertEquals(SHAFT.Properties.lambdaTest.appUrl(), "lt://app");
        assertTrue(SHAFT.Properties.lambdaTest.appProfiling());
        assertEquals(SHAFT.Properties.lambdaTest.deviceName(), "Pixel 9");
        assertTrue(SHAFT.Properties.lambdaTest.visual());
        assertEquals(ThreadLocalPropertiesManager.getProperty("browserStack.video"), "true");
        assertEquals(SHAFT.Properties.lambdaTest.resolution(), "");
        assertTrue(SHAFT.Properties.lambdaTest.headless());
        assertEquals(SHAFT.Properties.lambdaTest.timezone(), "UTC");
        assertEquals(SHAFT.Properties.lambdaTest.project(), "SHAFT Project");
        assertEquals(SHAFT.Properties.lambdaTest.build(), "Build 42");
        assertTrue(SHAFT.Properties.lambdaTest.tunnel());
        assertEquals(SHAFT.Properties.lambdaTest.tunnelName(), "Tunnel-A");
        assertEquals(SHAFT.Properties.lambdaTest.buildName(), "BuildName-A");
        assertFalse(SHAFT.Properties.lambdaTest.autoGrantPermissions());
        assertFalse(SHAFT.Properties.lambdaTest.autoAcceptAlerts());
        assertFalse(SHAFT.Properties.lambdaTest.acceptInsecureCerts());
        assertFalse(SHAFT.Properties.lambdaTest.isRealMobile());
        assertTrue(SHAFT.Properties.lambdaTest.debug());
        assertTrue(SHAFT.Properties.lambdaTest.console());
        assertEquals(SHAFT.Properties.lambdaTest.selenium_version(), "4.41.0");
        assertEquals(SHAFT.Properties.lambdaTest.browserVersion(), "latest");
        assertEquals(SHAFT.Properties.lambdaTest.appiumVersion(), "3.3.0");
        assertTrue(SHAFT.Properties.lambdaTest.networkLogs());
        assertEquals(SHAFT.Properties.lambdaTest.appRelativeFilePath(), "apps/my-app.apk");
        assertEquals(SHAFT.Properties.lambdaTest.appName(), "my-app");
        assertEquals(SHAFT.Properties.lambdaTest.driver_version(), "129");
        assertFalse(SHAFT.Properties.lambdaTest.w3c());
        assertEquals(SHAFT.Properties.lambdaTest.geoLocation(), "FR");
        assertEquals(SHAFT.Properties.lambdaTest.customID(), "custom-123");
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
