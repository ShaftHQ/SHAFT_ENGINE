package testPackage.appium;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class WindowsDesktopAppiumE2ETest {
    private DriverFactoryHelper helper;

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        if (helper != null) {
            helper.closeDriver();
            helper = null;
        }
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldCreateWindowsDesktopAppiumSession() {
        skipUnlessWindowsDesktopE2EIsEnabled();

        SHAFT.Properties.platform.set()
                .targetPlatform("Windows")
                .executionAddress(System.getProperty("executionAddress", "http://127.0.0.1:4723"));
        SHAFT.Properties.web.set()
                .targetBrowserName("WindowsApp")
                .headlessExecution(false);
        SHAFT.Properties.mobile.set()
                .browserName("")
                .automationName("Windows")
                .app(System.getProperty("mobile_app", "C:\\Windows\\System32\\notepad.exe"));
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(true);

        helper = new DriverFactory().getHelper(DriverFactory.DriverType.APPIUM_WINDOWS);

        SHAFT.Validations.assertThat().object(helper.getDriver()).isNotNull().perform();
        if (helper.getDriver() instanceof RemoteWebDriver remoteWebDriver) {
            SHAFT.Validations.assertThat().object(remoteWebDriver.getSessionId()).isNotNull().perform();
        }
    }

    private static void skipUnlessWindowsDesktopE2EIsEnabled() {
        if (!Boolean.getBoolean("runWindowsDesktopE2E")) {
            throw new SkipException("Windows desktop E2E is disabled. Set -DrunWindowsDesktopE2E=true to run it.");
        }
        if (!System.getProperty("os.name", "").toLowerCase().contains("windows")) {
            throw new SkipException("Windows desktop Appium E2E requires Windows.");
        }
    }
}
