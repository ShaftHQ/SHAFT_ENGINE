package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Platform;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTMobIPAAppURL {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test() {
        new ElementActions(driver.get().getDriver()).performTouchAction().tap(AppiumBy.accessibilityId("Text Button"));
        new ElementActions(driver.get().getDriver()).type(AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com" + "\n");
        driver.get().assertThat().element(AppiumBy.accessibilityId("Text Output")).text().isEqualTo("hello@browserstack.com").perform();
    }

    @BeforeMethod
    public void setup() {
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().platformVersion("15");
        SHAFT.Properties.lambdaTest.set().deviceName("iPhone 13");
        SHAFT.Properties.lambdaTest.set().appUrl(getUploadedIosAppUrl());
        SHAFT.Properties.mobile.set().browserName("");
        LambdaTestCredentials.apply();
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        driver.set(new SHAFT.GUI.WebDriver());

    }

    private static String getUploadedIosAppUrl() {
        String appUrl = firstNonBlank(
                System.getProperty("LambdaTest.iosAppUrl"),
                System.getenv("LAMBDATEST_IOS_APP_URL"));
        if (appUrl == null) {
            throw new SkipException("LambdaTest iOS app URL is not configured for this run.");
        }
        return appUrl;
    }

    private static String firstNonBlank(String... candidates) {
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                return candidate;
            }
        }
        return null;
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
    }
}
