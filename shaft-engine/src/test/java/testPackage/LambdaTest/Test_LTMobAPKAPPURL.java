package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTMobAPKAPPURL {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private final By actionBar = AppiumBy.accessibilityId("Action Bar");
    private final By displayOptions = AppiumBy.accessibilityId("Display Options");
    private final By displayShowCustom = AppiumBy.accessibilityId("DISPLAY_SHOW_CUSTOM");
    private final By app = AppiumBy.accessibilityId("App");

    @Test
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        driver.get().element().click(app);
        driver.get().element().click(actionBar);
        driver.get().element().click(displayOptions);
        driver.get().assertThat().element(displayShowCustom).text().isEqualTo("DISPLAY_SHOW_CUSTOM").perform();
    }


    @BeforeMethod
    public void setup() {
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);
        SHAFT.Properties.mobile.set().browserName("");
        LambdaTestCredentials.apply();
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().platformVersion("11");
        SHAFT.Properties.lambdaTest.set().deviceName("Poco X3 Pro");
        SHAFT.Properties.lambdaTest.set().appUrl(getUploadedAndroidAppUrl());
        SHAFT.Properties.mobile.set().browserName("");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    private static String getUploadedAndroidAppUrl() {
        String appUrl = firstNonBlank(
                System.getProperty("LambdaTest.androidAppUrl"),
                System.getenv("LAMBDATEST_ANDROID_APP_URL"));
        if (appUrl == null) {
            throw new SkipException("LambdaTest Android app URL is not configured for this run.");
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
