package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTMobAPKAPPURL {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    SHAFT.TestData.JSON testData;
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
        testData = new SHAFT.TestData.JSON("credentials.json");
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.lambdaTest.set().username(testData.getTestData("LambdaTestUserName"));
        SHAFT.Properties.lambdaTest.set().accessKey(testData.getTestData("LambdaTestAccessKey"));
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().platformVersion("11");
        SHAFT.Properties.lambdaTest.set().deviceName("Poco X3 Pro");
        SHAFT.Properties.lambdaTest.set().appUrl("lt://APP1016019381719168133998118");
        SHAFT.Properties.mobile.set().browserName("");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver != null) {
            driver.get().quit();
        }
    }
}