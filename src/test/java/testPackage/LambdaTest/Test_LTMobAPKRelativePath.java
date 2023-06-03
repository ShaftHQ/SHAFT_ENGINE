package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTMobAPKRelativePath {
    private SHAFT.GUI.WebDriver driver;
    private final By actionBar = AppiumBy.accessibilityId("Action Bar");
    private final By displayOptions = AppiumBy.accessibilityId("Display Options");
    private final By displayShowCustom = AppiumBy.accessibilityId("DISPLAY_SHOW_CUSTOM");
    private final By app = AppiumBy.accessibilityId("App");

    @Test
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        driver.element().click(app) ;
        driver.element().click(actionBar);
        driver.element().click(displayOptions);
        Assert.assertEquals(driver.element().getText(displayShowCustom),"DISPLAY_SHOW_CUSTOM");
    }


    @BeforeMethod
    public void setup() {
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);
        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().platformVersion("11");
        SHAFT.Properties.lambdaTest.set().deviceName("Galaxy A12");
        SHAFT.Properties.lambdaTest.set().appName("ApiDemos-debug.apk");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.lambdaTest.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.quit();
    }
}