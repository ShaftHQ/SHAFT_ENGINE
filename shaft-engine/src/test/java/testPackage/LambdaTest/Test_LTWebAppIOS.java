package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTWebAppIOS {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    SHAFT.TestData.JSON testData;

    private final By noThanks = By.xpath("//*[@resource-id='com.apple.mobilesafari:id/negative_button']");
    private final By appleMenuIcon = By.id("globalnav-menutrigger-button");
    private final By macTab = By.className("globalnav-link-mac");
    private final By elementToValidate = By.xpath("(//div[contains(@class,'globalnav-submenu-group-elevated')]//ul//li//a[@class='globalnav-submenu-link'])[7]");
    private final String exploreAllMacText = "Explore All Mac";
    private final String appleUrl = "https://www.apple.com/";

    @Test
    public void LT_Test_IOS_WebApp_V11() {
        driver.get().browser().navigateToURL(appleUrl);
        driver.get().element().click(appleMenuIcon);
        driver.get().element().click(macTab);
        driver.get().assertThat().element(elementToValidate).text().isEqualTo(exploreAllMacText).perform();
    }


    @BeforeMethod
    public void beforeMethod() {
        testData = new SHAFT.TestData.JSON("credentials.json");
        // common attributes
        SHAFT.Properties.lambdaTest.set().deviceName("iPhone 13 Pro Max");
        SHAFT.Properties.lambdaTest.set().platformVersion("15");
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.name());
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.web.set().targetBrowserName(Browser.SAFARI.browserName());
        SHAFT.Properties.mobile.set().browserName(Browser.SAFARI.browserName());
        SHAFT.Properties.lambdaTest.set().selenium_version("4.8.0");
        SHAFT.Properties.lambdaTest.set().isRealMobile(true);
        SHAFT.Properties.lambdaTest.set().username(testData.getTestData("LambdaTestUserName"));
        SHAFT.Properties.lambdaTest.set().accessKey(testData.getTestData("LambdaTestAccessKey"));
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}