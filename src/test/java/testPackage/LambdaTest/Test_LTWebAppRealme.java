package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTWebAppRealme {
    private final By noThanks = MobileBy.xpath("//*[@text='No thanks']");
    private final By noThanks1 = MobileBy.xpath("//*[@text='No, thanks']");
    private final By dismissChromAccountMessage = MobileBy.xpath("//*[@resource-id='com.android.chrome:id/signin_fre_dismiss_button']");
    private final By appleMenuIcon = By.id("globalnav-menutrigger-button");
    private final By macTab = By.className("globalnav-link-mac");
    private final By elementToValidate = By.xpath("(//div[contains(@class,'globalnav-submenu-group-elevated')]/ul[@class='globalnav-submenu-list']//li[@class='globalnav-submenu-list-item-elevated']//a[@class" + "='globalnav-submenu-link'])[7]");
    private final String exploreAllMacText = "Explore All Mac";
    private final String appleUrl = "https://www.apple.com/";
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;


    @Test
    public void LT_Test_Android_WebApp_V11() {
        checkPopUp();

        driver.browser().navigateToURL(appleUrl);
        driver.element().click(appleMenuIcon);
        driver.element().click(macTab);
        Assert.assertEquals(driver.element().getText(elementToValidate), exploreAllMacText);
    }


    public void checkPopUp() {
        int size1 = driver.getDriver().findElements(dismissChromAccountMessage).size();
        if (size1 != 0) {
            driver.element().click(dismissChromAccountMessage);
        }
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        int size = driver.getDriver().findElements(noThanks).size();
        int size2 = driver.getDriver().findElements(noThanks1).size();
        if (size != 0) {
            driver.element().click(noThanks);
        } else if (size2 != 0) {
            driver.element().click(noThanks1);
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        testData = new SHAFT.TestData.JSON("credentials.json");
        SHAFT.Properties.lambdaTest.set().deviceName("realme 7i");
        SHAFT.Properties.lambdaTest.set().platformVersion("10");
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.mobile.set().browserName("chrome");
        SHAFT.Properties.lambdaTest.set().selenium_version("4.8.0");
        SHAFT.Properties.lambdaTest.set().osVersion("11");
        SHAFT.Properties.lambdaTest.set().username(testData.getTestData("LambdaTestUserName"));
        SHAFT.Properties.lambdaTest.set().accessKey(testData.getTestData("LambdaTestAccessKey"));
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }
}