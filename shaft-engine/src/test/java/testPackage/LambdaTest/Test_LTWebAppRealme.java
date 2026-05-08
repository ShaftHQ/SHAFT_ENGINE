package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_LTWebAppRealme {
    private final By noThanks = By.xpath("//*[@text='No thanks']");
    private final By noThanks1 = By.xpath("//*[@text='No, thanks']");
    private final By dismissChromAccountMessage = By.xpath("//*[@resource-id='com.android.chrome:id/signin_fre_dismiss_button']");
    private final By appleMenuIcon = By.id("globalnav-menutrigger-button");
    private final By macTab = By.className("globalnav-link-mac");
    private final By elementToValidate = By.xpath("(//div[contains(@class,'globalnav-submenu-group-elevated')]/ul[@class='globalnav-submenu-list']//li[@class='globalnav-submenu-list-item-elevated']//a[@class" + "='globalnav-submenu-link'])[7]");
    private final String exploreAllMacText = "Explore All Mac";
    private final String appleUrl = "https://www.apple.com/";
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    SHAFT.TestData.JSON testData;


    @Test
    public void LT_Test_Android_WebApp_V11() {
        checkPopUp();
        driver.get().browser().navigateToURL(appleUrl);
        driver.get().element().click(appleMenuIcon);
        driver.get().element().click(macTab);
        driver.get().assertThat().element(elementToValidate).text().isEqualTo(exploreAllMacText).perform();
    }


    public void checkPopUp() {
        int size1 = driver.get().getDriver().findElements(dismissChromAccountMessage).size();
        if (size1 != 0) {
            driver.get().element().click(dismissChromAccountMessage);
        }
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        int size = driver.get().getDriver().findElements(noThanks).size();
        int size2 = driver.get().getDriver().findElements(noThanks1).size();
        if (size != 0) {
            driver.get().element().click(noThanks);
        } else if (size2 != 0) {
            driver.get().element().click(noThanks1);
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        testData = new SHAFT.TestData.JSON("credentials.json");
        // common attributes
        SHAFT.Properties.lambdaTest.set().deviceName("Pixel 7");
        SHAFT.Properties.lambdaTest.set().platformVersion("13");
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.web.set().targetBrowserName(Browser.CHROME.browserName());
        SHAFT.Properties.mobile.set().browserName(Browser.CHROME.browserName());
        SHAFT.Properties.lambdaTest.set().selenium_version("4.8.0");
        SHAFT.Properties.lambdaTest.set().osVersion("11");
        SHAFT.Properties.lambdaTest.set().username(testData.getTestData("LambdaTestUserName"));
        SHAFT.Properties.lambdaTest.set().accessKey(testData.getTestData("LambdaTestAccessKey"));
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}