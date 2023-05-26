
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.Assert;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

public class LTAppiumWebAndroid {
    static RemoteWebDriver driver = null;
    String username = "magdy.heibavodafone";
    String accesskey = "pA1PmVOfkQ5gKfbjk4Heh7Jo4Ly7SUslr2JCcUCCXPYKrZRBB8";
    String gridURL = "@mobile-hub.lambdatest.com/wd/hub";
    private final By noThanks = MobileBy.xpath("//*[@text='No thanks']");
    private final By noThanks1 = MobileBy.xpath("//*[@text='No, thanks']");
    private final By dismissChromAccountMessage = MobileBy.xpath("//*[@resource-id='com.android.chrome:id/signin_fre_dismiss_button']");
    private final By appleMenuIcon = By.id("globalnav-menutrigger-button");
    private final By macTab = By.className("globalnav-link-mac");
    private final By elementToValidate = By.xpath("(//div[contains(@class,'globalnav-submenu-group-elevated')]/ul[@class='globalnav-submenu-list']//li[@class='globalnav-submenu-list-item-elevated']//a[@class" + "='globalnav-submenu-link'])[7]");
    private final String exploreAllMacText = "Explore All Mac";
    private final String appleUrl = "https://www.apple.com/";
    DesiredCapabilities capabilities = new DesiredCapabilities();


    //adb shell dumpsys window | find "mCurrentFocus"
    @Test
    public void LT_Test_Android_WebApp_V11() {
        String deviceName = "Galaxy A12";
        String version = "11";
        setUp(deviceName, version);
        checkPopUp();

        driver.navigate().to(appleUrl);
        driver.findElement(appleMenuIcon).click();
        driver.findElement(macTab).click();
        Assert.assertEquals(driver.findElement(elementToValidate).getText(), exploreAllMacText);
    }


    public void checkPopUp() {
        int size1 = driver.findElements(dismissChromAccountMessage).size();
        if (size1 != 0) {
            driver.findElement(dismissChromAccountMessage).click();
        }
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        int size = driver.findElements(noThanks).size();
        int size2 = driver.findElements(noThanks1).size();
        if (size != 0) {
            driver.findElement(noThanks).click();
        } else if (size2 != 0) {
            driver.findElement(noThanks1).click();
        }
    }

    public void setUp(String deviceName, String version) {
        HashMap<String, Object> ltOptions = new HashMap<String, Object>();
        ltOptions.put("name", "Test " + deviceName + " V" + version);
        ltOptions.put("w3c", true);
        ltOptions.put("platformName", "android");
        ltOptions.put("deviceName", deviceName);
        ltOptions.put("platformVersion", version);
        ltOptions.put("isRealMobile", true);
        ltOptions.put("network", true);
        ltOptions.put("appProfiling", true);
        capabilities.setCapability("lt:options", ltOptions);

        try {
            driver = new RemoteWebDriver(new URL("https://" + username + ":" + accesskey + gridURL), capabilities);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }

    @AfterMethod
    public void tearDown(ITestResult result, Method method) {
        if (result.getStatus() == ITestResult.SUCCESS) {
            driver.executeScript("lambda-status=passed");
        } else if (result.getStatus() == ITestResult.FAILURE) {
            driver.executeScript("lambda-status=failed");
        } else if (result.getStatus() == ITestResult.SKIP) {
            driver.executeScript("lambda-status=skipped");
        }
        driver.quit();
    }
}