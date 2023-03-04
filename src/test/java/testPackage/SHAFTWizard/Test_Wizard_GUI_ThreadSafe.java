package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import org.testng.asserts.SoftAssert;

public class Test_Wizard_GUI_ThreadSafe {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final ThreadLocal<SHAFT.TestData.JSON> testData = new ThreadLocal<>();

    private static final By searchBox = By.name("q");
    private static final By resultStats = By.id("result-stats");

    @Test
    public void test() {
        driver.get().browser().navigateToURL("https://www.google.com/");
        driver.get().verifyThat().browser().title().isEqualTo("Google").perform();
        driver.get().element().type(searchBox, testData.get().getTestData("searchQuery"))
                .keyPress(searchBox, Keys.ENTER);
        driver.get().assertThat().element(resultStats).text().doesNotEqual("").withCustomReportMessage("Check that result stats is not empty").perform();
    }

    @Test
    public void test_nativeDriver() {
        WebDriver nativeWebDriver = driver.get().getDriver();
        nativeWebDriver.navigate().to("https://www.google.com/");
        new SoftAssert().assertEquals(nativeWebDriver.getTitle(), "Google");
        nativeWebDriver.findElement(searchBox).sendKeys(testData.get().getTestData("searchQuery") + Keys.ENTER);
        Assert.assertNotEquals(nativeWebDriver.findElement(resultStats).getText(), "");
    }

    @BeforeClass
    public void beforeClass() {
        //BrowserStack Web
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.platform.set().targetPlatform(Platform.MAC.name());
//        SHAFT.Properties.web.set().targetBrowserName(Browser.SAFARI.browserName());
//        SHAFT.Properties.browserStack.set().browserVersion("15.3");
//        SHAFT.Properties.browserStack.set().osVersion("Monterey");

        driver.set(new SHAFT.GUI.WebDriver());
        testData.set(new SHAFT.TestData.JSON("simpleJSON.json"));
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        driver.get().quit();
        driver.remove();
    }
}
