package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class MobileEmulationTests {
    SHAFT.GUI.WebDriver driver;

    By searchBox = GoogleSearch.getSearchBox_textField();
    By resultStats = By.id("result-stats");

    @Test
    public void test_mobileEmulation_device() {
        SHAFT.Properties.web.set().mobileEmulationIsCustomDevice(false);
        SHAFT.Properties.web.set().mobileEmulationDeviceName("Pixel 5");
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.verifyThat().browser().title().isEqualTo("Google").perform();
        driver.element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER);
        driver.assertThat().element(resultStats).doesNotExist().perform();
    }

    @Test
    public void test_mobileEmulation_customDevice() {
        SHAFT.Properties.web.set().mobileEmulationIsCustomDevice(true);
        SHAFT.Properties.web.set().mobileEmulationWidth(660);
        SHAFT.Properties.web.set().mobileEmulationHeight(660);
        SHAFT.Properties.web.set().mobileEmulationPixelRatio(3.0);
        SHAFT.Properties.web.set().mobileEmulationUserAgent("Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0");
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER);
        driver.assertThat().element(resultStats).doesNotExist().perform();
    }

    @BeforeClass
    public void beforeClass() {
        SHAFT.Properties.web.set().isMobileEmulation(true);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        SHAFT.Properties.web.set().isMobileEmulation(false);
    }
}
