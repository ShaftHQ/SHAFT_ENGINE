package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_MobileEmulation {
    SHAFT.GUI.WebDriver driver;

    By searchBox = By.name("q");
    By resultStats = By.id("result-stats");

    @Test
    public void test_mobileEmulation_device() {
        System.setProperty("mobileEmulation.isCustomDevice", "false");
        System.setProperty("mobileEmulation.deviceName", "Pixel 5");
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.verifyThat().browser().title().isEqualTo("Google").perform();
        driver.element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER);
        driver.assertThat().element(resultStats).doesNotExist().perform();
    }

    @Test
    public void test_mobileEmulation_customDevice() {
        System.setProperty("mobileEmulation.isCustomDevice", "true");
        System.setProperty("mobileEmulation.width", "660");
        System.setProperty("mobileEmulation.height", "660");
        System.setProperty("mobileEmulation.pixelRatio", "3.0");
        System.setProperty("mobileEmulation.userAgent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0");
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER);
        driver.assertThat().element(resultStats).doesNotExist().perform();
    }

    @BeforeClass
    public void beforeClass() {
        System.setProperty("isMobileEmulation", "true");
    }

    @AfterMethod
    public void afterClass() {
        driver.quit();
    }
}
