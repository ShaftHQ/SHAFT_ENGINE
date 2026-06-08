package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class MobileEmulationTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    By searchBox = By.name("q");
    By resultStats = By.id("result-stats");

    @Test
    public void test_mobileEmulation_device() {
        SHAFT.Properties.web.set().isMobileEmulation(true);
        SHAFT.Properties.web.set().mobileEmulationIsCustomDevice(false);
        SHAFT.Properties.web.set().mobileEmulationDeviceName("Pixel 2");
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("data:text/html;charset=utf-8,<html><title>Mobile Fixture</title><body><input name='q'></body></html>");
        driver.get().verifyThat().browser().title().isEqualTo("Mobile Fixture").perform();
        driver.get().element().type(searchBox, "SHAFT_Engine").type(searchBox, Keys.ENTER);
        driver.get().assertThat().element(resultStats).doesNotExist().perform();
    }

    @Test
    public void test_mobileEmulation_customDevice() {
        SHAFT.Properties.web.set().isMobileEmulation(true);
        SHAFT.Properties.web.set().mobileEmulationIsCustomDevice(true);
        SHAFT.Properties.web.set().mobileEmulationWidth(660);
        SHAFT.Properties.web.set().mobileEmulationHeight(660);
        SHAFT.Properties.web.set().mobileEmulationPixelRatio(3.0);
        SHAFT.Properties.web.set().mobileEmulationUserAgent("Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0");
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("data:text/html;charset=utf-8,<html><body><input name='q'></body></html>");
        driver.get().element().type(searchBox, "SHAFT_Engine").type(searchBox, Keys.ENTER);
        driver.get().assertThat().element(resultStats).doesNotExist().perform();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        try {
            if (driver.get() != null) {
                driver.get().quit();
            }
        } finally {
            driver.remove();
            Properties.clearForCurrentThread();
        }
    }
}
