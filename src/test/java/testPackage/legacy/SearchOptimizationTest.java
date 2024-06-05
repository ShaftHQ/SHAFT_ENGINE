package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.LocatorBuilder;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test
public class SearchOptimizationTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    public static By searchBox = By.xpath("(//*[@type='search'])|(//*[@name='q'])");
    LocatorBuilder genericLink = SHAFT.GUI.Locator.hasTagName("a");

//    public void gitHub() {
//        if (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
//            driver.get().browser().navigateToURL("https://www.google.com/ncr")
//                    .and().element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER)
//                    .and().click(genericLink.containsAttribute("href", "SHAFT_ENGINE").isFirst().build())
//                    .and().assertThat(SHAFT.GUI.Locator.hasTagName("a").containsAttribute("href", "/ShaftHQ/SHAFT_ENGINE").isFirst().build()).text().contains("SHAFT_ENGINE")
//                    .withCustomReportMessage("repo title is SHAFT_Engine").perform();
//        }
//    }

    public void website() {
        if (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL("https://www.google.com/ncr")
                    .and().element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER)
                    .and().click(genericLink.hasAttribute("href", "https://shafthq.github.io/").isFirst().build())
                    .and().assertThat(SHAFT.GUI.Locator.hasTagName("h1").hasClass("hero__title").build()).text().isEqualTo("SHAFT: Unified Test Automation Engine")
                    .withCustomReportMessage("website header is SHAFT User Guide").perform();
        }
    }

    @BeforeMethod
    void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        driver.get().quit();
    }
}
