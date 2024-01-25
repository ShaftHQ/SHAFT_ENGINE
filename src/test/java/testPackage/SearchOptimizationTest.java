package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.LocatorBuilder;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test
public class SearchOptimizationTest {
    SHAFT.GUI.WebDriver driver;
    By searchBox = SHAFT.GUI.Locator.hasAnyTagName().hasAttribute("type", "search").build();
    LocatorBuilder genericLink = SHAFT.GUI.Locator.hasTagName("a");

    public void gitHub() {
        driver.browser().navigateToURL("https://www.google.com/ncr")
                .and().element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER)
                .and().click(genericLink.hasAttribute("href", "https://github.com/ShaftHQ/SHAFT_ENGINE").isFirst().build())
                .and().assertThat(SHAFT.GUI.Locator.hasTagName("a").containsAttribute("href", "/ShaftHQ/SHAFT_ENGINE").isFirst().build()).text().contains("SHAFT_ENGINE")
                .withCustomReportMessage("repo title is SHAFT_Engine").perform();
    }

    public void website() {
        driver.browser().navigateToURL("https://www.google.com/ncr")
                .and().element().type(searchBox, "SHAFT_Engine").keyPress(searchBox, Keys.ENTER)
                .and().click(genericLink.hasAttribute("href", "https://shafthq.github.io/").isFirst().build())
                .and().assertThat(SHAFT.GUI.Locator.hasTagName("h1").hasClass("hero__title").build()).text().isEqualTo("SHAFT User Guide")
                .withCustomReportMessage("website header is SHAFT User Guide").perform();
    }

    @BeforeMethod
    void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    void afterMethod() {
        driver.quit();
    }
}
