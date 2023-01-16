package testPackage01.locator;

import com.shaft.driver.SHAFT;
import io.github.shafthq.shaft.gui.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class LocatorBuilderTest {

    SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    public void afterMethod() {
        driver.quit();
    }

    @Test
    public void hasTagName() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = SHAFT.GUI.Locator.hasTagName("h1").build();
        driver.assertThat().element(locator).text().contains("Selenium").perform();
    }

    @Test
    public void containsText() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("h1").containsText("browsers").build();
        driver.assertThat().element(locator).text().contains("Selenium").perform();
    }

    @Test
    public void containsAttribute() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("h1").containsAttribute("class", "display-1").build();
        driver.assertThat().element(locator).text().contains("Selenium").perform();
    }

    @Test
    public void containsClass() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("h1").containsClass("display-1").build();
        driver.assertThat().element(locator).text().contains("Selenium").perform();
    }

    @Test
    public void containsId() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("*").containsId("_navbar").build();
        driver.assertThat().element(locator).attribute("class").contains("navbar-collapse").perform();
    }

    @Test
    public void hasAttributeAndContainsId() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("*").hasAttribute("id").containsId("_navbar").build();
        driver.assertThat().element(locator).attribute("class").contains("navbar-collapse").perform();
    }

    @Test
    public void hasIdAndContainsId() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("*").hasId("main_navbar").build();
        driver.assertThat().element(locator).attribute("class").contains("navbar-collapse").perform();
    }

    @Test
    public void hasAttributeWithValue() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = SHAFT.GUI.Locator.hasAnyTagName()
                .and().hasAttribute("id", "main_navbar")
                .build();
        driver.assertThat().element(locator).attribute("class").contains("navbar-collapse").perform();
    }

    @Test
    public void hasClass() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("h1").hasClass("display-1 mt-0 mt-md-5 pb-1").build();
        driver.assertThat().element(locator).text().isEqualTo("Selenium automates browsers. That's it!").perform();
    }

    @Test
    public void hasText() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("h1").hasText("Selenium automates browsers. That's it!").build();
        driver.assertThat().element(locator).attribute("class").contains("display-1").perform();
    }

    @Test
    public void hasIndex() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("p").hasIndex(1).build();
        driver.assertThat().element(locator).text().contains("What you do with that power").perform();
    }

    @Test
    public void isFirst() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("p").isFirst().build();
        driver.assertThat().element(locator).text().contains("What you do with that power").perform();
    }

    @Test
    public void isLast() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("p").isLast().build();
        driver.assertThat().element(locator).text().contains("About Selenium").perform();
    }

    @Test
    public void relativeBy() {
        driver.browser().navigateToURL("https://www.selenium.dev/");
        By locator = Locator.hasTagName("h2").relativeBy().below(Locator.hasTagName("h1").hasText("Selenium automates browsers. That's it!").build());
        driver.assertThat().element(locator).text().contains("Started").perform();
    }
}
