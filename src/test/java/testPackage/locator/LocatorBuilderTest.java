package testPackage.locator;

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

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }

    @Test
    public void hasTagName() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = SHAFT.GUI.Locator.hasTagName("h1").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsText() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("h1").containsText("User Guide").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsAttribute() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("h1").containsAttribute("class", "hero").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsClass() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("h1").containsClass("hero").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsId() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("*").containsId("_docusaurus").build();
        driver.assertThat().element(locator).cssProperty("display").contains("flex").perform();
    }

    @Test
    public void hasAttributeAndContainsId() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("*").hasAttribute("id").containsId("_docusaurus").build();
        driver.assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasIdAndContainsId() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("*").hasId("__docusaurus").build();
        driver.assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasAttributeWithValue() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = SHAFT.GUI.Locator.hasAnyTagName()
                .and().hasAttribute("id", "__docusaurus")
                .build();
        driver.assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasClass() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("h1").hasClass("hero__title").build();
        driver.assertThat().element(locator).text().isEqualTo("SHAFT User Guide").perform();
    }

    @Test
    public void hasText() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("h1").hasText("SHAFT User Guide").build();
        driver.assertThat().element(locator).attribute("class").contains("hero__title").perform();
    }

    @Test
    public void hasIndex() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("p").hasIndex(1).build();
        driver.assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void isFirst() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("p").isFirst().build();
        driver.assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void isLast() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("p").isLast().build();
        driver.assertThat().element(locator).text().contains("Are you still here?").perform();
    }

    @Test
    public void relativeBy() {
        driver.browser().navigateToURL("https://shafthq.github.io/SHAFT_Engine_Docusaurus/");
        By locator = Locator.hasTagName("a").relativeBy().below(Locator.hasTagName("h1").hasText("SHAFT User Guide").build());
        driver.assertThat().element(locator).text().contains("Upgrade Now").perform();
    }
}
