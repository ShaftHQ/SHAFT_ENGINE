package testPackage.locator;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class LocatorBuilderTest {

    SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://shafthq.github.io/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }

    @Test
    public void hasTagName() {

        By locator = SHAFT.GUI.Locator.hasTagName("h1").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsText() {
        By locator = Locator.hasTagName("h1").containsText("User Guide").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsAttribute() {
        By locator = Locator.hasTagName("h1").containsAttribute("class", "hero").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsClass() {
        By locator = Locator.hasTagName("h1").containsClass("hero").build();
        driver.assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsIdAndIsFirst() {
        By locator = Locator.hasTagName("*").containsId("_docusaurus").isFirst().build();
        driver.assertThat().element(locator).cssProperty("display").contains("flex").perform();
    }

    @Test
    public void hasAttributeAndContainsIdAndIsFirst() {
        By locator = Locator.hasTagName("*").hasAttribute("id").containsId("_docusaurus").isFirst().build();
        driver.assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasIdAndContainsId() {
        By locator = Locator.hasTagName("*").hasId("__docusaurus").build();
        driver.assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasAttributeWithValue() {
        By locator = SHAFT.GUI.Locator.hasAnyTagName()
                .and().hasAttribute("id", "__docusaurus")
                .build();
        driver.assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasClass() {
        By locator = Locator.hasTagName("h1").hasClass("hero__title").build();
        driver.assertThat().element(locator).text().isEqualTo("SHAFT User Guide").perform();
    }

    @Test
    public void hasText() {
        By locator = Locator.hasTagName("h1").hasText("SHAFT User Guide").build();
        driver.assertThat().element(locator).attribute("class").contains("hero__title").perform();
    }

    @Test
    public void hasIndex() {
        By locator = Locator.hasTagName("p").hasIndex(1).build();
        driver.assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void isFirst() {
        By locator = Locator.hasTagName("p").isFirst().build();
        driver.assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void isLast() {
        By locator = Locator.hasTagName("p").isLast().build();
        driver.assertThat().element(locator).text().contains("Test Automation").perform();
    }

    @Test
    public void relativeBy() {
        By locator = Locator.hasTagName("a").relativeBy().below(Locator.hasTagName("h1").hasText("SHAFT User Guide").build());
        driver.assertThat().element(locator).text().contains("Upgrade Now").perform();
    }

    @Test
    public void axisBy_followingSibling() {
        By locator = Locator.hasTagName("h1").hasText("SHAFT User Guide").axisBy().followingSibling("p").build();
        driver.assertThat().element(locator).text().contains("Stop reinventing the wheel.").perform();
    }

    @Test
    public void axisBy_chain_precedingSibling() {
        By locator = Locator.hasTagName("h1").hasText("SHAFT User Guide").axisBy().followingSibling("p").axisBy().precedingSibling("h1").build();
        driver.assertThat().element(locator).text().isEqualTo("SHAFT User Guide").perform();
    }
}
