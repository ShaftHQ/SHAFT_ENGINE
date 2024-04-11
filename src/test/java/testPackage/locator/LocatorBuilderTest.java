package testPackage.locator;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class LocatorBuilderTest {

    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }

    @Test
    public void hasTagName() {

        By locator = SHAFT.GUI.Locator.hasTagName("h1").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsText() {
        By locator = Locator.hasTagName("h1").containsText("Unified Test Automation Engine").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsAttribute() {
        By locator = Locator.hasTagName("h1").containsAttribute("class", "hero").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsClass() {
        By locator = Locator.hasTagName("h1").containsClass("hero").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsIdAndIsFirst() {
        By locator = Locator.hasTagName("*").containsId("_docusaurus").isFirst().build();
        driver.get().assertThat().element(locator).cssProperty("display").contains("flex").perform();
    }

    @Test
    public void hasAttributeAndContainsIdAndIsFirst() {
        By locator = Locator.hasTagName("*").hasAttribute("id").containsId("_docusaurus").isFirst().build();
        driver.get().assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasIdAndContainsId() {
        By locator = Locator.hasTagName("*").hasId("__docusaurus").build();
        driver.get().assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasAttributeWithValue() {
        By locator = SHAFT.GUI.Locator.hasAnyTagName()
                .and().hasAttribute("id", "__docusaurus")
                .build();
        driver.get().assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasClass() {
        By locator = Locator.hasTagName("h1").hasClass("hero__title").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void hasText() {
        By locator = Locator.hasTagName("h1").hasText("SHAFT: Unified Test Automation Engine").build();
        driver.get().assertThat().element(locator).attribute("class").contains("hero__title").perform();
    }

    @Test
    public void hasIndex() {
        By locator = Locator.hasTagName("p").hasIndex(1).build();
        driver.get().assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void isFirst() {
        By locator = Locator.hasTagName("p").isFirst().build();
        driver.get().assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void isLast() {
        By locator = Locator.hasTagName("p").isLast().build();
        driver.get().assertThat().element(locator).text().contains("What are you waiting for?").perform();
    }

    @Test
    public void relativeBy() {
        By locator = Locator.hasTagName("a").relativeBy().below(Locator.hasTagName("h1").containsText("SHAFT").build());
        driver.get().assertThat().element(locator).text().contains("Upgrade Now").perform();
    }

    @Test
    public void axisBy_followingSibling() {
        By locator = Locator.hasTagName("h1").containsText("SHAFT").axisBy().followingSibling("p").build();
        driver.get().assertThat().element(locator).text().contains("Stop reinventing the wheel").perform();
    }

    @Test
    public void axisBy_chain_precedingSibling() {
        By locator = Locator.hasTagName("h1").containsText("SHAFT").axisBy().followingSibling("p").axisBy().precedingSibling("h1").hasIndex(1).build();
        driver.get().assertThat().element(locator).text().isEqualTo("SHAFT: Unified Test Automation Engine").perform();
    }
}
