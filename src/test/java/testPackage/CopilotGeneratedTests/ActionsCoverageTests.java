package testPackage.CopilotGeneratedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ActionsCoverageTests {
    SHAFT.GUI.WebDriver driver;
    Actions actions;
    String testUrl = "https://duckduckgo.com/";
    By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchbox_input").build();
    By firstResult = Locator.hasTagName("article").isFirst().build();

    @BeforeMethod
    public void setup() {
        driver = new SHAFT.GUI.WebDriver();
        actions = driver.element();
        driver.browser().navigateToURL(testUrl);
    }

    @Test
    public void testHoverOnLogo() {
        actions.hover(logo);
        driver.assertThat().element(logo).isVisible();
    }

    @Test
    public void testClickSearchBox() {
        actions.click(searchBox);
        driver.assertThat().element(searchBox).isVisible();
    }

    @Test
    public void testClearSearchBox() {
        actions.type(searchBox, "Test Clear");
        actions.clear(searchBox);
        driver.assertThat().element(searchBox).domProperty("value").isEqualTo("");
    }

    @Test
    public void testDragAndDropByOffset() {
        actions.dragAndDropByOffset(logo, 10, 10);
        driver.assertThat().element(logo).isVisible();
    }

    @Test
    public void testGetElementInformation() {
        driver.assertThat().element(logo).text().doesNotContain("error");
        driver.assertThat().element(logo).isVisible();
    }

    @Test
    public void testGetDomAttribute() {
        driver.assertThat().element(searchBox).attribute("type").isEqualTo(actions.get().domAttribute(searchBox, "type"));
    }

    @Test
    public void testGetCssValue() {
        driver.assertThat().element(logo).cssProperty("display").isEqualTo(actions.get().cssValue(logo, "display"));
    }

    @Test
    public void testIsEnabled() {
        boolean enabled = actions.get().isEnabled(logo);
        driver.assertThat().element(logo).isEnabled();
    }

    @Test
    public void testIsSelected() {
        boolean selected = actions.get().isSelected(searchBox);
        driver.assertThat().element(searchBox).isNotSelected();
    }

    @Test
    public void testClickAndHoldLogo() {
        actions.clickAndHold(logo);
        driver.assertThat().element(logo).isVisible();
    }

    @Test
    public void testDoubleClickLogo() {
        actions.doubleClick(logo);
        driver.assertThat().element(logo).isVisible();
    }

    @Test
    public void testClickUsingJavascriptSearchBox() {
        actions.clickUsingJavascript(searchBox);
        driver.assertThat().element(searchBox).isVisible();
    }

    @Test
    public void testSetValueUsingJavaScriptSearchBox() {
        actions.setValueUsingJavaScript(searchBox, "Copilot");
        driver.assertThat().element(searchBox).domProperty("value").isEqualTo("Copilot");
    }

    @Test
    public void testTypeSecureInSearchBox() {
        actions.typeSecure(searchBox, "secureText");
        driver.assertThat().element(searchBox).domProperty("value").contains("secureText");
    }

    @Test
    public void testTypeAppendInSearchBox() {
        actions.type(searchBox, "initial");
        actions.typeAppend(searchBox, "Appended");
        driver.assertThat().element(searchBox).domProperty("value").contains("Appended");
    }

    @Test
    public void testGetDomProperty() {
        driver.assertThat().element(searchBox).domProperty("value").isEqualTo(actions.get().domProperty(searchBox, "value"));
    }

    @Test
    public void testGetIsDisplayed() {
        boolean displayed = actions.get().isDisplayed(logo);
        driver.assertThat().element(logo).isVisible();
    }

    @Test
    public void testGetIsEnabled() {
        boolean enabled = actions.get().isEnabled(searchBox);
        driver.assertThat().element(searchBox).isEnabled();
    }

    @Test
    public void testGetIsSelected() {
        boolean selected = actions.get().isSelected(searchBox);
        driver.assertThat().element(searchBox).isNotSelected();
    }

    @Test
    public void testGetText() {
        String text = actions.get().text(logo);
        driver.assertThat().element(logo).text().isEqualTo(text);
    }

    @Test
    public void testWaitUntilTitleContainsDuckDuckGo() {
        actions.waitUntil(d -> d.getTitle().contains("DuckDuckGo"));
        driver.assertThat().browser().title().contains("DuckDuckGo");
    }

    @Test
    public void testWaitUntilWithTimeout() {
        actions.waitUntil(d -> d.getTitle().contains("DuckDuckGo"), java.time.Duration.ofSeconds(5));
        driver.assertThat().browser().title().contains("DuckDuckGo");
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testInvalidLocatorThrowsException() {
        actions.click(By.id("nonexistent-element"));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testDropFileToUploadFileNotFound() {
        actions.dropFileToUpload(searchBox, "C:/nonexistent/file.txt");
    }

    @AfterMethod
    public void teardown() {
        driver.quit();
    }
}
