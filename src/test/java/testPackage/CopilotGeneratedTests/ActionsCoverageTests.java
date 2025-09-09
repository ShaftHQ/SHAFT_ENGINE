package testPackage.CopilotGeneratedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.Tests;

import java.util.Objects;

public class ActionsCoverageTests extends Tests {
    protected static final ThreadLocal<Actions> actions = new ThreadLocal<>();
    String testUrl = "https://duckduckgo.com/";
    By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchbox_input").build();

    @BeforeMethod
    public void setup() {
        actions.set(driver.get().element());
        driver.get().browser().navigateToURL(testUrl);
    }

    @Test
    public void testHoverOnLogo() {
        actions.get().hover(logo);
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testClickSearchBox() {
        actions.get().click(searchBox);
        driver.get().assertThat().element(searchBox).isVisible();
    }

    @Test
    public void testClearSearchBox() {
        actions.get().type(searchBox, "Test Clear");
        actions.get().clear(searchBox);
        driver.get().assertThat().element(searchBox).domProperty("value").isEqualTo("");
    }

    @Test
    public void testDragAndDropByOffset() {
        actions.get().dragAndDropByOffset(logo, 10, 10);
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testGetElementInformation() {
        driver.get().assertThat().element(logo).text().doesNotContain("error");
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testGetDomAttribute() {
        driver.get().assertThat().element(searchBox).attribute("type").isEqualTo(actions.get().get().domAttribute(searchBox, "type"));
    }

    @Test
    public void testGetCssValue() {
        driver.get().assertThat().element(logo).cssProperty("display").isEqualTo(actions.get().get().cssValue(logo, "display"));
    }

    @Test
    public void testIsEnabled() {
        boolean enabled = actions.get().get().isEnabled(logo);
        SHAFT.Validations.assertThat().object(enabled).isTrue();
    }

    @Test
    public void testIsNotSelected() {
        boolean selected = actions.get().get().isSelected(searchBox);
        SHAFT.Validations.assertThat().object(selected).isFalse();
    }

    @Test
    public void testClickAndHoldLogo() {
        actions.get().clickAndHold(logo);
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testDoubleClickLogo() {
        actions.get().doubleClick(logo);
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testClickUsingJavascriptSearchBox() {
        actions.get().clickUsingJavascript(searchBox);
        driver.get().assertThat().element(searchBox).isVisible();
    }

    @Test
    public void testSetValueUsingJavaScriptSearchBox() {
        actions.get().setValueUsingJavaScript(searchBox, "Copilot");
        driver.get().assertThat().element(searchBox).domProperty("value").isEqualTo("Copilot");
    }

    @Test
    public void testTypeSecureInSearchBox() {
        actions.get().typeSecure(searchBox, "secureText");
        driver.get().assertThat().element(searchBox).domProperty("value").contains("secureText");
    }

    @Test
    public void testTypeAppendInSearchBox() {
        actions.get().type(searchBox, "initial");
        actions.get().typeAppend(searchBox, "Appended");
        driver.get().assertThat().element(searchBox).domProperty("value").contains("Appended");
    }

    @Test
    public void testGetDomProperty() {
        driver.get().assertThat().element(searchBox).domProperty("value").isEqualTo(actions.get().get().domProperty(searchBox, "value"));
    }

    @Test
    public void testGetIsDisplayed() {
        boolean displayed = actions.get().get().isDisplayed(logo);
        SHAFT.Validations.assertThat().object(displayed).isTrue();
    }

    @Test
    public void testGetIsEnabled() {
        boolean enabled = actions.get().get().isEnabled(searchBox);
        SHAFT.Validations.assertThat().object(enabled).isTrue();
    }

    @Test
    public void testGetIsNotSelected() {
        boolean selected = actions.get().get().isSelected(searchBox);
        SHAFT.Validations.assertThat().object(selected).isFalse();
    }

    @Test
    public void testGetText() {
        String text = actions.get().get().text(logo);
        driver.get().assertThat().element(logo).text().isEqualTo(text);
    }

    @Test
    public void testWaitUntilTitleContainsDuckDuckGo() {
        actions.get().waitUntil(d -> Objects.requireNonNull(d.getTitle()).contains("DuckDuckGo"));
        driver.get().assertThat().browser().title().contains("DuckDuckGo");
    }

    @Test
    public void testWaitUntilWithTimeout() {
        actions.get().waitUntil(d -> Objects.requireNonNull(d.getTitle()).contains("DuckDuckGo"), java.time.Duration.ofSeconds(5));
        driver.get().assertThat().browser().title().contains("DuckDuckGo");
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testInvalidLocatorThrowsException() {
        actions.get().click(By.id("nonexistent-element"));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testDropFileToUploadFileNotFound() {
        actions.get().dropFileToUpload(searchBox, "C:/nonexistent/file.txt");
    }
}
