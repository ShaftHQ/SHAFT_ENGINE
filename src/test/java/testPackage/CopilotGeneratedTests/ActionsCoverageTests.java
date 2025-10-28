package testPackage.CopilotGeneratedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.Tests;

import java.util.Objects;

public class ActionsCoverageTests extends Tests {
    String testUrl = "https://duckduckgo.com/";
    By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchbox_input").build();

    @BeforeMethod
    public void internalSetup() {
        driver.get().browser().navigateToURL(testUrl);
    }

    @Test
    public void testHoverOnLogo() {
        driver.get().element().hover(logo)
                .assertThat(logo).isVisible();
    }

    @Test
    public void testClickSearchBox() {
        driver.get().element().click(searchBox)
                .assertThat(searchBox).isVisible();
    }

    @Test
    public void testClearSearchBox() {
        driver.get().element().type(searchBox, "Test Clear")
                .clear(searchBox)
                .assertThat(searchBox).domProperty("value").isEqualTo("");
    }

    @Test
    public void testDragAndDropByOffset() {
        driver.get().element().dragAndDropByOffset(logo, 10, 10)
                .assertThat(logo).isVisible();
    }

    @Test
    public void testGetElementInformation() {
        driver.get().assertThat().element(logo).text().doesNotContain("error");
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testGetDomAttribute() {
        driver.get().assertThat().element(searchBox).attribute("type").isEqualTo(driver.get().element().get().domAttribute(searchBox, "type"));
    }

    @Test
    public void testGetCssValue() {
        driver.get().assertThat().element(logo).cssProperty("display").isEqualTo(driver.get().element().get().cssValue(logo, "display"));
    }

    @Test
    public void testIsEnabled() {
        boolean enabled = driver.get().element().get().isEnabled(logo);
        SHAFT.Validations.assertThat().object(enabled).isTrue();
    }

    @Test
    public void testIsNotSelected() {
        boolean selected = driver.get().element().get().isSelected(searchBox);
        SHAFT.Validations.assertThat().object(selected).isFalse();
    }

    @Test
    public void testClickAndHoldLogo() {
        driver.get().element().clickAndHold(logo);
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testDoubleClickLogo() {
        driver.get().element().doubleClick(logo);
        driver.get().assertThat().element(logo).isVisible();
    }

    @Test
    public void testClickUsingJavascriptSearchBox() {
        driver.get().element().clickUsingJavascript(searchBox);
        driver.get().assertThat().element(searchBox).isVisible();
    }

    @Test
    public void testSetValueUsingJavaScriptSearchBox() {
        driver.get().element().setValueUsingJavaScript(searchBox, "Copilot");
        driver.get().assertThat().element(searchBox).domProperty("value").isEqualTo("Copilot");
    }

    @Test
    public void testTypeSecureInSearchBox() {
        driver.get().element().typeSecure(searchBox, "secureText");
        driver.get().assertThat().element(searchBox).domProperty("value").contains("secureText");
    }

    @Test
    public void testTypeAppendInSearchBox() {
        driver.get().element().type(searchBox, "initial");
        driver.get().element().typeAppend(searchBox, "Appended");
        driver.get().assertThat().element(searchBox).domProperty("value").contains("Appended");
    }

    @Test
    public void testGetDomProperty() {
        driver.get().assertThat().element(searchBox).domProperty("value").isEqualTo(driver.get().element().get().domProperty(searchBox, "value"));
    }

    @Test
    public void testGetIsDisplayed() {
        boolean displayed = driver.get().element().get().isDisplayed(logo);
        SHAFT.Validations.assertThat().object(displayed).isTrue();
    }

    @Test
    public void testGetIsEnabled() {
        boolean enabled = driver.get().element().get().isEnabled(searchBox);
        SHAFT.Validations.assertThat().object(enabled).isTrue();
    }

    @Test
    public void testGetIsNotSelected() {
        boolean selected = driver.get().element().get().isSelected(searchBox);
        SHAFT.Validations.assertThat().object(selected).isFalse();
    }

    @Test
    public void testGetText() {
        String text = driver.get().element().get().text(logo);
        driver.get().assertThat().element(logo).text().isEqualTo(text);
    }

    @Test
    public void testWaitUntilTitleContainsDuckDuckGo() {
        driver.get().element().waitUntil(d -> Objects.requireNonNull(d.getTitle()).contains("DuckDuckGo"));
        driver.get().assertThat().browser().title().contains("DuckDuckGo");
    }

    @Test
    public void testWaitUntilWithTimeout() {
        driver.get().element().waitUntil(d -> Objects.requireNonNull(d.getTitle()).contains("DuckDuckGo"), java.time.Duration.ofSeconds(5));
        driver.get().assertThat().browser().title().contains("DuckDuckGo");
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testInvalidLocatorThrowsException() {
        driver.get().element().click(By.id("nonexistent-element"));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testDropFileToUploadFileNotFound() {
        driver.get().element().dropFileToUpload(searchBox, "C:/nonexistent/file.txt");
    }
}
