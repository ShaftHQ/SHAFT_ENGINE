package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_ShadowDOM {
    WebDriver driver;

    @Test
    public void shadowdom() {
        String targetText = "Typing into SHADOW DOM...";
        BrowserActions.navigateToURL(driver, "https://mdn.github.io/web-components-examples/popup-info-box-web-component/");
        ElementActions.type(driver, By.id("cvc"), targetText);
        Assertions.assertElementAttribute(driver, By.id("cvc"), "Text", targetText);
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = BrowserFactory.getBrowser();
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
