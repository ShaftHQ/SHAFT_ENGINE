package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

public class Test_chainableElementActions {
    @Test
    public void chainElementActions() {
        WebDriver driver = BrowserFactory.getBrowser(BrowserFactory.BrowserType.GOOGLE_CHROME);
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        By searchBox = By.name("q");
        new ElementActions(driver).type(searchBox, "chained type 1").type(searchBox, "chained type 2").typeAppend(searchBox, "345");
        Assertions.assertElementAttribute(driver, searchBox, "text", "chained type 2345");
        BrowserActions.closeCurrentWindow(driver);
    }
}
