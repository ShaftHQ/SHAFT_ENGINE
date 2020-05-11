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
        BrowserActions.navigateToURL(driver, "https://jqueryui.com/droppable/");
        new ElementActions(driver).type(By.name("s"), "chained type 1").type(By.name("s"), "chained type 2");
        Assertions.assertElementAttribute(driver, By.name("s"), "text", "chained type 2");
        BrowserActions.closeCurrentWindow(driver);
    }
}
