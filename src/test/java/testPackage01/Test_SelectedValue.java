package testPackage01;

import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

public class Test_SelectedValue {
    String baseURL = "https://yari-demos.prod.mdn.mozit.cloud/en-US/docs/Web/HTML/Element/select/_sample_.";
    @Test
    public void simpleSelect() {
        WebDriver driver = BrowserFactory.getBrowser(DriverType.DESKTOP_CHROME);
        BrowserActions.navigateToURL(driver, baseURL+"Basic_select.html");
        By select = By.tagName("select");
        ElementActions.select(driver, select, "Third Value");
        ReportManager.log(ElementActions.getSelectedText(driver, select));
        Assertions.assertElementAttribute(driver, select, "selectedText", "Third Value");
        BrowserActions.closeCurrentWindow(driver);
    }

    @Test
    public void multipleSelect() {
        WebDriver driver = BrowserFactory.getBrowser(DriverType.DESKTOP_CHROME);
        BrowserActions.navigateToURL(driver, baseURL+"Advanced_select_with_multiple_features.html");
        By select = By.tagName("select");
        ElementActions.select(driver, select, "Dog");
        ElementActions.select(driver, select, "Cat");
        ReportManager.log(ElementActions.getSelectedText(driver, select));
        Assertions.assertElementAttribute(driver, select, "selectedText", "DogCat");
        BrowserActions.closeCurrentWindow(driver);
    }
}
