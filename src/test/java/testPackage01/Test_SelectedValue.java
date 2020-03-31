package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_SelectedValue {
    @Test
    public void simpleSelect(){
        WebDriver driver = BrowserFactory.getBrowser(BrowserFactory.BrowserType.GOOGLE_CHROME);
        BrowserActions.navigateToURL(driver, "https://mdn.mozillademos.org/en-US/docs/Web/HTML/Element/select$samples/Basic_select?revision=1612304");
        By select = By.tagName("select");
        ElementActions.select(driver, select, "Third Value");
        ReportManager.log(ElementActions.getSelectedText(driver, select));
        Assertions.assertElementAttribute(driver, select, "selectedText", "Third Value");
    }

    @Test
    public void multipleSelect(){
        WebDriver driver = BrowserFactory.getBrowser(BrowserFactory.BrowserType.GOOGLE_CHROME);
        BrowserActions.navigateToURL(driver, "https://mdn.mozillademos.org/en-US/docs/Web/HTML/Element/select$samples/Advanced_select_with_multiple_features?revision=1612304");
        By select = By.tagName("select");
        ElementActions.select(driver, select, "Dog");
        ElementActions.select(driver, select, "Cat");
        ReportManager.log(ElementActions.getSelectedText(driver, select));
        Assertions.assertElementAttribute(driver, select, "selectedText", "DogCat");
    }
}
