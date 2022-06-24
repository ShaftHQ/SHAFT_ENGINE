package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidSelectorException;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_SwipeElementIntoView {
    WebDriver driver;

    @Test(priority = 1)
    public void SwipeElement1Successfully() {
        By Element1 =By.id("search_form_input_homepage");
        ElementActions.performTouchAction(driver).swipeElementIntoView(Element1, TouchActions.SwipeDirection.DOWN);
    }

    @Test(priority = 2)
    public void SwipeElement2Failled() {
        By Element2=By.xpath("");

        try {
            ElementActions.performTouchAction(driver).swipeElementIntoView(Element2, TouchActions.SwipeDirection.DOWN);
        }
        catch (AssertionError e){
            // pass
        }
    }

    @Test(priority = 3)
    public void SwipeElement3Failled() {
        By Element3=null;

        try {
            ElementActions.performTouchAction(driver).swipeElementIntoView(Element3, TouchActions.SwipeDirection.DOWN);
        }
        catch (AssertionError e){
            // pass
        }
    }

    @Test(priority = 4)
    public void SwipeElement1toElement2Sucessfull() {
        By Element1 =By.id("search_form_input_homepage");
        By Element2 =By.id("search_form_input_homepage");
        ElementActions.performTouchAction(driver).swipeElementIntoView(Element1,Element2, TouchActions.SwipeDirection.DOWN);
    }

    @Test(priority = 5)
    public void SwipeElement1toElement2Failled() {
        By Element1 =By.xpath("");
        By Element2 =By.id("search_form_input_homepage");
        try {
            ElementActions.performTouchAction(driver).swipeElementIntoView(Element1,Element2, TouchActions.SwipeDirection.DOWN);
        }
        catch (AssertionError e){
            // pass
        }
    }

    @Test(priority = 6)
    public void SwipeElement3toElement2Failled() {
        By Element3 =null;
        By Element2 =By.id("search_form_input_homepage");
        ElementActions.performTouchAction(driver).swipeElementIntoView(Element3,Element2, TouchActions.SwipeDirection.DOWN);
    }

    @Test(priority = 7)
    public void SwipeElement2toElement1Failled() {
        By Element2 =By.id("search_form_input_homepage");
        By Element1 =By.xpath("");

        try {
            ElementActions.performTouchAction(driver).swipeElementIntoView(Element2,Element1, TouchActions.SwipeDirection.DOWN);
        }
        catch (AssertionError e){
            // pass
        }
    }

    @Test(priority = 8)
    public void SwipeElement1toElement3Failled() {
        By Element1 =By.id("search_form_input_homepage");
        By Element3 =null;

        try {
            ElementActions.performTouchAction(driver).swipeElementIntoView(Element1,Element3, TouchActions.SwipeDirection.DOWN);
        }
        catch (AssertionError e){
            // pass
        }
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        driver = DriverFactory.getDriver(DriverType.DESKTOP_CHROME);
        BrowserActions.navigateToURL(driver, "https://duckduckgo.com/");
    }

    @AfterClass
    public void afterClass() {
        BrowserActions.closeCurrentWindow(driver);
    }
}