package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_RelativeLocators {
    WebDriver driver;

    @Test
    public void relativeLocatorsTest1(){
        BrowserActions.getInstance(driver).navigateToURL("https://duckduckgo.com/?");
        By searchbar = By.xpath("//*[@id='search_form_input_homepage'] | //input[@name='q']");
        new ElementActions(driver).type(searchbar,"SHAFT_Engine")
                .keyPress(searchbar, Keys.ENTER);

        //the below locator matches all 10 search results
        By searchResults = By.xpath("//a[@data-testid='result-title-a']");

        //the below traditional xpath matches the first search result by using index
        By firstSearchResult = By.xpath("(//a[@data-testid='result-title-a'])[1]");

        //to use the relative xpaths we need to identify a clear element first
        By searchBox = By.xpath("//input[@name='q']");

        //now we can identify the first search result that's below that search box
        By firstSearchResultRelatively = RelativeLocator.with(searchResults).below(searchBox);

        //this method makes writing locators much easier! And now we can perform our validation
        Validations.assertThat()
                .element(driver, firstSearchResultRelatively)
                .text()
                .doesNotEqual("")
                .perform();

    }

    @BeforeMethod
    public void beforeMethod(){
        driver = DriverFactory.getHelper().getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        DriverFactory.closeAllDrivers();
    }
}
