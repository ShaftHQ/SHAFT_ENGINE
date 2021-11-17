package testPackage01;

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
    ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void relativeLocatorsTest1(){
        BrowserActions.navigateToURL(driver.get(), "https://duckduckgo.com/");
        By searchbar = By.id("search_form_input_homepage");
        new ElementActions(driver.get()).type(searchbar,"SHAFT_Engine")
                .keyPress(searchbar, Keys.ENTER);

        //the below locator matches all 10 search results
        By searchResults = By.xpath("//a[@class='result__a js-result-title-link']");

        //the below traditional xpath matches the first search result by using index
        By firstSearchResult = By.xpath("(//a[@class='result__a js-result-title-link'])[1]");

        //to use the relative xpaths we need to identify a clear element first
        By searchBox = By.xpath("//input[@name='q']");

        //now we can identify the first search result that's below that search box
        By firstSearchResultRelatively = RelativeLocator.with(searchResults).below(searchBox);

        //this method makes writing locators much easier! And now we can perform our validation
        Validations.assertThat()
                .element(driver.get(), firstSearchResultRelatively)
                .text()
                .doesNotEqual("")
                .perform();

    }

    @BeforeMethod
    public void beforeMethod(){
        driver.set(DriverFactory.getDriver());
    }

    @AfterMethod
    public void afterMethod(){
        BrowserActions.closeCurrentWindow(driver.get());
    }
}
