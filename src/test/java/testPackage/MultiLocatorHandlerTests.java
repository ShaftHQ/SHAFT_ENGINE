package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.MultiLocatorHandler;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MultiLocatorHandlerTests {
    SHAFT.GUI.WebDriver driver;
    By genericElement = By.cssSelector(".routing a");


    @Test
    public void testGetLocatorByText(){
        By uniqueLocator = MultiLocatorHandler.getLocatorByText(genericElement, "Dojo");
        driver.element().click(uniqueLocator);
        Assert.assertEquals(driver.browser().getCurrentURL(), "https://todomvc.com/examples/dojo/");
    }
    @Test
    public void testGetLocatorContainingText(){
        By uniqueLocator = MultiLocatorHandler.getLocatorContainingText(genericElement, "Angular");
        driver.element().click(uniqueLocator);
        Assert.assertEquals(driver.browser().getCurrentURL(), "https://todomvc.com/examples/angularjs/#/");
    }
    @Test
    public void testGetLocatorByIndex(){
        By uniqueLocator = MultiLocatorHandler.getLocatorByIndex(genericElement, 3);
        driver.element().click(uniqueLocator);
        Assert.assertEquals(driver.browser().getCurrentURL(), "https://todomvc.com/examples/knockoutjs/");
    }
    @Test(description = "Verify that the using the method more than once doesn't override the results")
    public void testLocatorUniqueness(){
        //one locator before
        MultiLocatorHandler.getLocatorContainingText(genericElement, "Angular");
        //target locator
        By uniqueLocator = MultiLocatorHandler.getLocatorContainingText(genericElement, "Dojo");
        //one locator after
        MultiLocatorHandler.getLocatorContainingText(genericElement, "React");
        driver.element().click(uniqueLocator);
        Assert.assertEquals(driver.browser().getCurrentURL(), "https://todomvc.com/examples/dojo/");

    }


    @BeforeMethod(description = "Setup Browser instance.")
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://todomvc.com/");
    }

    @AfterMethod(description = "Teardown Browser instance.")
    public void afterMethod() {
        driver.quit();
    }
}
