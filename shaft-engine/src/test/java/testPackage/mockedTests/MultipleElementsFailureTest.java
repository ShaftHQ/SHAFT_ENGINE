package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

public class MultipleElementsFailureTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String mockedHTML = TestPageServer.url("multipleElementsFixture.html");
    // issue #4321: 3 elements match, but only 1 is displayed+enabled (the other 2 are hidden /
    // disabled respectively) -- this MUST auto-resolve to the single actionable element instead of
    // throwing, unlike mockedHTML above where all 3 matches are genuinely actionable.
    String oneVisibleHTML = TestPageServer.url("multipleElementsOneVisibleFixture.html");


    @Test(expectedExceptions = {RuntimeException.class})
    public void type() {
        driver.get().browser().navigateToURL(mockedHTML);
        driver.get().element().type(By.xpath("//input"), "standard_user");
    }

    @Test(expectedExceptions = {RuntimeException.class})
    public void click() {
        driver.get().browser().navigateToURL(mockedHTML);
        driver.get().element().click(By.xpath("//input"));
    }

    @Test(expectedExceptions = {RuntimeException.class})
    public void clickUsingJS() {
        driver.get().browser().navigateToURL(mockedHTML);
        driver.get().element().clickUsingJavascript(By.xpath("//input"));
    }

    @Test
    public void typeResolvesToTheOnlyVisibleAndEnabledElementAmongMultipleMatches() {
        driver.get().browser().navigateToURL(oneVisibleHTML);
        // the locator itself is genuinely ambiguous (3 matches); the action must still succeed
        // because exactly one of them is displayed and enabled.
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//input")), 3);
        driver.get().element().type(By.xpath("//input"), "standard_user");
    }

    @Test
    public void clickResolvesToTheOnlyVisibleAndEnabledElementAmongMultipleMatches() {
        driver.get().browser().navigateToURL(oneVisibleHTML);
        Assert.assertEquals(driver.get().element().getElementsCount(By.xpath("//input")), 3);
        driver.get().element().click(By.xpath("//input"));
    }

    @BeforeMethod

    void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }


    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        if (driver.get() != null) driver.get().quit();
    }
}
