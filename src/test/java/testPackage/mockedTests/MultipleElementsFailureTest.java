package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MultipleElementsFailureTest {
    SHAFT.GUI.WebDriver driver;
    String mockedHTML = "data:text/html,<input/><input/><input/><script>var result;</script><button ${HIDDEN} alt='Google' onclick='result=\"Clicked\"'>Go</button>";


    @Test(expectedExceptions = {AssertionError.class})
    public void type() {
        driver.browser().navigateToURL(mockedHTML);
        driver.element().type(By.xpath("//input"), "standard_user");
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void click() {
        driver.browser().navigateToURL(mockedHTML);
        driver.element().click(By.xpath("//input"));
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void clickUsingJS() {
        driver.browser().navigateToURL(mockedHTML);
        driver.element().clickUsingJavascript(By.xpath("//input"));
    }

    @BeforeMethod

    void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }


    @AfterMethod
    void afterMethod() {
        driver.quit();
    }
}
