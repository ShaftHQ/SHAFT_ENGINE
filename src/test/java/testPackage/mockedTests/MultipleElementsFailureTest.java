package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MultipleElementsFailureTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String mockedHTML = "data:text/html,<input/><input/><input/><script>var result;</script><button ${HIDDEN} alt='Google' onclick='result=\"Clicked\"'>Go</button>";


    @Test(expectedExceptions = {AssertionError.class})
    public void type() {
        driver.get().browser().navigateToURL(mockedHTML);
        driver.get().element().type(By.xpath("//input"), "standard_user");
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void click() {
        driver.get().browser().navigateToURL(mockedHTML);
        driver.get().element().click(By.xpath("//input"));
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void clickUsingJS() {
        driver.get().browser().navigateToURL(mockedHTML);
        driver.get().element().clickUsingJavascript(By.xpath("//input"));
    }

    @BeforeMethod

    void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }


    @AfterMethod
    void afterMethod() {
        driver.get().quit();
    }
}
