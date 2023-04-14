package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.*;

public class NoSuchElementFailureTest {
    SHAFT.GUI.WebDriver driver;
    int defaultElementIdentificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
    String mockedHTML = "data:text/html,<input/><input/><input/><script>var result;</script><button ${HIDDEN} alt='Google' onclick='result=\"Clicked\"'>Go</button>";


    @Test(expectedExceptions = {AssertionError.class})
    public void type() {
        driver.browser().navigateToURL(mockedHTML);
        driver.element().type(By.xpath("//input[@id='noSuchElement']"), "standard_user");
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void click() {
        driver.browser().navigateToURL(mockedHTML);
        driver.element().click(By.xpath("//input[@id='noSuchElement']"));
    }

    @BeforeMethod
    void beforeMethod() {
        driver = SHAFT.GUI.WebDriver.getInstance();
    }

    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        driver.quit();
    }

    @BeforeClass
    void beforeClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
    }

    @AfterClass(alwaysRun = true)
    void afterClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultElementIdentificationTimeout);
    }
}
