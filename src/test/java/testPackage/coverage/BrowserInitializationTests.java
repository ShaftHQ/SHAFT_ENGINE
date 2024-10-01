package testPackage.coverage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BrowserInitializationTests {
    String executionAddress = "local";

    @Test(expectedExceptions = AssertionError.class)
    public void failedInitializationDueToMalformedURL() {
        SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
        driver.quit();
    }

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        executionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("http://127.0.0.1:4723");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
    }
}
