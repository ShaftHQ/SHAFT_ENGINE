package testPackage.legacy;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import org.openqa.selenium.chrome.ChromeOptions;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import testPackage.Tests;

public class ChromeProfileTest extends Tests {
    @BeforeMethod
    public void init() {
    }

    //    @Test(expectedExceptions = java.lang.AssertionError.class)
    public void simpleTest() {
        String userDataDirectory = "C:/Users/User/AppData/Local/Google/Chrome/User Data";
        String profileDirectory = "Profile 2";
        ChromeOptions chromeOptions = new ChromeOptions();
        chromeOptions.addArguments("user-data-dir=" + userDataDirectory);
        chromeOptions.addArguments("profile-directory=" + profileDirectory);
        driver.set(new SHAFT.GUI.WebDriver(DriverFactory.DriverType.CHROME, chromeOptions));
        driver.get().browser().navigateToURL("https://www.google.com");
        driver.get().browser().assertThat().url().contains("google").perform();
    }

    @AfterMethod
    public void tear() {
    }
}
