package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import io.github.shafthq.shaft.performance.lighthouse.LHGenerateReport;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_LightHouse2 {
    WebDriver driver;
    @BeforeClass
    public void beforeClass() {
        driver = DriverFactory.getDriver();
    }

    @Test (description = " Generate Lighthouse report for Google.com ")
    public void RunLightHouseGoogleSearch() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/search?q=shaft_engine");

        new LHGenerateReport(driver).GenerateReport("html");
    }

    @AfterClass
    public void afterClass() {
        driver.close();
    }

}
