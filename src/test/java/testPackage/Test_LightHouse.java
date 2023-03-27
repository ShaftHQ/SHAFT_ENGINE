package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_LightHouse {
   SHAFT.GUI.WebDriver driver;

    @BeforeClass
    public void beforeClass() {
         driver = new SHAFT.GUI.WebDriver();
    }

    @Test (description = " Generate Lighthouse report for Google.com ")
    public void RunLightHouseGoogleSearch() {
        driver.browser().navigateToURL("https://www.google.com/search?q=shaft_engine&safe=active&ssui=on");
        driver.browser().generateLightHouseReport();
//        driver.browser().navigateToURL("https://assets-es-pprd.dxlpreprod.local.vodafone.es/mves/login");
//        driver.browser().generateLightHouseReport();
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        driver.quit();
    }

}
