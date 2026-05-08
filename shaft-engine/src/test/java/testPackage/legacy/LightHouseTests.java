package testPackage.legacy;

import com.shaft.driver.SHAFT;

public class LightHouseTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    //@BeforeClass
    public void beforeClass() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    //@Test (description = " Generate Lighthouse report for Google.com ")
    public void RunLightHouseGoogleSearch() {
        driver.get().browser().navigateToURL("https://www.google.com/search?q=shaft_engine&safe=active&ssui=on");
        driver.get().browser().generateLightHouseReport();
//        driver.get().browser().navigateToURL("https://assets-es-pprd.dxlpreprod.local.vodafone.es/mves/login");
//        driver.get().browser().generateLightHouseReport();
    }

    //@AfterClass(alwaysRun = true)
    public void afterClass() {
        driver.get().quit();
    }

}
