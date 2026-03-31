package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.PropertyFileManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Map;

public class PropertyFileManagerUnitTest {
    private String savedMobileApp;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (savedMobileApp != null) {
            SHAFT.Properties.mobile.set().app(savedMobileApp);
        }
    }

    @Test(description = "getAppiumDesiredCapabilities should preserve BrowserStack bs:// app URL without path resolution")
    public void getAppiumDesiredCapabilities_preservesBrowserStackAppUrl() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String bsUrl = "bs://ce7b85a493195b37af78ec03750a7ecda18284cf";
        SHAFT.Properties.mobile.set().app(bsUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(bsUrl).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should preserve LambdaTest lt:// app URL without path resolution")
    public void getAppiumDesiredCapabilities_preservesLambdaTestAppUrl() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String ltUrl = "lt://APP100201061735398359698453";
        SHAFT.Properties.mobile.set().app(ltUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(ltUrl).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should preserve https:// app URL without path resolution")
    public void getAppiumDesiredCapabilities_preservesHttpsAppUrl() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String httpsUrl = "https://example.com/app.apk";
        SHAFT.Properties.mobile.set().app(httpsUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(httpsUrl).perform();
    }
}
