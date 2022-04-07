package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class Test_Wizard_API {
    SHAFT.API driver;

    @Test
    public void test() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        driver.assertThatResponse().matchesSchema("schema.json").perform();
    }
}
