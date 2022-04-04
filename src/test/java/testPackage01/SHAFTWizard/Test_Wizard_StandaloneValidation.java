package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class Test_Wizard_StandaloneValidation {
    @Test
    public void number() {
        SHAFT.Validations.assertThat().number(3).isGreaterThanOrEquals(1).perform();
    }

    @Test
    public void object() {
        SHAFT.Validations.assertThat().object("").isEqualTo("").perform();
    }

    @Test
    public void file() {
        SHAFT.Validations.assertThat().file("src/test/resources/testDataFiles/", "simpleJSON.json").exists().perform();
    }

    @Test
    public void forceFail() {
        try {
            SHAFT.Validations.assertThat().forceFail().perform();
        } catch (AssertionError assertionError) {

        }
    }
}
