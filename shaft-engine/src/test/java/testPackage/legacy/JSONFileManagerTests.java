package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.JSONFileManager;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

public class JSONFileManagerTests {
    JSONFileManager testDataJSON;

    @Test
    public void readStringFromJson() {
        testDataJSON = new JSONFileManager(SHAFT.Properties.paths.testData() + "simpleJSON.json");
        Validations.assertThat().object(testDataJSON.getTestData("x.name")).equals("Mohab Mohie");
    }
}
