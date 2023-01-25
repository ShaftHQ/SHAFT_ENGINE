package testPackage;

import com.shaft.tools.io.JSONFileManager;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

public class Test_JSONFileManager {
    JSONFileManager testDataJSON;

    @Test
    public void readStringFromJson() {
        testDataJSON = new JSONFileManager(System.getProperty("testDataFolderPath") + "simpleJSON.json");
        Validations.assertThat().object(testDataJSON.getTestData("x.name")).equals("Mohab Mohie");
    }
}
