package testPackage01;

import com.shaft.tools.io.JSONFileManager;
import com.shaft.validation.Assertions;
import org.testng.annotations.Test;

public class Test_JSONFileManager {
    JSONFileManager testDataJSON;

    @Test
    public void readStringFromJson() {
        testDataJSON = new JSONFileManager(System.getProperty("testDataFolderPath") + "simpleJSON.json");
        Assertions.assertEquals("Mohab Mohie", testDataJSON.getTestData("x.name"));
    }
}
