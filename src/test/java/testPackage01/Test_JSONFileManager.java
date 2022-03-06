package testPackage01;

import com.shaft.tools.io.JSONFileManager;
import com.shaft.validation.Assertions;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;


public class Test_JSONFileManager {
    JSONFileManager testDataJSON;

    @Test
    public void readStringFromJson() {
        testDataJSON = new JSONFileManager(System.getProperty("testDataFolderPath") + "simpleJSON.json");
        Assertions.assertEquals("Mohab Mohie", testDataJSON.getTestData("x.name"));
    }
    @Test
    public void readJsonFileAsString() throws IOException{
        testDataJSON = new JSONFileManager(System.getProperty("testDataFolderPath") + "simpleJSON.json");
        String Excepted = Files.readString(Paths.get(System.getProperty("testDataFolderPath") + "simpleJSON.txt"));

        Assertions.assertEquals(Excepted,testDataJSON.readJsonFileAsString());
    }
}
