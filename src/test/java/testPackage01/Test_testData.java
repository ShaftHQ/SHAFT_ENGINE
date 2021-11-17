package testPackage01;

import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.annotations.Test;

public class Test_testData {
    @Test
    public void f() {
        System.setProperty("testDataFilePath", System.getProperty("testDataFolderPath")+"testSuite01/TestData.xlsx");
        ExcelFileManager testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));

        ReportManager.log("Last Column Number is: [" + testDataReader.getLastColumnNumber() + "]. Zero-based.");
    }
}
