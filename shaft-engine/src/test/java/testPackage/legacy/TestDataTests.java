package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.annotations.Test;

public class TestDataTests {
    @Test
    public void f() {
        ExcelFileManager testDataReader = new ExcelFileManager(SHAFT.Properties.paths.testData() + "testSuite01/TestData.xlsx");
        ReportManager.log("Last Column Number is: [" + testDataReader.getLastColumnNumber() + "]. Zero-based.");
    }
}
