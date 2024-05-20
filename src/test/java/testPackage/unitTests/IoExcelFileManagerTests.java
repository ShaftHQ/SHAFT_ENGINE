package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class IoExcelFileManagerTests {

    ExcelFileManager testDataReader;
    // WebDriver driver;

    @Test
    public void getCellData_rowName_expectedToPass() {
        ReportManager.log(testDataReader.getCellData("testRowValue"));
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        testDataReader = new ExcelFileManager(SHAFT.Properties.paths.testData() + "testSuite01/TestData.xlsx");
        // driver = BrowserFactory.getBrowser(testDataReader);
    }
}
