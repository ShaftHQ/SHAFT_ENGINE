package unitTests;

import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class tests_io_excelFileManager {

    ExcelFileManager testDataReader;
    // WebDriver driver;

    @Test
    public void getCellData_rowName_expectedToPass() {
        ReportManager.log(testDataReader.getCellData("testRowValue"));
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        System.setProperty("testDataFilePath", System.getProperty("testDataFolderPath")+"testSuite01/TestData.xlsx");
        testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));
        // driver = BrowserFactory.getBrowser(testDataReader);
    }
}
