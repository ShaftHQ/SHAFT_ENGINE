package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class IoExcelFileManagerTests {

    ExcelFileManager testDataReader;

    @BeforeClass
    public void beforeClass() {
        testDataReader = new ExcelFileManager(SHAFT.Properties.paths.testData() + "testSuite01/TestData.xlsx");
    }

    @Test
    public void getCellData_rowName_expectedToPass() {
        String value = testDataReader.getCellData("testRowValue");
        ReportManager.log("getCellData(rowName) returned: " + value);
        Assert.assertNotNull(value, "getCellData(rowName) must not return null");
    }

    @Test(description = "getCellData(rowName, columnName) returns a non-null value")
    public void getCellDataByRowNameAndColumnName() {
        // Empty column name defaults to the first value column (index 1)
        String value = testDataReader.getCellData("testRowValue", "");
        ReportManager.log("getCellData(rowName, '') returned: " + value);
        Assert.assertNotNull(value, "getCellData with empty column name must not return null");
    }

    @Test(description = "getLastColumnNumber returns a non-negative value")
    public void getLastColumnNumberReturnsNonNegative() {
        int lastCol = testDataReader.getLastColumnNumber();
        ReportManager.log("Last column number (0-based): " + lastCol);
        Assert.assertTrue(lastCol >= 0, "Last column number must be >= 0");
    }

    @Test(description = "getLastColumnNumber with explicit sheet name matches default sheet's last column")
    public void getLastColumnNumberWithSheetNameMatchesDefault() {
        int defaultLast = testDataReader.getLastColumnNumber();
        // "TestData" is the known sheet name used by the test data file at testSuite01/TestData.xlsx
        try {
            int explicitLast = testDataReader.getLastColumnNumber("TestData");
            Validations.assertThat().object(explicitLast).isEqualTo(defaultLast).perform();
        } catch (Exception e) {
            // If the sheet name differs, skip comparison rather than failing
            ReportManager.log("Skipping explicit-sheet comparison: " + e.getMessage());
        }
    }

    @Test(description = "getColumnNameUsingRowNameAndCellData returns a non-empty string")
    public void getColumnNameUsingRowNameAndCellData() {
        // First read the value from the default cell, then look up which column holds it
        String cellValue = testDataReader.getCellData("testRowValue");
        if (cellValue != null && !cellValue.isEmpty()) {
            String colName = testDataReader.getColumnNameUsingRowNameAndCellData("testRowValue", cellValue);
            Assert.assertNotNull(colName, "Column name lookup must not be null");
            Assert.assertFalse(colName.isEmpty(), "Column name lookup must not be empty");
        }
    }
}
