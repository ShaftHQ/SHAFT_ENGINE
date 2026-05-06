package testPackage.unitTests;

import com.shaft.tools.io.CSVFileManager;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

/**
 * Unit tests for {@link CSVFileManager}.
 * Exercises all public accessor methods using the pre-existing test data file at
 * {@code src/test/resources/testDataFiles/TestData.csv}.
 *
 * <p>A fresh {@link CSVFileManager} instance is created before each test because
 * the underlying CSV readers are one-pass iterables; reusing the same instance
 * across tests would cause subsequent {@code getRows()} calls to return empty lists.
 *
 * <p>The CSV file has the following columns (0-based header row):
 * {@code barCode, aName, clientName, amountCalculated, amountCalculatedPayed, discount, totalDiscount}
 */
public class CSVFileManagerUnitTest {

    private static final String CSV_FILE = "src/test/resources/testDataFiles/TestData.csv";

    private CSVFileManager csv;

    @BeforeMethod
    public void setUp() {
        // Create a fresh instance per test — the CSV parsers are one-shot iterables
        csv = new CSVFileManager(CSV_FILE);
    }

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── getColumns ───────────────────────────────────────────────────────────

    @Test(description = "getColumns returns non-empty list")
    public void getColumnsReturnsNonEmptyList() {
        List<String> columns = csv.getColumns();
        Assert.assertNotNull(columns, "Columns list must not be null");
        Assert.assertFalse(columns.isEmpty(), "Columns list must not be empty");
    }

    @Test(description = "getColumns contains expected header names")
    public void getColumnsContainsExpectedHeaders() {
        List<String> columns = csv.getColumns();
        Assert.assertTrue(columns.contains("barCode"), "Expected 'barCode' column");
        Assert.assertTrue(columns.contains("totalDiscount"), "Expected 'totalDiscount' column");
    }

    // ─── getRows ──────────────────────────────────────────────────────────────

    @Test(description = "getRows returns non-empty list")
    public void getRowsReturnsNonEmptyList() {
        List<String[]> rows = csv.getRows();
        Assert.assertNotNull(rows, "Rows list must not be null");
        Assert.assertFalse(rows.isEmpty(), "Rows list must not be empty");
    }

    @Test(description = "getRows row count matches expected data rows")
    public void getRowsCountMatchesExpectedDataRows() {
        // TestData.csv has 3 data rows (excluding header)
        List<String[]> rows = csv.getRows();
        Assert.assertEquals(rows.size(), 3, "TestData.csv should have 3 data rows");
    }

    // ─── getFirstColumn ───────────────────────────────────────────────────────

    @Test(description = "getFirstColumn returns first column name")
    public void getFirstColumnReturnsFirstColumnName() {
        String firstColumn = csv.getFirstColumn();
        Assert.assertNotNull(firstColumn, "First column must not be null");
        Validations.assertThat().object(firstColumn).isEqualTo("barCode").perform();
    }

    // ─── getLastColumn ────────────────────────────────────────────────────────

    @Test(description = "getLastColumn returns last column name")
    public void getLastColumnReturnsLastColumnName() {
        String lastColumn = csv.getLastColumn();
        Assert.assertNotNull(lastColumn, "Last column must not be null");
        Validations.assertThat().object(lastColumn).isEqualTo("totalDiscount").perform();
    }

    // ─── getSpecificColumnName ────────────────────────────────────────────────

    @Test(description = "getSpecificColumnName(1) returns first column name")
    public void getSpecificColumnNameIndexOneReturnsFirstColumn() {
        String name = csv.getSpecificColumnName(1);
        Validations.assertThat().object(name).isEqualTo("barCode").perform();
    }

    @Test(description = "getSpecificColumnName(2) returns second column name")
    public void getSpecificColumnNameIndexTwoReturnsSecondColumn() {
        String name = csv.getSpecificColumnName(2);
        Validations.assertThat().object(name).isEqualTo("aName").perform();
    }

    // ─── getColumnsWithData ───────────────────────────────────────────────────

    @Test(description = "getColumnsWithData returns a populated map")
    public void getColumnsWithDataReturnsPopulatedMap() {
        Map<String, List<String>> columnsWithData = csv.getColumnsWithData();
        Assert.assertNotNull(columnsWithData, "Columns-with-data map must not be null");
        Assert.assertFalse(columnsWithData.isEmpty(), "Columns-with-data map must not be empty");
    }

    @Test(description = "getColumnsWithData map contains barCode key")
    public void getColumnsWithDataMapContainsBarCodeKey() {
        Map<String, List<String>> columnsWithData = csv.getColumnsWithData();
        Assert.assertTrue(columnsWithData.containsKey("barCode"),
                "Column 'barCode' should be present in getColumnsWithData map");
    }

    // ─── getSpecificColumnData (by name) ─────────────────────────────────────

    @Test(description = "getSpecificColumnData by name returns non-empty list")
    public void getSpecificColumnDataByNameReturnsNonEmptyList() {
        List<String> data = csv.getSpecificColumnData("barCode");
        Assert.assertNotNull(data, "Column data by name must not be null");
        Assert.assertFalse(data.isEmpty(), "Column data by name must not be empty");
    }

    @Test(description = "getSpecificColumnData by name contains known value")
    public void getSpecificColumnDataByNameContainsKnownValue() {
        List<String> barCodes = csv.getSpecificColumnData("barCode");
        Assert.assertTrue(barCodes.contains("1061998"),
                "barCode column should contain the value '1061998'");
    }

    // ─── getCellData (by row, column name) ────────────────────────────────────

    @Test(description = "getCellData(row=0, columnName='barCode') returns first row's barCode")
    public void getCellDataByRowZeroAndBarCodeColumnReturnsFirstBarCode() {
        String value = csv.getCellData(0, "barCode");
        Assert.assertNotNull(value, "getCellData by name must not return null");
        Validations.assertThat().object(value).isEqualTo("1234567890").perform();
    }

    @Test(description = "getCellData(row=1, columnName='barCode') returns second row's barCode")
    public void getCellDataByRowOneAndBarCodeColumnReturnsSecondBarCode() {
        String value = csv.getCellData(1, "barCode");
        Assert.assertNotNull(value, "getCellData by name must not return null");
        Validations.assertThat().object(value).isEqualTo("1061998").perform();
    }

    // ─── getMinCellValue (by column name) ────────────────────────────────────

    @Test(description = "getMinCellValue by column name returns a valid double for discount column")
    public void getMinCellValueByColumnNameReturnsValidDouble() {
        double min = csv.getMinCellValue("discount");
        Assert.assertFalse(Double.isNaN(min), "Min cell value must not be NaN");
    }

    // ─── getMinCellValue (by column index) ───────────────────────────────────

    @Test(description = "getMinCellValue by column index 5 (discount) returns valid double")
    public void getMinCellValueByColumnIndexFiveReturnsValidDouble() {
        // Index 5 = "discount" column (0-based)
        double min = csv.getMinCellValue(5);
        Assert.assertFalse(Double.isNaN(min), "Min cell value by index must not be NaN");
    }

    // ─── getMaxCellValue (by column name) ────────────────────────────────────

    @Test(description = "getMaxCellValue by column name returns a value >= min for discount column")
    public void getMaxCellValueByColumnNameIsGreaterOrEqualToMin() {
        double min = csv.getMinCellValue("discount");
        double max = csv.getMaxCellValue("discount");
        Assert.assertFalse(Double.isNaN(max), "Max cell value must not be NaN");
        Assert.assertTrue(max >= min, "Max must be >= min");
    }

    // ─── getMaxCellValue (by column index) ───────────────────────────────────

    @Test(description = "getMaxCellValue by column index 5 (discount) returns valid double")
    public void getMaxCellValueByColumnIndexFiveReturnsValidDouble() {
        double max = csv.getMaxCellValue(5);
        Assert.assertFalse(Double.isNaN(max), "Max cell value by index must not be NaN");
    }

    // ─── getCellCount (by column name) ───────────────────────────────────────

    @Test(description = "getCellCount by column name returns count of 3")
    public void getCellCountByColumnNameReturnsThree() {
        int count = csv.getCellCount("barCode");
        Assert.assertEquals(count, 3, "TestData.csv has 3 data rows, so cell count should be 3");
    }

    // ─── getCellCount (by column index) ──────────────────────────────────────

    @Test(description = "getCellCount by column index 0 (barCode) returns count of 3")
    public void getCellCountByColumnIndexZeroReturnsThree() {
        int count = csv.getCellCount(0);
        Assert.assertEquals(count, 3, "getCellCount by index 0 should return 3 for TestData.csv");
    }
}
