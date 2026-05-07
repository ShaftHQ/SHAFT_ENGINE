package testPackage.unitTests;

import com.shaft.tools.io.CSVFileManager;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.util.HashMap;
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

    // ─── error and edge paths ────────────────────────────────────────────────

    @Test(description = "constructor throws runtime exception when file is missing")
    public void constructorThrowsRuntimeExceptionWhenFileIsMissing() {
        Assert.expectThrows(RuntimeException.class, () -> new CSVFileManager("missing_CSVFileManagerUnitTest.csv"));
    }

    @Test(description = "getRows catches failures when row records are unavailable")
    public void getRowsCatchesFailuresWhenRowRecordsAreUnavailable() {
        setPrivateField(csv, "RowRecords", null);
        Assert.assertTrue(csv.getRows().isEmpty(), "Rows should be empty when records are unavailable");
    }

    @Test(description = "getColumns catches failures when parser records are unavailable")
    public void getColumnsCatchesFailuresWhenParserRecordsAreUnavailable() {
        setPrivateField(csv, "records", null);
        Assert.assertTrue(csv.getColumns().isEmpty(), "Columns should be empty when parser records are unavailable");
    }

    @Test(description = "getColumnsWithData catches runtime exceptions from row retrieval")
    public void getColumnsWithDataCatchesRuntimeExceptions() {
        CSVFileManager csvWithBrokenRows = new CSVFileManager(CSV_FILE) {
            @Override
            public List<String[]> getRows() {
                throw new RuntimeException("broken rows");
            }
        };
        Assert.assertTrue(csvWithBrokenRows.getColumnsWithData().isEmpty(),
                "Columns-with-data should be empty when row retrieval fails");
    }

    @Test(description = "getLastColumn returns null when source has no header rows")
    public void getLastColumnReturnsNullWhenColumnsAreUnavailable() {
        setPrivateField(csv, "records", null);
        Assert.assertNull(csv.getLastColumn(), "Unavailable columns should return null last column");
    }

    @Test(description = "getSpecificColumnName returns null for invalid index")
    public void getSpecificColumnNameReturnsNullForInvalidIndex() {
        Assert.assertNull(csv.getSpecificColumnName(0), "Invalid index should return null column name");
    }

    @Test(description = "getSpecificColumnData handles unexpected runtime exceptions")
    public void getSpecificColumnDataHandlesUnexpectedRuntimeExceptions() {
        CSVFileManager csvWithBrokenMapping = new CSVFileManager(CSV_FILE) {
            @Override
            public Map<String, List<String>> getColumnsWithData() {
                throw new RuntimeException("broken mapping");
            }
        };
        Assert.assertTrue(csvWithBrokenMapping.getSpecificColumnData("barCode").isEmpty(),
                "Column data should be empty when mapping fails unexpectedly");
        Assert.assertTrue(csvWithBrokenMapping.getSpecificColumnData(1).isEmpty(),
                "Column data by index should be empty when mapping fails unexpectedly");
    }

    @Test(description = "getCellData returns null when file content is unavailable")
    public void getCellDataReturnsNullWhenFileContentIsUnavailable() {
        Assert.assertNull(csv.getCellData(99, "barCode"), "Invalid row should return null cell by name");
        Assert.assertNull(csv.getCellData(99, 1), "Invalid row should return null cell by index");
    }

    @Test(description = "getFirstColumn returns null when columns are unavailable")
    public void getFirstColumnReturnsNullWhenColumnsAreUnavailable() {
        setPrivateField(csv, "records", null);
        Assert.assertNull(csv.getFirstColumn(), "Unavailable columns should return null first column");
    }

    @Test(description = "getMinCellValue by name returns NaN for non-numeric input")
    public void getMinCellValueByNameReturnsNaNForNonNumericInput() {
        double min = csv.getMinCellValue("amountCalculated");
        Assert.assertTrue(Double.isNaN(min), "Non-numeric column should return NaN");
    }

    @Test(description = "getMinCellValue by index returns NaN for non-numeric input")
    public void getMinCellValueByIndexReturnsNaNForNonNumericInput() {
        double min = csv.getMinCellValue(3);
        Assert.assertTrue(Double.isNaN(min), "Non-numeric column index should return NaN");
    }

    @Test(description = "getMaxCellValue by name parses numeric columns and returns expected max")
    public void getMaxCellValueByNameParsesNumericColumnsAndReturnsExpectedMax() {
        double max = csv.getMaxCellValue("totalDiscount");
        Assert.assertEquals(max, 9_999_999_999d, "Expected max totalDiscount from test data");
    }

    @Test(description = "getMaxCellValue by name returns NaN for non-numeric input")
    public void getMaxCellValueByNameReturnsNaNForNonNumericInput() {
        double max = csv.getMaxCellValue("amountCalculated");
        Assert.assertTrue(Double.isNaN(max), "Non-numeric column should return NaN");
    }

    @Test(description = "getMaxCellValue by index returns NaN for non-numeric input")
    public void getMaxCellValueByIndexReturnsNaNForNonNumericInput() {
        double max = csv.getMaxCellValue(3);
        Assert.assertTrue(Double.isNaN(max), "Non-numeric column index should return NaN");
    }

    @Test(description = "getCellCount catches unexpected runtime exceptions")
    public void getCellCountCatchesUnexpectedRuntimeExceptions() {
        CSVFileManager csvWithEmptyMapping = new CSVFileManager(CSV_FILE) {
            @Override
            public Map<String, List<String>> getColumnsWithData() {
                return new HashMap<>();
            }
        };
        Assert.assertEquals(csvWithEmptyMapping.getCellCount("barCode"), 0,
                "Cell count by name should fallback to zero when mapping is incomplete");
        Assert.assertEquals(csvWithEmptyMapping.getCellCount(0), 0,
                "Cell count by index should fallback to zero when mapping is incomplete");
    }

    private void setPrivateField(CSVFileManager target, String fieldName, Object value) {
        try {
            Field field = CSVFileManager.class.getDeclaredField(fieldName);
            field.setAccessible(true);
            field.set(target, value);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }
}
