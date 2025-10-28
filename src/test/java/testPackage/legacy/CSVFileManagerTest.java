package testPackage.legacy;

import com.shaft.tools.io.CSVFileManager;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class CSVFileManagerTest {
    CSVFileManager csvFileManager;

    @BeforeMethod
    public void setUp() {
        // Initialize the CSVFileManager with a test file
        csvFileManager = new CSVFileManager("src/test/resources/testDataFiles/TestData.csv");
    }

    @Test
    public void testGetRows() {
        List<String[]> rows = csvFileManager.getRows();
        for (String[] row : rows) {
            System.out.println(Arrays.toString(row));
        }
        Assert.assertTrue(!rows.isEmpty(), "Rows should not be empty.");
    }

    @Test
    public void testGetColumns() {
        List<String> columns = csvFileManager.getColumns();
        System.out.println("Columns : " + columns);
        Assert.assertTrue(!columns.isEmpty(), "Columns should not be empty.");
    }

    @Test
    public void testGetColumnsWithData() {
        Map<String, List<String>> columnsWithData = csvFileManager.getColumnsWithData();
        System.out.println("columnsWithData : " + columnsWithData);
        Assert.assertTrue(!columnsWithData.isEmpty(), "Columns with data should not be empty.");
    }

    @Test
    public void testGetLastColumn() {
        String lastColumn = csvFileManager.getLastColumn();
        Assert.assertNotNull(lastColumn, "Last column should not be null.");
    }

    @Test
    public void testGetSpecificColumnName() {
        String columnName = csvFileManager.getSpecificColumnName(1); // Adjust the index if necessary
        Assert.assertNotNull(columnName, "Column name should not be null.");
        System.out.println(columnName);
        Assert.assertEquals(columnName, "barCode", "Column name should match the expected value.");
    }

    @Test
    public void testGetSpecificColumnData() {
        List<String> columnData = csvFileManager.getSpecificColumnData("barCode");
        Assert.assertNotNull(columnData, "Column data should not be null.");
        Assert.assertTrue(!columnData.isEmpty(), "Column data should not be empty.");
        System.out.println(columnData);
        Assert.assertTrue(columnData.contains("1061998"), "Column data should contain the expected data.");
    }

    @Test
    public void testGetCellData() {
        String cellData = csvFileManager.getCellData(1, "barCode");
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
        Assert.assertEquals(cellData, "1061998", "Cell data should match the expected value.");
    }
    @Test
    public void testCellCount() {
        String cellData = String.valueOf(csvFileManager.getCellCount("totalDiscount"));
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
    @Test
    public void testCellMax() {
        String cellData = String.valueOf(csvFileManager.getMaxCellValue("totalDiscount"));
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
    @Test
    public void testCellMin() {
        String cellData = String.valueOf(csvFileManager.getMinCellValue("totalDiscount"));
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
    @Test
    public void testCellCountIndex() {
        String cellData = String.valueOf(csvFileManager.getCellCount(6));
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
    @Test
    public void testCellMaxIndex() {
        String cellData = String.valueOf(csvFileManager.getMaxCellValue(6));
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
    @Test
    public void testCellMinIndex() {
        String cellData = String.valueOf(csvFileManager.getMinCellValue(6));
        System.out.println(cellData);
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
}
