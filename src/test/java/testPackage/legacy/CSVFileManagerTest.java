package testPackage.legacy;

import com.shaft.tools.io.CSVFileManager;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

public class CSVFileManagerTest {
    private CSVFileManager csvFileManager;
    private final String testCsvFilePath = "src/test/resources/NegativeTestDataforinvoices.csv"; // Replace with the path to your test CSV file

    @BeforeClass
    public void setUp() {
        // Initialize the CSVFileManager with a test file
        csvFileManager = new CSVFileManager(testCsvFilePath);
    }

    @Test
    public void testGetRows() {
        List<String[]> rows = csvFileManager.GetRows();
        System.out.println("Rows: "+rows);
        Assert.assertTrue(!rows.isEmpty(), "Rows should not be empty.");
    }

    @Test
    public void testGetColumns() {
        List<String> columns = csvFileManager.GetColumns();
        System.out.println("Columns : "+columns);
        Assert.assertTrue(!columns.isEmpty(), "Columns should not be empty.");
    }

    @Test
    public void testGetColumnsWithData() {
        Map<String, List<String>> columnsWithData = csvFileManager.GetColumnsWithData();
        System.out.println("columnsWithData : "+columnsWithData);
        Assert.assertTrue(!columnsWithData.isEmpty(), "Columns with data should not be empty.");
    }

    @Test
    public void testGetLastColumn() {
        String lastColumn = csvFileManager.GetLastColumn();
        Assert.assertNotNull(lastColumn, "Last column should not be null.");
    }

    @Test
    public void testGetSpecificColumnName() {
        String columnName = csvFileManager.GetSpecificColumnName(1);
    }

    @Test
    public void testGetSpecificColumnData() {
        List<String> columnData = csvFileManager.GetSpecificColumnData("Column1");
        Assert.assertNotNull(columnData, "Column data should not be null.");
        Assert.assertTrue(!columnData.isEmpty(), "Column data should not be empty.");
    }

    @Test
    public void testGetCellData() {
        String cellData = csvFileManager.getCellData(1, "Column1");
        Assert.assertNotNull(cellData, "Cell data should not be null.");
    }
}
