package testPackage.unitTests;

import com.shaft.tools.io.ExcelFileManager;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.ZoneId;

public class ExcelFileManagerCoverageUnitTest {
    private static final Path TEMP_DIRECTORY = Path.of("target", "temp", "excelFileManagerCoverage");
    private final ThreadLocal<Path> excelFilePath = new ThreadLocal<>();
    private final ThreadLocal<ExcelFileManager> excelFileManager = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws IOException {
        Files.createDirectories(TEMP_DIRECTORY);
        Path workbookPath = Files.createTempFile(TEMP_DIRECTORY, "coverage-", ".xlsx");
        excelFilePath.set(workbookPath);
        createWorkbook(workbookPath);
        excelFileManager.set(new ExcelFileManager(workbookPath.toAbsolutePath().toString()));
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws IOException {
        Path workbookPath = excelFilePath.get();
        if (workbookPath != null) {
            Files.deleteIfExists(workbookPath);
        }
        excelFileManager.remove();
        excelFilePath.remove();
    }

    @Test
    public void getCellDataShouldParseNumericDateBooleanAndBlankValues() {
        Assert.assertEquals(excelFileManager.get().getCellData("numericRow", "testData1"), "42");
        Assert.assertEquals(excelFileManager.get().getCellData("dateRow", "testData1"), "15/01/24");
        Assert.assertEquals(excelFileManager.get().getCellData("booleanRow", "testData1"), "true");
        Assert.assertEquals(excelFileManager.get().getCellData("blankRow", "testData1"), "");
    }

    @Test
    public void getCellDataShouldReturnEmptyStringWhenTargetCellIsMissing() {
        Assert.assertEquals(excelFileManager.get().getCellData("missingCellRow", "testData1"), "");
    }

    @Test
    public void getLastColumnNumberShouldStopAtFirstNonStringHeader() {
        Assert.assertEquals(excelFileManager.get().getLastColumnNumber("Sheet1"), 2);
    }

    @Test
    public void getCellDataShouldThrowWhenRowNameDoesNotExist() {
        Assert.expectThrows(RuntimeException.class,
                () -> excelFileManager.get().getCellData("missingRow", "testData1"));
    }

    @Test
    public void getCellDataShouldThrowWhenColumnNameDoesNotExist() {
        Assert.expectThrows(RuntimeException.class,
                () -> excelFileManager.get().getCellData("numericRow", "missingColumn"));
    }

    @Test
    public void getColumnNameUsingRowNameAndCellDataShouldThrowWhenCellDataDoesNotExist() {
        Assert.expectThrows(RuntimeException.class,
                () -> excelFileManager.get().getColumnNameUsingRowNameAndCellData("Sheet1", "numericRow", "missingValue"));
    }

    private static void createWorkbook(Path path) throws IOException {
        try (XSSFWorkbook workbook = new XSSFWorkbook();
             OutputStream outputStream = Files.newOutputStream(path)) {
            XSSFSheet sheet = workbook.createSheet("Sheet1");

            Row header = sheet.createRow(0);
            header.createCell(0).setCellValue("key");
            header.createCell(1).setCellValue("testData1");
            header.createCell(2).setCellValue("testData2");
            header.createCell(3).setCellValue(99);

            Row numericRow = sheet.createRow(1);
            numericRow.createCell(0).setCellValue("numericRow");
            numericRow.createCell(1).setCellValue(42);

            Row dateRow = sheet.createRow(2);
            dateRow.createCell(0).setCellValue("dateRow");
            var dateCell = dateRow.createCell(1);
            LocalDate localDate = LocalDate.of(2024, 1, 15);
            dateCell.setCellValue(java.util.Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant()));
            CreationHelper creationHelper = workbook.getCreationHelper();
            CellStyle dateStyle = workbook.createCellStyle();
            dateStyle.setDataFormat(creationHelper.createDataFormat().getFormat("dd/MM/yy"));
            dateCell.setCellStyle(dateStyle);

            Row booleanRow = sheet.createRow(3);
            booleanRow.createCell(0).setCellValue("booleanRow");
            booleanRow.createCell(1).setCellValue(true);

            Row blankRow = sheet.createRow(4);
            blankRow.createCell(0).setCellValue("blankRow");
            blankRow.createCell(1);

            Row missingCellRow = sheet.createRow(5);
            missingCellRow.createCell(0).setCellValue("missingCellRow");

            workbook.write(outputStream);
        }
    }
}
