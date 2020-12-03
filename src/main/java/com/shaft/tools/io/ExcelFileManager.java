package com.shaft.tools.io;

import org.apache.poi.EmptyFileException;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.testng.Assert;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

@SuppressWarnings("unused")
public class ExcelFileManager {
    private FileInputStream fis;
    private XSSFWorkbook workbook;
    private XSSFSheet sheet;
    private XSSFRow row;
    private XSSFCell cell;
    private String excelFilePath;
    private String testDataColumnNamePrefix;

    /**
     * Creates a new instance of the test data excel reader using the target excel
     * file path
     *
     * @param excelFilePath target test data excel file path
     */
    public ExcelFileManager(String excelFilePath) {
        initializeVariables();
        this.excelFilePath = excelFilePath;
        try {
            fis = new FileInputStream(excelFilePath);
            workbook = new XSSFWorkbook(fis);
            fis.close();
//            ReportManager.logDiscrete("Reading test data from the following file [" + excelFilePath + "].");
        } catch (IOException e) {
            ReportManager.log(e);
            ReportManager.log("Couldn't find the desired file. [" + excelFilePath + "].");
            Assert.fail("Couldn't find the desired file. [" + excelFilePath + "].");
        } catch (OutOfMemoryError e) {
//	    ReportManager.log(e); override function to be able to log errors
            ReportManager.log("Couldn't open the desired file. [" + excelFilePath + "].");
            Assert.fail("Couldn't open the desired file. [" + excelFilePath + "].");
        } catch (EmptyFileException e) {
            ReportManager.log(e);
            ReportManager.log("Please check the target file, as it may be corrupted. [" + excelFilePath + "].");
            Assert.fail("Please check the target file, as it may be corrupted. [" + excelFilePath + "].");
        }

        List<List<Object>> attachments = new ArrayList<>();
        List<Object> testDataFileAttachment = null;
        try {
            testDataFileAttachment = Arrays.asList("Test Data", "Excel",
                    new FileInputStream(excelFilePath));
        } catch (FileNotFoundException e) {
            //unreachable code because if the file was not found then the reader would have failed at a previous step
        }
        attachments.add(testDataFileAttachment);
        ReportManager.log("Successfully loaded the following test data file [" + excelFilePath + "].", attachments);
    }

    /**
     * Reads cell data from the first sheet in the desired excel workbook Reads cell
     * data using row name (1st column) only Reads cell data from the 2nd column
     * (1st Value in the test data file)
     *
     * @param rowName the value of the first cell of the target row
     * @return the value of the target cell within the target row and the second
     * column within the default sheet
     */
    public String getCellData(String rowName) {
        return getCellData(getDefaultSheetName(), rowName, "");
    }

    /**
     * Reads cell data from the first sheet in the desired excel workbook Reads cell
     * data using row name (1st column) and column name
     *
     * @param rowName    the value of the first cell of the target row
     * @param columnName the value of the first cell of the target column
     * @return the value of the target cell within the target row and column within
     * the default sheet
     */
    public String getCellData(String rowName, String columnName) {
        return getCellData(getDefaultSheetName(), rowName, columnName);
    }

    /**
     * Reads cell data from a specific sheet name inside the excel file Reads cell
     * data using row name (1st column) and column name
     *
     * @param sheetName  the name of the target excel sheet
     * @param rowName    the value of the first cell of the target row
     * @param columnName the value of the first cell of the target column
     * @return the value of the target cell within the target row and column within
     * the target sheet
     */
    public String getCellData(String sheetName, String rowName, String columnName) {
        try {
            int rowNum = getRowNumberFromRowName(sheetName, rowName);
            int colNum = getColumnNumberFromColumnName(sheetName, columnName);

            // get the desired row
            row = sheet.getRow(rowNum); // why use -1 here?
            // get the desired cell
            cell = row.getCell(colNum);

            // return cell value given the different cell types
            return getCellData();

        } catch (Exception e) {
            ReportManager.log(e);
            ReportManager.log("Failed to read data from row [" + rowName + "] and column [" + columnName
                    + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath
                    + "].");
            Assert.fail("Failed to read data from row [" + rowName + "] and column [" + columnName
                    + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath
                    + "].");
            return "";
        }
    }

    /**
     * Returns the last column number that contains a header value (zero based)
     * assuming that you are working within the default sheet. That is if the last
     * column number is 3, it means that there are 4 columns.
     *
     * @return the number of the last column that holds data in the default test
     * data sheet
     */
    public int getLastColumnNumber() {
        return getLastColumnNumber(getDefaultSheetName());
    }

    /**
     * Returns the last column number that contains a header value (zero based).
     * That is if the last column number is 3, it means that there are 4 columns.
     *
     * @param sheetName the name of the target excel sheet
     * @return the number of the last column that holds data in the target test data
     * sheet
     */
    public int getLastColumnNumber(String sheetName) {
        sheet = workbook.getSheet(sheetName);
        row = sheet.getRow(0);
        int lastColumnNumber = 0;
        while (true) {
            try {
                cell = row.getCell(lastColumnNumber);
                if (cell.getCellType() == CellType.STRING) {
                    lastColumnNumber++;
                } else {
                    return lastColumnNumber - 1;
                }
            } catch (java.lang.NullPointerException e) {
                return lastColumnNumber - 1;
            }
        }
    }

    /**
     * Looks for the column name that holds the cellData in the specified rowName
     * and sheetName
     *
     * @param sheetName the name of the target excel sheet
     * @param rowName   the value of the first cell of the target row
     * @param cellData  the value of the target cell within the target row
     * @return the value of the first cell of the target column
     */
    public String getColumnNameUsingRowNameAndCellData(String sheetName, String rowName, String cellData) {
        String columnName;
        for (int i = 1; i <= getLastColumnNumber(sheetName); i++) {
            columnName = testDataColumnNamePrefix + i;
            if (cellData.equals(getCellData(rowName, columnName))) {
                return columnName;
            }
        }
        ReportManager.log("Failed to get column name using row [" + rowName + "] and cell data [" + cellData
                + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath + "].");
        Assert.fail("Failed to get column name using row [" + rowName + "] and cell data [" + cellData
                + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath + "].");
        return "";
    }

    /**
     * Looks for the column name that holds the cellData in the specified rowName
     * and the default sheet name
     *
     * @param rowName  the value of the first cell of the target row
     * @param cellData the value of the target cell within the target row
     * @return the value of the first cell of the target column
     */
    public String getColumnNameUsingRowNameAndCellData(String rowName, String cellData) {
        return getColumnNameUsingRowNameAndCellData(getDefaultSheetName(), rowName, cellData);
    }

    private void initializeVariables() {
        fis = null;
        workbook = null;
        sheet = null;
        row = null;
        cell = null;
        excelFilePath = "";
        testDataColumnNamePrefix = System.getProperty("testDataColumnNamePrefix");
    }

    private int getRowNumberFromRowName(String sheetName, String rowName) {
        try {
            // get the row number that corresponds to the desired rowName within the first
            // column [0]
            sheet = workbook.getSheet(sheetName);

            for (int i = 0; i <= sheet.getLastRowNum(); i++) {
                row = sheet.getRow(i);
                // get the first cell of each row, and compare it to rowName
                // if they match then that's the row we want

                if (row != null && row.getCell(0).getStringCellValue().equals(rowName)) {
                    return i;
                }
                // in certain cases if the row is empty, its value is set to null, and hence a
                // null pointer exception is thrown when
                // you try to get the cell from it.
                // we can skip this exception by checking if row != null.
            }

            // in case you provided valid data type, no exceptions were thrown, and yet the
            // rowName you mentioned was not present in this sheet
            ReportManager.log(
                    "Failed to get the row number that coresponds to rowName [" + rowName + "] in the Test Data Sheet ["
                            + sheetName + "], under the following path [" + excelFilePath + "].");
            Assert.fail(
                    "Failed to get the row number that coresponds to rowName [" + rowName + "] in the Test Data Sheet ["
                            + sheetName + "], under the following path [" + excelFilePath + "].");
            return -1; // in case of failure this line is unreachable
        } catch (Exception e) {
            ReportManager.log(e);
            ReportManager.log(
                    "Failed to get the row number that coresponds to rowName [" + rowName + "] in the Test Data Sheet ["
                            + sheetName + "], under the following path [" + excelFilePath + "].");
            Assert.fail(
                    "Failed to get the row number that coresponds to rowName [" + rowName + "] in the Test Data Sheet ["
                            + sheetName + "], under the following path [" + excelFilePath + "].");
            return -1; // in case of failure this line is unreachable
        }
    }

    private int getColumnNumberFromColumnName(String sheetName, String columnName) {
        try {
            // get the column number that corresponds to the desired columnName within the
            // target row [row_Num]
            // if no column name is provided, retrieves data from the 2nd
            // column (1st Value in the test data file)
            if (!columnName.equals("")) {
                row = sheet.getRow(0);
                for (int i = 0; i < row.getLastCellNum(); i++) {
                    // get the first cell of each column, and compare it to columnName
                    // if they match then that's the column we want
                    if (row.getCell(i).getStringCellValue().equals(columnName)) {
                        return i;
                    }
                }
            } else {
                return 1;
            }

            // in case you provided valid data type, no exceptions were thrown, and yet the
            // columnName you mentioned was not present in this sheet
            ReportManager.log("Failed to get the column number that coresponds to columnName [" + columnName
                    + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath
                    + "].");
            Assert.fail("Failed to get the column number that coresponds to columnName [" + columnName
                    + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath
                    + "].");
            return -1; // in case of failure this line is unreachable
        } catch (Exception e) {
            ReportManager.log(e);
            ReportManager.log("Failed to get the column number that coresponds to columnName [" + columnName
                    + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath
                    + "].");
            Assert.fail("Failed to get the column number that coresponds to columnName [" + columnName
                    + "] in the Test Data Sheet [" + sheetName + "], under the following path [" + excelFilePath
                    + "].");
            return -1; // in case of failure this line is unreachable
        }
    }

    private String getCellData() {
        try {
            if (cell.getCellType() == CellType.STRING) {
                return cell.getStringCellValue();
            } else if (cell.getCellType() == CellType.NUMERIC || cell.getCellType() == CellType.FORMULA) {
                String cellValue = String.valueOf(cell.getNumericCellValue());
                if (cellValue.contains(".0")) {
                    cellValue = cellValue.split("\\.")[0];
                }
                if (DateUtil.isCellDateFormatted(cell)) {
                    DateFormat df = new SimpleDateFormat("dd/MM/yy");
                    Date date = cell.getDateCellValue();
                    cellValue = df.format(date);
                }
                return cellValue;
            } else if (cell.getCellType() == CellType.BOOLEAN) {
                return String.valueOf(cell.getBooleanCellValue());
            } else {
                return "";
            }
        } catch (Exception e) {
            return "";
        }
    }

    /**
     * Extracts the first sheet name from the desired workbook.
     *
     * @return the first sheet name for the current test data file
     */
    private String getDefaultSheetName() {
        return workbook.getSheetName(0);
    }

}
