package com.shaft.io;

import java.io.FileInputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.poi.hssf.usermodel.HSSFDateUtil;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.testng.Assert;

public class ExcelFileManager {
    private FileInputStream fis = null;
    private XSSFWorkbook workbook = null;
    private XSSFSheet sheet = null;
    private XSSFRow row = null;
    private XSSFCell cell = null;

    private String testDataColumnNamePrefix = System.getProperty("testDataColumnNamePrefix");

    // private String currentFilePath;

    /**
     * Creates a new instance of the test data excel reader using the expected excel
     * file path
     * 
     * @param xlFilePath
     *            the expected path for the target excel file
     */
    public ExcelFileManager(String xlFilePath) {
	try {
	    fis = new FileInputStream(xlFilePath);
	    workbook = new XSSFWorkbook(fis);
	    fis.close();
	    // currentFilePath = xlFilePath;
	} catch (IOException e) {
	    ReportManager.log(e);
	    ReportManager.log("Couldn't find the desired file. [" + xlFilePath + "].");
	    Assert.fail("Couldn't find the desired file. [" + xlFilePath + "].");
	} catch (OutOfMemoryError e) {
	    ReportManager.log("Couldn't open the desired file. [" + xlFilePath + "].");
	    Assert.fail("Couldn't open the desired file. [" + xlFilePath + "].");
	    // t.printStackTrace();
	}
    }

    /**
     * Reads cell data from the first sheet in the desired excel workbook Reads cell
     * data using row name (1st column) only Reads cell data from the 2nd column
     * (1st Value in the test data file)
     * 
     * @param rowName
     *            the value of the first cell of the target row
     * @return the value of the target cell within the target row and the second
     *         column within the default sheet
     */
    public String getCellData(String rowName) {
	return getCellData(getDefaultSheetName(), rowName, "");
    }

    /**
     * Reads cell data from the first sheet in the desired excel workbook Reads cell
     * data using row name (1st column) and column name
     * 
     * @param rowName
     *            the value of the first cell of the target row
     * @param columnName
     *            the value of the first cell of the target column
     * @return the value of the target cell within the target row and column within
     *         the default sheet
     */
    public String getCellData(String rowName, String columnName) {
	return getCellData(getDefaultSheetName(), rowName, columnName);
    }

    /**
     * Reads cell data from a specific sheet name inside the excel file Reads cell
     * data using row name (1st column) and column name
     * 
     * @param sheetName
     *            the name of the target excel sheet
     * @param rowName
     *            the value of the first cell of the target row
     * @param columnName
     *            the value of the first cell of the target column
     * @return the value of the target cell within the target row and column within
     *         the target sheet
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
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    Assert.fail("Failed to read data from row [" + rowName + "] and column [" + columnName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    return "";
	}
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
	    ReportManager.log("Failed to get the row number that coresponds to rowName [" + rowName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    Assert.fail("Failed to get the row number that coresponds to rowName [" + rowName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    return -1; // in case of failure this line is unreachable
	} catch (Exception e) {
	    ReportManager.log(e);
	    ReportManager.log("Failed to get the row number that coresponds to rowName [" + rowName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    Assert.fail("Failed to get the row number that coresponds to rowName [" + rowName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
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
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    Assert.fail("Failed to get the column number that coresponds to columnName [" + columnName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    return -1; // in case of failure this line is unreachable
	} catch (Exception e) {
	    ReportManager.log(e);
	    ReportManager.log("Failed to get the column number that coresponds to columnName [" + columnName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    Assert.fail("Failed to get the column number that coresponds to columnName [" + columnName
		    + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
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
		if (HSSFDateUtil.isCellDateFormatted(cell)) {
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
	    // ReportManager.log(e);
	    // ReportManager.log("Failed to read data from row [" + rowName + "] and column
	    // [" + columnName
	    // + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    // Assert.fail("Failed to read data from row [" + rowName + "] and column [" +
	    // columnName
	    // + "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	    return "";
	}
    }

    /**
     * Returns the last column number that contains a header value (zero based)
     * assuming that you are working within the default sheet. That is if the last
     * column number is 3, it means that there are 4 columns.
     * 
     * @return the number of the last column that holds data in the default test
     *         data sheet
     */
    public int getLastColumnNumber() {
	return getLastColumnNumber(getDefaultSheetName());
    }

    /**
     * Returns the last column number that contains a header value (zero based).
     * That is if the last column number is 3, it means that there are 4 columns.
     * 
     * @param sheetName
     *            the name of the target excel sheet
     * @return the number of the last column that holds data in the target test data
     *         sheet
     */
    public int getLastColumnNumber(String sheetName) {
	sheet = workbook.getSheet(sheetName);
	row = sheet.getRow(0);
	int lastColumnNumber = 0;
	while (true) {
	    try {
		cell = row.getCell(lastColumnNumber);
		if (cell.getCellType() == CellType.STRING) {
		    // ReportManager.log("String Column Number is: [" + lastColumnNumber + "].");
		    lastColumnNumber++;
		} else {
		    return lastColumnNumber - 1;
		}
	    } catch (java.lang.NullPointerException e) {
		return lastColumnNumber - 1;
	    }
	}
    }

    //
    // /**
    // * Extracts the default sheet name from the file name, assuming that the sheet
    // * has the same name as the file.
    // *
    // * @return the default sheet name for the current test data file
    // */
    /**
     * Extracts the first sheet name from the desired workbook.
     * 
     * @return the first sheet name for the current test data file
     */
    private String getDefaultSheetName() {
	// return (currentFilePath.split(FileSystems.getDefault()
	// .getSeparator())[currentFilePath.split(FileSystems.getDefault().getSeparator()).length
	// - 1])
	// .split("\\.")[(currentFilePath.split(FileSystems.getDefault()
	// .getSeparator())[currentFilePath.split(FileSystems.getDefault().getSeparator()).length
	// - 1]).split("\\.").length
	// - 2];
	return workbook.getSheetName(0);
    }

    /**
     * Looks for the column name that holds the cellData in the specified rowName
     * and sheetName
     * 
     * @param sheetName
     *            the name of the target excel sheet
     * @param rowName
     *            the value of the first cell of the target row
     * @param cellData
     *            the value of the target cell within the target row
     * @return the value of the first cell of the target column
     */
    public String getColumnNameUsingRowNameAndCellData(String sheetName, String rowName, String cellData) {
	String columnName;
	for (int i = 1; i <= getLastColumnNumber(sheetName); i++) {
	    columnName = testDataColumnNamePrefix + String.valueOf(i);
	    if (cellData.equals(getCellData(rowName, columnName))) {
		return columnName;
	    }
	}
	ReportManager.log("Failed to get column name using row [" + rowName + "] and cell data [" + cellData
		+ "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	Assert.fail("Failed to get column name using row [" + rowName + "] and cell data [" + cellData
		+ "] in the Test Data Sheet [" + sheetName + ".xlsx].");
	return "";
    }

    /**
     * Looks for the column name that holds the cellData in the specified rowName
     * and the default sheet name
     * 
     * @param rowName
     *            the value of the first cell of the target row
     * @param cellData
     *            the value of the target cell within the target row
     * @return the value of the first cell of the target column
     */
    public String getColumnNameUsingRowNameAndCellData(String rowName, String cellData) {
	return getColumnNameUsingRowNameAndCellData(getDefaultSheetName(), rowName, cellData);
    }

}
