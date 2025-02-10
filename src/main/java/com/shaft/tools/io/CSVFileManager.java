package com.shaft.tools.io;
import com.shaft.driver.DriverFactory;
import com.shaft.tools.io.internal.FailureReporter;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.logging.log4j.Level;

import java.io.FileReader;
import java.io.IOException;
import java.util.*;

@SuppressWarnings("unused")
public class CSVFileManager {
    private FileReader reader;
    private List<String[]> rows;
    private Map<String,List<String>> ColumnWithRows;
    private CSVParser records;
    private String csvFilePath;
    private FileReader RowReader;
    private Iterable<CSVRecord> RowRecords;


    /**
     * Creates a new instance of the test data CSV reader using the target CSV
     * file path
     *
     * @param csvFilePath target test data CSV file path
     */
    public CSVFileManager(String csvFilePath){
        DriverFactory.reloadProperties();
        initializeVariables();
        this.csvFilePath =csvFilePath;
        try {
            reader = new FileReader(csvFilePath);
            records = CSVFormat.DEFAULT.withFirstRecordAsHeader().parse(reader);
            RowReader = new FileReader(csvFilePath);
            RowRecords = CSVFormat.DEFAULT.withFirstRecordAsHeader().parse(RowReader);
            ReportManager.logDiscrete("Reading test data from the following file. ["+this.csvFilePath +"]",Level.INFO);
        } catch (IOException | OutOfMemoryError e) {
            FailureReporter.fail(this.getClass(),"Couldn't find the desired file. [" + this.csvFilePath + "] ",e);
            ReportManager.logDiscrete("Couldn't find the desired file. [" + this.csvFilePath + "] ", Level.ERROR);
        }

    }
    /**
     * Retrieves all rows from the CSV file as a list of string arrays.
     * Each row is represented as an array of strings.
     *
     * @return a list of string arrays, where each array represents a row in the CSV file.
     */
    public List<String[]> getRows(){
        rows = new ArrayList<>();
        try {
            for (CSVRecord record : RowRecords) {
                String[] row = new String[record.size()];
                for (int i = 0; i < record.size(); i++) {
                    row[i] = record.get(i);
                }
                rows.add(row);
            }
            ReportManager.logDiscrete("Successfully retrieved all rows from ["+csvFilePath+"].",Level.INFO);
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving rows: " + e.getMessage(),Level.ERROR);
        }
        return rows;
    }
    /**
     * Retrieves the column names from the CSV file.
     *
     * @return a list of column names as strings.
     */
    public List<String> getColumns(){
        try {
            List<String> columns = new ArrayList<>(records.getHeaderNames());
            ReportManager.logDiscrete("Successfully retrieved column names from ["+csvFilePath+"].",Level.INFO);
            return columns;
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving columns: " + e.getMessage(),Level.ERROR);
            return Collections.emptyList();
        }
    }
    /**
     * Maps each column name to its corresponding list of row data.
     *
     * @return a map where keys are column names and values are lists of column data.
     */
    public Map<String, List<String>> getColumnsWithData() {
        ColumnWithRows = new HashMap<>();
        try {
            List<String[]> rows = getRows();
            List<String> columns = getColumns();

            for (int i = 0; i < columns.size(); i++) {
                List<String> columnData = new ArrayList<>();
                for (String[] row : rows) {
                    if (i < row.length) {
                        columnData.add(row[i]);
                    }
                }
                ColumnWithRows.put(columns.get(i), columnData);
            }
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while mapping columns with data: " + e.getMessage(),Level.ERROR);
        }
        return ColumnWithRows;
    }
    /**
     * Retrieves the name of the last column in the CSV file.
     *
     * @return the name of the last column, or null if an error occurs.
     */
    public String getLastColumn(){
        try {
            List<String> columns = getColumns();
            return columns.getLast();
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving the last column: " + e.getMessage(),Level.ERROR);
            return null;
        }
    }
    /**
     * Retrieves the name of a specific column based on its index.
     *
     * @param ColumnNum the 1-based index of the column.
     * @return the column name at the specified index, or null if an error occurs.
     */
    public String getSpecificColumnName(int ColumnNum){
        try {
            return getColumns().get(ColumnNum - 1);
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving column name at index " + ColumnNum + ": " + e.getMessage(),Level.ERROR);
            return null;
        }
    }
    /**
     * Retrieves all data for a specific column.
     *
     * @param ColumnName the name of the column.
     * @return a list of strings containing the column data, or an empty list if an error occurs.
     */
    public List<String> getSpecificColumnData(String ColumnName){
        try {
            return getColumnsWithData().get(ColumnName);
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving data for column: " + ColumnName + ". " + e.getMessage(),Level.ERROR);
            return Collections.emptyList();
        }
    }
    /**
     * Retrieves all data for a specific column.
     *
     * @param ColumnIndex the 1-based index of the column.
     * @return a list of strings containing the column data, or an empty list if an error occurs.
     */
    public List<String> getSpecificColumnData(int ColumnIndex){
        try {
            return getColumnsWithData().get(getSpecificColumnName(ColumnIndex-1));
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving data for column: " + getSpecificColumnName(ColumnIndex-1) + ". " + e.getMessage(),Level.ERROR);
            return Collections.emptyList();
        }
    }
    /**
     * Retrieves a specific cell's data based on row number and column name.
     *
     * @param RowNum the 0-based index of the row.
     * @param ColumnName the name of the column.
     * @return the data in the specified cell, or null if an error occurs.
     */
    public String getCellData(int RowNum, String ColumnName) {
        try {
            String[] row = getRows().get(RowNum);
            List<String> columns = getColumns();
            return row[columns.indexOf(ColumnName)];
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving cell data for Row: " + RowNum + ", Column: " + ColumnName + ". " + e.getMessage(),Level.ERROR);
            return null;
        }
    }
    /**
     * Retrieves a specific cell's data based on row number and column name.
     *
     * @param RowNum the 0-based index of the row.
     * @param ColumnIndex the 1-based index of the column.
     * @return the data in the specified cell, or null if an error occurs.
     */
    public String getCellData(int RowNum, int ColumnIndex) {
        try {
            String[] row = getRows().get(RowNum);
            List<String> columns = getColumns();
            return row[columns.indexOf(getSpecificColumnName(ColumnIndex-1))];
        } catch (Exception e) {
            ReportManager.logDiscrete("Error while retrieving cell data for Row: " + RowNum + ", Column: " + getSpecificColumnName(ColumnIndex-1) + ". " + e.getMessage(),Level.ERROR);
            return null;
        }
    }
    private void initializeVariables() {
        reader = null;
        RowReader = null;
        records =null;
        RowRecords=null;
        rows = null;
        ColumnWithRows=null;
        csvFilePath = "";
    }
}
