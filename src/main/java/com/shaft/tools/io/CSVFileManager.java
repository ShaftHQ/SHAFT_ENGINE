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
            FailureReporter.fail("Error while retrieving rows: " + e.getMessage());
        }
        return rows;
    }

    public List<String> getColumns(){
        try {
            List<String> columns = new ArrayList<>(records.getHeaderNames());
            ReportManager.logDiscrete("Successfully retrieved column names from ["+csvFilePath+"].",Level.INFO);
            return columns;
        } catch (Exception e) {
            FailureReporter.fail("Error while retrieving columns: " + e.getMessage());
            return Collections.emptyList();
        }
    }
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
            FailureReporter.fail("Error while mapping columns with data: " + e.getMessage());
        }
        return ColumnWithRows;
    }
    public String getLastColumn(){
        try {
            List<String> columns = getColumns();
            return columns.getLast();
        } catch (Exception e) {
            FailureReporter.fail("Error while retrieving the last column: " + e.getMessage());
            return null;
        }
    }
    public String getSpecificColumnName(int ColumnNum){
        try {
            return getColumns().get(ColumnNum - 1);
        } catch (Exception e) {
            FailureReporter.fail("Error while retrieving column name at index " + ColumnNum + ": " + e.getMessage());
            return null;
        }
    }
    public List<String> getSpecificColumnData(String ColumnName){
        try {
            return getColumnsWithData().get(ColumnName);
        } catch (Exception e) {
            FailureReporter.fail("Error while retrieving data for column: " + ColumnName + ". " + e.getMessage());
            return Collections.emptyList();
        }
    }
    public String getCellData(int RowNum, String ColumnName) {
        try {
            String[] row = getRows().get(RowNum);
            List<String> columns = getColumns();
            return row[columns.indexOf(ColumnName)];
        } catch (Exception e) {
            FailureReporter.fail("Error while retrieving cell data for Row: " + RowNum + ", Column: " + ColumnName + ". " + e.getMessage());
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
