package com.shaft.tools.io;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.logging.log4j.Level;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
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
     * @param csvFilePath target test data Excel file path
     */
    public CSVFileManager(String csvFilePath){
        initializeVariables();
        this.csvFilePath =csvFilePath;
        try {
            reader = new FileReader(csvFilePath);
            records = CSVFormat.DEFAULT.withFirstRecordAsHeader().parse(reader);
            RowReader = new FileReader(csvFilePath);
            RowRecords = CSVFormat.DEFAULT.withFirstRecordAsHeader().parse(RowReader);
            ReportManager.log("Reading test data from the following file. ["+this.csvFilePath +"]",Level.INFO);
        } catch (IOException | OutOfMemoryError e) {
            FailureReporter.fail(this.getClass(),"Couldn't find the desired file. [" + this.csvFilePath + "] ",e);
            ReportManager.log("Couldn't find the desired file. [" + this.csvFilePath + "] ", Level.ERROR);
            e.printStackTrace();
        }
        List<List<Object>> attachments = new ArrayList<>();
        List<Object> testDataFileAttachment = null;
        try {
            testDataFileAttachment = Arrays.asList("Test Data", "CSV",
                    new FileInputStream(this.csvFilePath));
            ReportManagerHelper.log("Loaded Test Data: [" + this.csvFilePath + "].", attachments);
        } catch (FileNotFoundException e) {
            //unreachable code because if the file was not found then the reader would have failed at a previous step
            ReportManager.log("Test data file not found for attachment: [" + this.csvFilePath + "]",Level.ERROR);
        }
        attachments.add(testDataFileAttachment);
        ReportManagerHelper.log("Loaded Test Data: [" + this.csvFilePath + "].", attachments);
    }
    public List<String[]> GetRows(){
        rows = new ArrayList<>();
        try {
            for (CSVRecord record : RowRecords) {
                String[] row = new String[record.size()];
                for (int i = 0; i < record.size(); i++) {
                    row[i] = record.get(i);
                }
                rows.add(row);
            }
            ReportManager.log("Successfully retrieved all rows from ["+csvFilePath+"].",Level.INFO);
        } catch (Exception e) {
            ReportManager.log("Error while retrieving rows: " + e.getMessage(),Level.ERROR);
        }
        return rows;
    }

    public List<String> GetColumns(){
        try {
            List<String> columns = new ArrayList<>(records.getHeaderNames());
            ReportManager.log("Successfully retrieved column names from ["+csvFilePath+"].",Level.INFO);
            return columns;
        } catch (Exception e) {
            ReportManager.log("Error while retrieving columns: " + e.getMessage(),Level.ERROR);
            return Collections.emptyList();
        }
    }
    public Map<String, List<String>> GetColumnsWithData() {
        ColumnWithRows = new HashMap<>();
        try {
            List<String[]> rows = GetRows();
            List<String> columns = GetColumns();

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
            ReportManager.log("Error while mapping columns with data: " + e.getMessage(),Level.ERROR);
        }
        return ColumnWithRows;
    }
    public String GetLastColumn(){
        try {
            List<String> columns = GetColumns();
            return columns.getLast();
        } catch (Exception e) {
            ReportManager.log("Error while retrieving the last column: " + e.getMessage(),Level.ERROR);
            return null;
        }
    }
    public String GetSpecificColumnName(int ColumnNum){
        try {
            return GetColumns().get(ColumnNum - 1);
        } catch (Exception e) {
            ReportManager.log("Error while retrieving column name at index " + ColumnNum + ": " + e.getMessage(),Level.ERROR);
            return null;
        }
    }
    public List<String> GetSpecificColumnData(String ColumnName){
        try {
            return GetColumnsWithData().get(ColumnName);
        } catch (Exception e) {
            ReportManager.log("Error while retrieving data for column: " + ColumnName + ". " + e.getMessage(),Level.ERROR);
            return Collections.emptyList();
        }
    }
    public String getCellData(int RowNum, String ColumnName) {
        try {
            String[] row = GetRows().get(RowNum);
            List<String> columns = GetColumns();
            return row[columns.indexOf(ColumnName)];
        } catch (Exception e) {
            ReportManager.log("Error while retrieving cell data for Row: " + RowNum + ", Column: " + ColumnName + ". " + e.getMessage(),Level.ERROR);
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
