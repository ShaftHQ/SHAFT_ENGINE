package com.shaft.db;

import com.shaft.tools.io.ReportManagerHelper;

import java.sql.ResultSet;
import java.sql.SQLException;

public class ResultObject {
    ResultSet resultSet;

    public ResultObject(ResultSet resultSet) {
        this.resultSet= resultSet;
    }

    public  ResultSet getResult() {
        return resultSet;
    }
    public  String getString() {
        var resultSetString = getResultStringValue(false);
        DBReporter.passAction();
        return resultSetString;
    }

    public String getRow( String columnName, String knownCellValue) {
        String reportMessage = "Column Name: \"" + columnName + "\" | Cell Content: \"" + knownCellValue + "\"";
        var str = new StringBuilder();
        var foundRow = false;

        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                int columnsCount = resultSet.getMetaData().getColumnCount();
                int lastRowID = resultSet.getRow();
                int targetColumnID = resultSet.findColumn(columnName);

                // read table data
                for (var i = 1; i <= lastRowID; i++) {
                    resultSet.absolute(i);
                    if (String.valueOf(resultSet.getString(targetColumnID)).trim().equals(knownCellValue.trim())) {
                        for (var j = 1; j <= columnsCount; j++) {
                            str.append(resultSet.getString(j)).append("\t");
                        }
                        str.append("\n");
                        foundRow = true;
                    }
                }
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(reportMessage, rootCauseException);
        }
        if (Boolean.TRUE.equals(foundRow)) {
            DBReporter.passAction(reportMessage);
        } else {
            DBReporter.failAction(reportMessage);
        }
        return str.toString().trim();
    }

    public int getRowCount() {
        var rowCount = 0;
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                rowCount = resultSet.getRow();
                resultSet.beforeFirst(); // reset pointer
            }
        } catch (SQLException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(rootCauseException);
        }
        DBReporter.passAction();
        return rowCount;
    }

    public String getColumn( String columnName) {
        var str = new StringBuilder();
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                int lastRowID = resultSet.getRow();
                int targetColumnID = resultSet.findColumn(columnName);
                // read table data
                for (var i = 1; i <= lastRowID; i++) {
                    resultSet.absolute(i);
                    str.append(resultSet.getString(targetColumnID)).append("\n");
                }
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(rootCauseException);
        }
        DBReporter.passAction(columnName);
        return str.toString().trim();
    }

    private StringBuilder readColumnHeaders(ResultSet resultSet, boolean readColumnNames, int columnsCount)
            throws SQLException {
        var str = new StringBuilder();
        if (readColumnNames) {
            for (var i = 1; i <= columnsCount; i++) {
                str.append(this.resultSet.getMetaData().getColumnName(i));
                if (i != columnsCount) {
                    str.append("\t");
                }
            }
            str.append("\n");
        }
        return str;
    }

    private StringBuilder readColumnData(ResultSet resultSet, int columnsCount, int lastRowID)
            throws SQLException {
        var str = new StringBuilder();
        for (var i = 1; i <= lastRowID; i++) {
            this.resultSet.absolute(i);
            for (var j = 1; j <= columnsCount; j++) {
                str.append(this.resultSet.getString(j));
                if (j != columnsCount) {
                    str.append("\t");
                }
            }
            str.append("\n");
        }
        return str;
    }

     String getResultStringValue( boolean readColumnNames) {
        var str = new StringBuilder();
        try {
            this.resultSet.beforeFirst();
            if (this.resultSet.last()) {
                int columnsCount = this.resultSet.getMetaData().getColumnCount();
                int lastRowID = this.resultSet.getRow();
                // read column headers
                str.append(readColumnHeaders(this.resultSet, readColumnNames, columnsCount));
                // read table data
                str.append(readColumnData(this.resultSet, columnsCount, lastRowID));
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(rootCauseException);
        }
        return str.toString().trim();
    }

}
