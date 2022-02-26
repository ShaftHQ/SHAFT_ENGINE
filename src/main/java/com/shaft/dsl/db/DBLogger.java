package com.shaft.dsl.db;

import com.shaft.tools.io.ReportManager;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

public class DBLogger {
    protected static void logResultStatus(Connection connection, Statement statement) {
        if (statement != null) {
            ReportManager.logDiscrete("Statement created successfully");
        } else {
            DBReporter.failAction("Failed to create a statement with this string [" + connection + "] due to an unhandled exception.");
        }
    }
    protected static void logSelectStmtResultStatus(String sql, ResultSet resultSet) {
        if (resultSet != null) {
            DBReporter.passAction(getReportMessage("SELECT", sql), new ResultObject(resultSet).getResultStringValue( true));
        } else {
            DBReporter.failAction("Null or no resultSet was returned from executing this query [" + sql + "]");
        }
    }

    protected static void logConnectionStatus(DBConnection connection) {
        if (connection.connection != null) {
            ReportManager.logDiscrete("Connection created successfully");
        } else {
            DBReporter.failAction("Failed to create a connection with this string [" +  connection.connectionString + "] due to an unhandled exception.");
        }
    }

    protected static String getReportMessage(String queryType, String query) {
        return  " Query Type: \"" + queryType + "\"" + "| Query: \"" + query + "\"";
    }
}
