package com.shaft.db;

import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.Assert;

import java.sql.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;

@SuppressWarnings("unused")
public class DatabaseActions {
    private DatabaseType dbType;
    private String dbServerIP;
    private String dbPort;
    private String dbName;
    private String username;
    private String password;

    /**
     * This constructor is used for initializing the database variables that are
     * needed to create new connections and perform queries
     *
     * @param databaseType database type that you want to connect with:
     *                     DatabaseType.MY_SQL ,SQL_SERVER,POSTGRE_SQL.
     * @param ip           IP address that has database installation that we need to
     *                     connect to (e.g. 72.55.136.25)
     * @param port         port of database installation on the server (e.g. 3306)
     * @param name         database name that you need to connect to
     * @param username     database username
     * @param password     password of database user
     */
    public DatabaseActions(DatabaseType databaseType, String ip, String port, String name, String username,
                           String password) {
        if (!ip.equals("") && !port.equals("") && !name.equals("") && !username.equals("") && !password.equals("")) {
            this.dbType = databaseType;
            this.dbServerIP = ip;
            this.dbPort = port;
            this.dbName = name;
            this.username = username;
            this.password = password;
        } else {
            failAction("Database Type: \"" + databaseType + "\", IP: \"" + ip + "\", Port: \"" + port + "\", Name: \""
                    + name + "\", Username: \"" + username + "\", Password: \"" + password + "\"");
        }
    }

    /**
     * Returns a string representation of the provided resultSet object
     *
     * @param resultSet the object returned as a result of performing a certain
     *                  database query
     * @return a string value which represents the provided resultSet object
     */
    public static String getResult(ResultSet resultSet) {
        String resultSetString = getResultStringValue(resultSet, false);
        passAction();
        return resultSetString;
    }

    /**
     * Returns a string value which represents the data of the target row
     *
     * @param resultSet      the object returned as a result of performing a certain
     *                       database query
     * @param columnName     the name of the column holding the knownCellValue
     * @param knownCellValue a value that the engine searches for under the
     *                       specified columnName, when that value is found, the row
     *                       that contains it is read and added to the returned
     *                       string
     * @return a string value which represents the data of the target row
     */
    public static String getRow(ResultSet resultSet, String columnName, String knownCellValue) {
        String reportMessage = "Column Name: \"" + columnName + "\" | Cell Content: \"" + knownCellValue + "\"";
        StringBuilder str = new StringBuilder();
        boolean foundRow = false;

        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                int columnsCount = resultSet.getMetaData().getColumnCount();
                int lastRowID = resultSet.getRow();
                int targetColumnID = resultSet.findColumn(columnName);

                // read table data
                for (int i = 1; i <= lastRowID; i++) {
                    resultSet.absolute(i);
                    if (String.valueOf(resultSet.getString(targetColumnID)).trim().equals(knownCellValue.trim())) {
                        for (int j = 1; j <= columnsCount; j++) {
                            str.append(resultSet.getString(j)).append("\t");
                        }
                        str.append("\n");
                        foundRow = true;
                    }
                }
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(reportMessage, rootCauseException);
        }
        if (Boolean.TRUE.equals(foundRow)) {
            passAction(reportMessage);
        } else {
            failAction(reportMessage);
        }
        return str.toString().trim();
    }

    /**
     * Returns a string value which represents the data of the target column
     *
     * @param resultSet  the object returned as a result of performing a certain
     *                   database query
     * @param columnName the name of the target column that will be read
     * @return a string value which represents the data of the target column
     */
    public static String getColumn(ResultSet resultSet, String columnName) {
        StringBuilder str = new StringBuilder();
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                int lastRowID = resultSet.getRow();
                int targetColumnID = resultSet.findColumn(columnName);

                // read table data
                for (int i = 1; i <= lastRowID; i++) {
                    resultSet.absolute(i);
                    str.append(resultSet.getString(targetColumnID)).append("\n");
                }
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(rootCauseException);
        }
        passAction(columnName);
        return str.toString().trim();
    }

    /**
     * Returns the number of rows contained inside the provided resultSet
     *
     * @param resultSet the object returned as a result of performing a certain
     *                  database query
     * @return an integer value which represents the number of rows contained inside
     * the provided resultSet
     */
    public static int getRowCount(ResultSet resultSet) {
        int rowCount = 0;
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                rowCount = resultSet.getRow();
                resultSet.beforeFirst(); // reset pointer
            }
        } catch (SQLException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(rootCauseException);
        }
        passAction();
        return rowCount;
    }

    private static void passAction(String actionName, String testData, String queryResult) {
        reportActionResult(actionName, testData, queryResult, true);
    }

    private static void passAction(String testData, String queryResult) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, queryResult);
    }

    private static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, null);
    }

    private static void passAction() {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, null, null);
    }

    private static void failAction(String actionName, String testData, Exception... rootCauseException) {
        String message = reportActionResult(actionName, testData, null, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static void failAction(String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, rootCauseException);
    }

    private static void failAction(Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, null, rootCauseException);
    }

    private static String reportActionResult(String actionName, String testData, String queryResult,
                                             Boolean passFailStatus) {
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Database Action [" + actionName + "] successfully performed.";
        } else {
            message = "Database Action [" + actionName + "] failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty() && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Database Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }

        if (queryResult != null && !queryResult.trim().equals("")) {
            attachments.add(Arrays.asList("Database Action Actual Result", "Query Result", queryResult));
        }

        if (!attachments.equals(new ArrayList<>())) {
            ReportManager.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    private static StringBuilder readColumnHeaders(ResultSet resultSet, boolean readColumnNames, int columnsCount)
            throws SQLException {
        StringBuilder str = new StringBuilder();
        if (readColumnNames) {
            for (int i = 1; i <= columnsCount; i++) {
                str.append(resultSet.getMetaData().getColumnName(i));
                if (i != columnsCount) {
                    str.append("\t");
                }
            }
            str.append("\n");
        }
        return str;
    }

    private static StringBuilder readColumnData(ResultSet resultSet, int columnsCount, int lastRowID)
            throws SQLException {
        StringBuilder str = new StringBuilder();
        for (int i = 1; i <= lastRowID; i++) {
            resultSet.absolute(i);
            for (int j = 1; j <= columnsCount; j++) {
                str.append(resultSet.getString(j));
                if (j != columnsCount) {
                    str.append("\t");
                }
            }
            str.append("\n");
        }
        return str;
    }

    private static String getResultStringValue(ResultSet resultSet, boolean readColumnNames) {
        StringBuilder str = new StringBuilder();
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                int columnsCount = resultSet.getMetaData().getColumnCount();
                int lastRowID = resultSet.getRow();

                // read column headers
                str.append(readColumnHeaders(resultSet, readColumnNames, columnsCount));

                // read table data
                str.append(readColumnData(resultSet, columnsCount, lastRowID));
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(rootCauseException);
        }
        return str.toString().trim();
    }

    /**
     * Executes a SELECT statement and returns the result as a ResultSet object
     *
     * @param sql an SQL statement to be sent to the database, typically a static
     *            SQL SELECT statement
     * @return a ResultSet object that contains the data produced by the given
     * query; never null
     */
    public ResultSet executeSelectQuery(String sql) {
        ResultSet resultSet = null;
        try {
            resultSet = createStatement(createConnection()).executeQuery(sql);
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(getReportMessage("SELECT", sql), rootCauseException);
        }

        if (resultSet != null) {
            passAction(getReportMessage("SELECT", sql), getResultStringValue(resultSet, true));
        } else {
            failAction("Null or no resultSet was returned from executing this query [" + sql + "]");
        }

        return resultSet;
    }

    /**
     * Executes any DML or DDL statement and returns the result as a ResultSet
     * object
     *
     * @param sql an SQL Data Manipulation Language (DML) statement, such as INSERT,
     *            UPDATE or DELETE; or an SQL statement that returns nothing, such
     *            as a DDL statement.
     * @return either (1) the row count for SQL Data Manipulation Language (DML)
     * statements or (2) 0 for SQL statements that return nothing
     */
    public int executeUpdateQuery(String sql) {
        int updatedRows = 0;
        try {
            updatedRows = createStatement(createConnection()).executeUpdate(sql);
            passAction(sql);
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(getReportMessage("UPDATE", sql), rootCauseException);
        }
        return updatedRows;
    }

    private Connection createConnection() {
        Connection connection = null;
        String connectionString = "";
        try {
            switch (dbType) {
                case MY_SQL -> connectionString = "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;
                case SQL_SERVER -> connectionString = "jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName;
                case POSTGRE_SQL -> connectionString = "jdbc:postgresql://" + dbServerIP + ":" + dbPort + "/" + dbName;
                case ORACLE -> connectionString = "jdbc:oracle:thin:@" + dbServerIP + ":" + dbPort + ":" + dbName;
                case IBM_DB2 -> connectionString = "jdbc:db2://" + dbServerIP + ":" + dbPort + "/" + dbName;
                default -> {
                    ReportManager.log("Database not supported");
                    failAction(dbType.toString());
                }
            }
            if (System.getProperty("databaseLoginTimeout") == null) {
                PropertyFileManager.readPropertyFiles();
            }
            DriverManager.setLoginTimeout(Integer.parseInt(System.getProperty("databaseLoginTimeout")));
            connection = DriverManager.getConnection(connectionString, username, password);
            if (!dbType.toString().equals("MY_SQL") && !dbType.toString().equals("POSTGRE_SQL")) {
                // com.mysql.jdbc.JDBC4Connection.setNetworkTimeout
                // org.postgresql.jdbc4.Jdbc4Connection.setNetworkTimeout
                connection.setNetworkTimeout(Executors.newFixedThreadPool(1),
                        Integer.parseInt(System.getProperty("databaseNetworkTimeout")) * 60000);
            }
        } catch (SQLException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(connectionString, rootCauseException);
        }

        if (connection != null) {
            ReportManager.logDiscrete("Connection created successfully");
        } else {
            failAction("Failed to create a connection with this string [" + connectionString
                    + "] due to an unhandled exception.");
        }
        return connection;
    }

    private Statement createStatement(Connection connection) {
        Statement statement = null;
        try {
            statement = connection.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
            // https://www.tutorialspoint.com/jdbc/jdbc-result-sets.htm
            statement.setQueryTimeout(Integer.parseInt(System.getProperty("databaseQueryTimeout")));
        } catch (SQLFeatureNotSupportedException rootCauseException) {
            if (!rootCauseException.getMessage().contains("org.postgresql.jdbc4.Jdbc4Statement.setQueryTimeout")) {
                ReportManager.log(rootCauseException);
                failAction(connection.toString(), rootCauseException);
            }
        } catch (SQLException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(connection.toString(), rootCauseException);
        }

        if (statement != null) {
            ReportManager.logDiscrete("Statement created successfully");
        } else {
            failAction("Failed to create a statement with this string [" + connection.toString()
                    + "] due to an unhandled exception.");
        }

        return statement;
    }

    @SuppressWarnings("SuspiciousRegexArgument")
    private String getReportMessage(String queryType, String query) {
        return "Database Type: \"" + dbType + "\"" +
                "| Server: \"" + dbServerIP + ":" + dbPort + "\"" +
                "| Name: \"" + dbName + "\"" +
                "| Username: \"" + username + "\"" +
                "| Password: \"" + password.replaceAll(".", "*") + "\"" +
                "| Query Type: \"" + queryType + "\"" +
                "| Query: \"" + query + "\"";
    }

    public enum DatabaseType {
        MY_SQL, SQL_SERVER, POSTGRE_SQL, ORACLE, IBM_DB2
    }

}