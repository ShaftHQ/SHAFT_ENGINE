package com.shaft.db;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;

import javax.sql.rowset.CachedRowSet;
import javax.sql.rowset.RowSetProvider;
import java.sql.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;

@SuppressWarnings("unused")
public class DatabaseActions {
    private final ThreadLocal<ResultSet> resultSetThreadLocal = new ThreadLocal<>();
    private final ThreadLocal<Integer> rowCountThreadLocal = new ThreadLocal<>();
    private DatabaseType dbType;
    private String dbServerIP;
    private String dbPort;
    private String dbName;
    private String username;
    private String password;
    private String customConnectionString = "";

    /**
     * This constructor is used for initializing the database variables that are
     * needed to create new connections and perform queries
     *
     * @param databaseType database type that you want to connect with:
     *                     DatabaseType.MY_SQL ,SQL_SERVER,POSTGRES_SQL.
     * @param ip           IP address that has database installation that we need to
     *                     connect to (e.g. 72.55.136.25)
     * @param port         port of database installation on the server (e.g. 3306)
     * @param name         database name that you need to connect to
     * @param username     database username
     * @param password     password of database user
     */
    public DatabaseActions(DatabaseType databaseType, String ip, String port, String name, String username,
                           String password) {
        if (!ip.isEmpty() && !port.isEmpty() && !name.isEmpty() && !username.isEmpty() && !password.isEmpty()) {
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
     * This constructor is used for initializing the database variables that are
     * needed to create new connections and perform queries
     *
     * @param customConnectionString custom database connection string ex: "jdbc:oracle:thin:@dbServerIP:dbPort:dbName"
     */
    public DatabaseActions(String customConnectionString) {
        if (!"".equals(customConnectionString)) {
            this.customConnectionString = customConnectionString;
        } else {
            failAction("Custom Connection String: \"" + customConnectionString + "\"");
        }
    }

    public static DatabaseActions getInstance(DatabaseType databaseType, String ip, String port, String name, String username,
                                              String password) {
        return new DatabaseActions(databaseType, ip, port, name, username, password);
    }

    public static DatabaseActions getInstance(String customConnectionString) {
        return new DatabaseActions(customConnectionString);
    }

    /**
     * Returns a string representation of the provided resultSet object
     *
     * @param resultSet the object returned as a result of performing a certain
     *                  database query
     * @return a string value which represents the provided resultSet object
     */
    public static String getResult(ResultSet resultSet) {
        var resultSetString = getResultStringValue(resultSet, false);
        passAction();
        return resultSetString;
    }

    private static void failAction(String actionName, String testData, Exception... rootCauseException) {
        String message = reportActionResult(actionName, testData, null, false, rootCauseException);
        FailureReporter.fail(DatabaseActions.class, message, rootCauseException[0]);
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
        var rowCount = 0;
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                rowCount = resultSet.getRow();
                resultSet.beforeFirst(); // reset pointer
            }
        } catch (SQLException rootCauseException) {
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

    private static void failAction(String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, rootCauseException);
    }

    private static void failAction(Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, null, rootCauseException);
    }

    private static String reportActionResult(String actionName, String testData, String queryResult,
                                             Boolean passFailStatus, Exception... rootCauseException) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Database Action \"" + actionName + "\" successfully performed.";
        } else {
            message = "Database Action \"" + actionName + "\" failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Database Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data \"" + testData + "\".";
        }

        if (queryResult != null && !queryResult.trim().isEmpty()) {
            attachments.add(Arrays.asList("Database Action Actual Result", "Query Result", queryResult));
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("Database Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        if (!attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    private static StringBuilder readColumnHeaders(ResultSet resultSet, boolean readColumnNames, int columnsCount)
            throws SQLException {
        var str = new StringBuilder();
        if (readColumnNames) {
            for (var i = 1; i <= columnsCount; i++) {
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
        var str = new StringBuilder();
        for (var i = 1; i <= lastRowID; i++) {
            resultSet.absolute(i);
            for (var j = 1; j <= columnsCount; j++) {
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
        var str = new StringBuilder();
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
            failAction(rootCauseException);
        }
        return str.toString().trim();
    }

    public String getResult() {
        return DatabaseActions.getResult(resultSetThreadLocal.get());
    }

    public String getRow(String columnName, String knownCellValue) {
        return DatabaseActions.getRow(resultSetThreadLocal.get(), columnName, knownCellValue);
    }

    public String getColumn(String columnName) {
        return DatabaseActions.getColumn(resultSetThreadLocal.get(), columnName);
    }

    public int getRowCount() {
        return rowCountThreadLocal.get();
    }

    private void setRowCountForSelectStatement(ResultSet resultSet) {
        var rowCount = 0;
        try {
            resultSet.beforeFirst();
            if (resultSet.last()) {
                rowCount = resultSet.getRow();
                resultSet.beforeFirst(); // reset pointer
            }
        } catch (SQLException rootCauseException) {
            failAction(rootCauseException);
        }
        rowCountThreadLocal.set(rowCount);
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
        try (var connection = createConnection()) {
            resultSet = createStatement(connection).executeQuery(sql);
            if (resultSet != null) {
                CachedRowSet crs = RowSetProvider.newFactory().createCachedRowSet();
                crs.populate(resultSet);
                resultSetThreadLocal.set(crs);
                setRowCountForSelectStatement(crs);
                passAction(getReportMessage("SELECT", sql), getResultStringValue(crs, true));
            } else {
                failAction("Null or no resultSet was returned from executing this query \"" + sql + "\"");
            }
        } catch (SQLException | NullPointerException rootCauseException) {
            failAction(getReportMessage("SELECT", sql), rootCauseException);
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
    private int executeDataManipulationQueries(String sql, String queryType) {
        var affectedRows = 0;
        try (var connection = createConnection()) {
            affectedRows = createStatement(connection).executeUpdate(sql);
            passAction(sql);
        } catch (SQLException | NullPointerException rootCauseException) {
            failAction(getReportMessage(queryType, sql), rootCauseException);
        }
        rowCountThreadLocal.set(affectedRows);
        return affectedRows;
    }

    /**
     * Executes any DML or DDL statement and returns the result as a ResultSet
     * object
     *
     * @param sql an SQL Data Manipulation Language (DML) ;UPDATE statement,
     *            or an SQL statement that returns nothing, such
     *            as a DDL statement.
     * @return either (1) the row count for SQL Data Manipulation Language (DML)
     * statements or (2) 0 for SQL statements that return nothing
     */
    public int executeUpdateQuery(String sql) {
        return executeDataManipulationQueries(sql, "UPDATE");
    }

    public void executeDDLStatement(String sql) {
        executeDataManipulationQueries(sql, "DDL");
    }

    /**
     * Executes any DML or DDL statement and returns the result as a ResultSet
     * object
     *
     * @param sql an SQL Data Manipulation Language (DML) ;INSERT statement,
     *            or an SQL statement that returns nothing, such
     *            as a DDL statement.
     * @return either (1) the row count for SQL Data Manipulation Language (DML)
     * statements or (2) 0 for SQL statements that return nothing
     */
    public int executeInsertQuery(String sql) {
        return executeDataManipulationQueries(sql, "INSERT");

    }

    /**
     * Executes any DML or DDL statement and returns the result as a ResultSet
     * object
     *
     * @param sql an SQL Data Manipulation Language (DML) ;DELETE statement,
     *            or an SQL statement that returns nothing, such
     *            as a DDL statement.
     * @return either (1) the row count for SQL Data Manipulation Language (DML)
     * statements or (2) 0 for SQL statements that return nothing
     */
    public int executeDeleteQuery(String sql) {
        return executeDataManipulationQueries(sql, "DELETE");
    }

    private Connection createConnection() {
        Connection connection = null;
        var connectionString = "";
        if (!this.customConnectionString.isEmpty()) {
            connectionString = this.customConnectionString;
        } else {
            switch (dbType) {
                case MY_SQL -> connectionString = "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;
                case SQL_SERVER ->
                        connectionString = "jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName;
                case POSTGRES_SQL -> connectionString = "jdbc:postgresql://" + dbServerIP + ":" + dbPort + "/" + dbName;
                case ORACLE -> connectionString = "jdbc:oracle:thin:@" + dbServerIP + ":" + dbPort + ":" + dbName;
                case ORACLE_SERVICE_NAME ->
                        connectionString = "jdbc:oracle:thin:@" + dbServerIP + ":" + dbPort + "/" + dbName;
                case IBM_DB2 -> connectionString = "jdbc:db2://" + dbServerIP + ":" + dbPort + "/" + dbName;
                default -> {
                    ReportManager.log("Database not supported");
                    failAction(dbType.toString());
                }
            }
        }
        try {
            DriverManager.setLoginTimeout(SHAFT.Properties.timeouts.databaseLoginTimeout());
            connection = DriverManager.getConnection(connectionString, username, password);
            if (!dbType.toString().equals("MY_SQL") && !dbType.toString().equals("POSTGRES_SQL")) {
                // com.mysql.jdbc.JDBC4Connection.setNetworkTimeout
                // org.postgresql.jdbc4.Jdbc4Connection.setNetworkTimeout
                connection.setNetworkTimeout(Executors.newFixedThreadPool(1), SHAFT.Properties.timeouts.databaseNetworkTimeout() * 60000);
            }
        } catch (SQLException rootCauseException) {
            failAction(connectionString, rootCauseException);
        }

        if (connection != null) {
            ReportManager.logDiscrete("Connection created successfully");
        } else {
            failAction("Failed to create a connection with this string \"" + connectionString
                    + "\" due to an unhandled exception.");
        }
        return connection;
    }

    private Statement createStatement(Connection connection) {
        Statement statement = null;
        try {
            statement = connection.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
            // https://www.tutorialspoint.com/jdbc/jdbc-result-sets.htm
            statement.setQueryTimeout(SHAFT.Properties.timeouts.databaseQueryTimeout());
        } catch (SQLFeatureNotSupportedException rootCauseException) {
            if (!rootCauseException.getMessage().contains("org.postgresql.jdbc4.Jdbc4Statement.setQueryTimeout")) {
                failAction(connection.toString(), rootCauseException);
            }
        } catch (SQLException rootCauseException) {
            failAction(connection.toString(), rootCauseException);
        }

        if (statement != null) {
            ReportManager.logDiscrete("Statement created successfully");
        } else {
            failAction("Failed to create a statement with this string \"" + connection
                    + "\" due to an unhandled exception.");
        }

        return statement;
    }

    private String getReportMessage(String queryType, String query) {
        //noinspection SuspiciousRegexArgument
        return "Database Type: \"" + dbType + "\"" +
                "| Server: \"" + dbServerIP + ":" + dbPort + "\"" +
                "| Name: \"" + dbName + "\"" +
                "| Username: \"" + username + "\"" +
                "| Password: \"" + password.replaceAll(".", "*") + "\"" +
                "| Query Type: \"" + queryType + "\"" +
                "| Query: \"" + query + "\"";
    }

    public enum DatabaseType {
        MY_SQL, SQL_SERVER, POSTGRES_SQL, ORACLE, ORACLE_SERVICE_NAME, IBM_DB2
    }

}