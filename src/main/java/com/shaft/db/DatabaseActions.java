package com.shaft.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import java.util.concurrent.Executors;

import org.testng.Assert;

import com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException;
import com.shaft.tools.io.ReportManager;

public class DatabaseActions {
    private DatabaseType dbType;
    private String dbServerIP;
    private String dbPort;
    private String dbName;
    private String username;
    private String password;

    public enum DatabaseType {
	MY_SQL, SQL_SERVER, POSTGRE_SQL, ORACLE;
    }

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
	    failAction("createDatabaseActionsObject",
		    "Database Type: \"" + databaseType + "\", IP: \"" + ip + "\", Port: \"" + port + "\", Name: \""
			    + name + "\", Username: \"" + username + "\", Password: \"" + password + "\"");
	}
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void passAction(String actionName, String testData, String queryResult) {
	String message = "Successfully performed action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	if (queryResult != null) {
	    ReportManager.attachAsStep("DB Response", "Query Result", queryResult);
	}
    }

    private static void passAction(String actionName, String testData) {
	passAction(actionName, testData, null);
    }

    private static void passAction(String actionName) {
	passAction(actionName, null, null);
    }

    private static void failAction(String actionName, String testData) {
	String message = "Failed to perform action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	Assert.fail(message);
    }

    private static void failAction(String actionName) {
	failAction(actionName, null);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private Connection createConnection() {
	Connection connection = null;
	String connectionString = "";
	try {
	    switch (dbType) {

	    case MY_SQL:
		connectionString = "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;
		break;

	    case SQL_SERVER:
		connectionString = "jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName;
		break;

	    case POSTGRE_SQL:
		connectionString = "jdbc:postgresql://" + dbServerIP + ":" + dbPort + "/" + dbName;
		break;

	    case ORACLE:
		connectionString = "jdbc:oracle:thin:@" + dbServerIP + ":" + dbPort + ":" + dbName;
		break;

	    default:
		ReportManager.log("Database not supported");
		failAction("createConnection", dbType.toString());
		break;
	    }
	    DriverManager.setLoginTimeout(Integer.parseInt(System.getProperty("databaseLoginTimeout")));
	    connection = DriverManager.getConnection(connectionString, username, password);
	    if (!dbType.toString().equals("MY_SQL") && !dbType.toString().equals("POSTGRE_SQL")) {
		// com.mysql.jdbc.JDBC4Connection.setNetworkTimeout
		// org.postgresql.jdbc4.Jdbc4Connection.setNetworkTimeout
		connection.setNetworkTimeout(Executors.newFixedThreadPool(1),
			Integer.parseInt(System.getProperty("databaseNetworkTimeout")) * 60000);
	    }
	} catch (SQLException e) {
	    ReportManager.log(e);
	    failAction("createConnection", connectionString);
	}

	if (connection != null) {
	    ReportManager.logDiscrete("Connection created successfully");
	} else {
	    failAction("createConnection", "Failed to create a connection with this string [" + connectionString
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
	} catch (SQLFeatureNotSupportedException e) {
	    if (!e.getMessage().contains("org.postgresql.jdbc4.Jdbc4Statement.setQueryTimeout")) {
		ReportManager.log(e);
		failAction("createStatement", connection.toString());
	    }
	} catch (SQLException e) {
	    ReportManager.log(e);
	    failAction("createStatement", connection.toString());
	}

	if (statement != null) {
	    ReportManager.logDiscrete("Statement created successfully");
	} else {
	    failAction("createStatement", "Failed to create a statement with this string [" + connection.toString()
		    + "] due to an unhandled exception.");
	}

	return statement;
    }

    private static String getResultStringValue(ResultSet resultSet, boolean readColumnNames) {
	StringBuilder str = new StringBuilder();
	try {
	    resultSet.beforeFirst();
	    if (resultSet.last()) {
		int columnsCount = resultSet.getMetaData().getColumnCount();
		int lastRowID = resultSet.getRow();
		if (readColumnNames) {
		    // read column headers
		    for (int i = 1; i <= columnsCount; i++) {
			str.append(String.valueOf(resultSet.getMetaData().getColumnName(i)) + "\t");
		    }
		    str.append("\n");
		}

		// read table data
		for (int i = 1; i <= lastRowID; i++) {
		    resultSet.absolute(i);
		    for (int j = 1; j <= columnsCount; j++) {
			str.append(String.valueOf(resultSet.getString(j)) + "\t");
		    }
		    str.append("\n");
		}
	    }
	} catch (SQLException | NullPointerException e) {
	    ReportManager.log(e);
	    failAction("getResultStringValue");
	}
	return str.toString().trim();
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core Database Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Executes a SELECT statement and returns the result as a ResultSet object
     * 
     * @param sql an SQL statement to be sent to the database, typically a static
     *            SQL SELECT statement
     * @return a ResultSet object that contains the data produced by the given
     *         query; never null
     * 
     */
    public ResultSet executeSelectQuery(String sql) {
	ResultSet resultSet = null;
	try {
	    resultSet = createStatement(createConnection()).executeQuery(sql);
	} catch (MySQLSyntaxErrorException e) {
	    ReportManager.log(e);
	    failAction("executeSelectQuery", "this query has a syntax error [" + sql + "]");
	} catch (SQLException | NullPointerException e) {
	    ReportManager.log(e);
	    failAction("executeSelectQuery", sql);
	}

	if (resultSet != null) {
	    passAction("executeSelectQuery", sql, getResultStringValue(resultSet, true));
	} else {
	    failAction("executeSelectQuery",
		    "Null or no resultSet was returned from executing this query [" + sql + "]");
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
     *         statements or (2) 0 for SQL statements that return nothing
     */
    public int executeUpdateQuery(String sql) {
	int updatedRows = 0;
	try {
	    updatedRows = createStatement(createConnection()).executeUpdate(sql);
	    passAction("executeUpdateQuery", sql);
	} catch (MySQLSyntaxErrorException e) {
	    ReportManager.log(e);
	    failAction("executeSelectQuery", "this query has a syntax error [" + sql + "]");
	} catch (SQLException e) {
	    ReportManager.log(e);
	    failAction("executeUpdateQuery", sql);
	}
	return updatedRows;
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
	passAction("getResult");
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
	StringBuilder str = new StringBuilder();
	Boolean foundRow = false;

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
			    str.append(String.valueOf(resultSet.getString(j)) + "\t");
			}
			str.append("\n");
			foundRow = true;
		    }
		}
	    }
	} catch (SQLException | NullPointerException e) {
	    ReportManager.log(e);
	    failAction("getRow", "columnName \"" + columnName + "\", and cellContent \"" + knownCellValue + "\"");
	}
	if (foundRow) {
	    passAction("getRow", "columnName \"" + columnName + "\", and cellContent \"" + knownCellValue + "\"");
	} else {
	    failAction("getRow", "columnName \"" + columnName + "\", and cellContent \"" + knownCellValue + "\"");
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
		    str.append(String.valueOf(resultSet.getString(targetColumnID)) + "\n");
		}
	    }
	} catch (SQLException | NullPointerException e) {
	    ReportManager.log(e);
	    failAction("getColumn");
	}
	passAction("getColumn", columnName);
	return str.toString().trim();
    }

    /**
     * Returns the number of rows contained inside the provided resultSet
     * 
     * @param resultSet the object returned as a result of performing a certain
     *                  database query
     * @return an integer value which represents the number of rows contained inside
     *         the provided resultSet
     */
    public static int getRowCount(ResultSet resultSet) {
	int rowCount = 0;
	try {
	    resultSet.beforeFirst();
	    if (resultSet.last()) {
		rowCount = resultSet.getRow();
		resultSet.beforeFirst(); // reset pointer
	    }
	} catch (SQLException e) {
	    ReportManager.log(e);
	    failAction("getRowCount");
	}
	passAction("getRowCount");
	return rowCount;
    }

}