package com.shaft.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import java.util.concurrent.Executors;

import org.testng.Assert;

import com.shaft.tools.io.ReportManager;

/**
 * 
 * @author mennamaged
 *
 */

public class DatabaseActions {
    private String dbType;
    private String dbServerIP;
    private String dbPort;
    private String dbName;
    private String username;
    private String password;

    /**
     * This constructor is used for initializing database variables that needed to
     * create new connection
     * 
     * @param dbType     database type that you want to connect with:
     *                   MySQL,SqlServer,PostgreSql.
     * @param dbServerIP IP address that has database installation that we need to
     *                   connect to (e.g. 72.55.136.25)
     * @param dbPort     port of database installation on the server (e.g. 3306)
     * @param dbName     database name that you need to connect to
     * @param username   database username
     * @param password   password of database user
     */
    public DatabaseActions(String dbType, String dbServerIP, String dbPort, String dbName, String username,
	    String password) {
	this.dbType = dbType;
	this.dbServerIP = dbServerIP;
	this.dbPort = dbPort;
	this.dbName = dbName;
	this.username = username;
	this.password = password;
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
    //////////////////////////////////// [Public] Core Database Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Setup connection to database
     * 
     * 
     */

    private Connection createConnection() {
	Connection connection = null;
	String connectionString = "";
	try {
	    switch (dbType.toLowerCase().trim()) {

	    case ("mysql"):
		connectionString = "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;
		break;

	    case ("sqlserver"):
		connectionString = "jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName;
		break;

	    case ("postgresql"):
		connectionString = "jdbc:postgresql://" + dbServerIP + ":" + dbPort + "/" + dbName;
		break;

	    default:
		ReportManager.log("Database not supported");
		failAction("createConnection", dbType);
		break;
	    }
	    DriverManager.setLoginTimeout(Integer.parseInt(System.getProperty("databaseLoginTimeout")));
	    connection = DriverManager.getConnection(connectionString, username, password);

	    if (!dbType.toLowerCase().trim().equals("mysql") && !dbType.toLowerCase().trim().equals("postgresql")) {
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
	    ReportManager.logDiscrete("Connection is created successfully");
	} else {
	    failAction("createConnection", "Failed to create a connection with this string [" + connectionString
		    + "] due to an unhandled exception.");
	}
	return connection;
    }

    /**
     * prepare statement for database connection to perform execute query
     * 
     * 
     */

    private Statement createStatement(Connection connection) {
	Statement statement = null;
	try {
	    statement = connection.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
	    // https://www.tutorialspoint.com/jdbc/jdbc-result-sets.htm
	    statement.setQueryTimeout(Integer.parseInt(System.getProperty("databaseQueryTimeout")));
	} catch (SQLFeatureNotSupportedException e) {
	    if (!e.getMessage().contains("org.postgresql.jdbc4.Jdbc4Statement.setQueryTimeout")) {
		ReportManager.log(e);
		failAction("createConnection", connection.toString());
	    }
	} catch (SQLException e) {
	    ReportManager.log(e);
	    failAction("createStatement", connection.toString());
	}

	if (statement != null) {
	    ReportManager.logDiscrete("Statement is created successfully");
	} else {
	    failAction("createConnection", "Failed to create a statement with this string [" + connection.toString()
		    + "] due to an unhandled exception.");
	}

	return statement;
    }

    /**
     * execute DB query and return the result as ResultSet object
     * 
     * @param dbQuery
     * @return ResultSet
     * 
     *         return ResultSet
     */
    public ResultSet executeSelectQuery(String dbQuery) {
	ResultSet resultSet = null;
	try {
	    resultSet = createStatement(createConnection()).executeQuery(dbQuery);
	} catch (SQLException | NullPointerException e) {
	    ReportManager.log(e);
	    failAction("executeSelectQuery", dbQuery);
	}

	if (resultSet != null) {
	    passAction("executeSelectQuery", dbQuery, getStringFromResultSet(resultSet));
	} else {
	    failAction("executeSelectQuery",
		    "Null or no resultSet was returned from executing this query [" + dbQuery + "]");
	}

	return resultSet;
    }

    private static String getStringFromResultSet(ResultSet resultSet) {
	StringBuilder str = new StringBuilder();

	try {
	    if (resultSet.last()) {
		int lastRowID = resultSet.getRow();
		for (int i = 1; i <= lastRowID; i++) {
		    resultSet.absolute(i);
		    int currentColumnIndex = 1;
		    boolean reachedLastColumn = false;
		    do {
			try {
			    str.append(String.valueOf(resultSet.getString(currentColumnIndex)) + "\t");
			} catch (SQLException e) {
			    reachedLastColumn = true;
			    str.append("\n");
			}
			currentColumnIndex++;
		    } while (!reachedLastColumn);
		}
		resultSet.beforeFirst(); // reset pointer
	    }
	} catch (SQLException | NullPointerException e) {
	    ReportManager.log(e);
	    failAction("getStringFromResultSet");
	}
	return str.toString().trim();
    }

    public static String getResultSetValue(ResultSet resultSet) {
	String resultSetString = getStringFromResultSet(resultSet);
	passAction("getResultSetValue");
	return resultSetString;
    }
}
