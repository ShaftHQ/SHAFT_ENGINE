package com.shaft.database;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.testng.Assert;

import com.shaft.io.ReportManager;

/**
 * 
 * @author mennamaged
 *
 */

public class DBActions {

	private Connection connection;
	private Statement statement;
	private ResultSet resultSet;
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

	public DBActions(String dbType, String dbServerIP, String dbPort, String dbName, String username, String password) {
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

	private void passAction(String actionName, String testData, String log) {
		String message = "Successfully performed action [" + actionName + "].";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ReportManager.log(message);
		if (log != null) {
			ReportManager.attachAsStep("Passed", "DB Connection Log", log);
		}
	}

	private void passAction(String actionName, String testData) {
		passAction(actionName, testData, null);
	}

	private void failAction(String actionName, String testData, String log) {
		String message = "Failed to perform action [" + actionName + "].";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ReportManager.log(message);
		if (log != null) {
			ReportManager.attachAsStep("Failed", "DB Connection Log", log);
		}
		Assert.fail(message);
	}

	private void failAction(String actionName, String testData) {
		failAction(actionName, testData, null);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////// [Public] Core Database Actions
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Setup connection to database
	 * 
	 * @throws ClassNotFoundException
	 * @throws SQLException
	 * 
	 * 
	 */

	private void createConnection() {
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
			connection = DriverManager.getConnection(connectionString, username, password);
			ReportManager.logDiscrete("Connection is created successfully");
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("createConnection", connectionString);
		}
	}

	/**
	 * prepare statement for database connection to perform execute query
	 * 
	 * 
	 */

	private void createStatement() {
		try {
			statement = connection.createStatement();
			ReportManager.logDiscrete("Statement is created successfully");
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("createStatement", connection.toString());
		}
	}

	/**
	 * execute DB query and return the result as ResultSet object
	 * 
	 * @param dbQuery
	 * 
	 */
	public void executeSelectQuery(String dbQuery) {
		resultSet = null;
		try {
			createConnection();
			createStatement();
			resultSet = statement.executeQuery(dbQuery);
			passAction("executeSelectQuery", dbQuery);
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("executeSelectQuery", dbQuery);
		}
	}
	
	/**
	 * Retrieve single string value from resultset 
	 * @param dataType
	 * @param columnIndex after executing query
	 * @return string value
	 */

	public String retrieveSingleValue(String dataType, int columnIndex) {
		int intSearchResult;
		String stringSearchResult;
		try {
			while (resultSet.next()) {
				switch (dataType.toLowerCase().trim()) {
				case ("int"):
					intSearchResult = resultSet.getInt(columnIndex); 
					return String.valueOf(intSearchResult);
				case ("string"):
					stringSearchResult = resultSet.getString(columnIndex);
					return stringSearchResult;
				default:
					ReportManager.log("Data type is not supported");
					failAction("retrieveSingleValue", dbType);
					break;
				}
			}
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("retrieveSingleDataValue", dataType);
		}
		return "";
	}
	
	/**
	 * Retrieve string list from resultset 
	 * @param columnIndex
	 * @return list of strings
	 */
	
	public ArrayList<String> retrieveStringList(int columnIndex){
		ArrayList<String> retrievedList = new ArrayList<String>();
		try {
			while (resultSet.next()) {
				retrievedList.add(resultSet.getString(columnIndex));
			}
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("retrieveStringList","");
		}
		return retrievedList;
	}

	/**
	 * execute DB queries including DELETE\UPDATE\INSERT
	 * 
	 * @param dbQuery
	 */
	public void executeUpdateQuery(String dbQuery) {
		createConnection();
		createStatement();
		try {
			statement.executeUpdate(dbQuery);
			passAction("executeUpdateQuery", dbQuery);
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("executeUpdateQuery", dbQuery);
		}
	}

	/**
	 * Close database connection, resultset and statement
	 */
	public void closeConnection() {
		try {
			if (resultSet != null)
				resultSet.close();
			if (statement != null)
				statement.close();
			if (connection != null)
				connection.close();
			passAction("closeConnection", connection.toString());
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("closeConnection", connection.toString());
		}
	}
}
