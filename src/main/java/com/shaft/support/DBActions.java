package com.shaft.support;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

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
	private String testData = "";

	private static final String MySQL = "mysql";
	private static final String SqlServer = "sqlserver";
	private static final String PostgreSql = "postgresql";
	private static final String Oracle = "oracle";

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
	 */
	private void createConnection() {
		try {
			switch (dbType.toLowerCase().trim()) {
			// create connection to mysql DB
			case MySQL:
				Class.forName("com.mysql.jdbc.Driver");
				connection = DriverManager.getConnection("jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName,
						username, password);
				testData = "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;
				break;
			// create connection to SQL Server DB
			case SqlServer:
				Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver");
				connection = DriverManager.getConnection(
						"jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName, username,
						password);
				testData = "jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName;
				break;
			// create connection to SQL Server DB
			case PostgreSql:
				Class.forName("org.postgresql.Driver");
				connection = DriverManager.getConnection(
						"jdbc:postgresql://" + dbServerIP + ":" + dbPort + "/" + dbName, username, password);
				testData = "jdbc:postgresql://" + dbServerIP + ":" + dbPort + "/" + dbName;
				break;
			// create connection to oracle DB
			case Oracle:
				break;
			}

			statement = connection.createStatement();
			passAction("create DB connection", testData);
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("create DB connection", testData);
		} catch (ClassNotFoundException e) {
			ReportManager.log(e);
			failAction("create DB connection", testData);
		}
	}

	/**
	 * execute DB query and return the result as ResultSet object
	 * 
	 * @param dbQuery
	 * @return ResultSet
	 * @throws SQLException
	 */
	public ResultSet executeSelectQuery(String dbQuery) {
		try {
			createConnection();
			resultSet = statement.executeQuery(dbQuery);
			passAction("execute DB query", dbQuery);
		}
		catch (SQLException e) {
			ReportManager.log(e);
			failAction("execute DB query", dbQuery);
		}
		closeTheConnection();
		return resultSet;
	}

	/**
	 * Close database connection
	 */
	private void closeTheConnection() {
		try {
			if (resultSet != null)
				resultSet.close();
			if (statement != null)
				statement.close();
			if (connection != null)
				connection.close();
			passAction("closing connection", testData);
		} catch (SQLException e) {
			ReportManager.log(e);
			failAction("closing connection", testData);
		}
	}
}
