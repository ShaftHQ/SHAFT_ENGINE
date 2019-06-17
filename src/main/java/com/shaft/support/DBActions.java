package com.shaft.support;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.testng.Assert;

import com.shaft.io.ReportManager;

public class DBActions {

	Connection connection;
	Statement statement;
	ResultSet resultSet;
	String dbType;
	String dbServerIP;
	String dbPort;
	String dbName;
	String username;
	String password;

	public DBActions(String dbType, String dbServerIP, String dbPort, String dbName, String username, String password) {
		this.dbType = dbType;
		this.dbServerIP = dbServerIP;
		this.dbPort = dbPort;
		this.dbName = dbName;
		this.username = username;
		this.password = password;
	}

	private void passAction(String actionName, String testData, String log) {
		String message = "Successfully performed action [" + actionName + "].";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ReportManager.log(message);
		if (log != null) {
			ReportManager.attach("DB Connection Log", log);
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
			ReportManager.attach("DB Connection Log", log);
		}
		Assert.fail(message);
	}

	private void failAction(String actionName, String testData) {
		failAction(actionName, testData, null);
	}

	/**
	 * Setup connection to database
	 * 
	 * @throws ClassNotFoundException
	 * @throws SQLException
	 */
	public void createConnection() {
		String testData = "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;

		try {
			switch (dbType) {
			// create connection to mysql DB
			case ("mysql"):
				Class.forName("com.mysql.jdbc.Driver");
				connection = DriverManager.getConnection("jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName,
						username, password);
				break;

			// create connection to oracle DB
			case ("oracle"):
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
			resultSet = statement.executeQuery(dbQuery);
			passAction("Execute DB query", dbQuery);
		}

		catch (SQLException e) {
			ReportManager.log(e);
		}
	return resultSet;
	}

	/**
	 * Close database connection
	 */
	public void closeTheConnection() {
		if (resultSet != null) {
			try {
				resultSet.close();
			} catch (Exception e) {
			}
		}

		if (statement != null) {
			try {
				statement.close();
			} catch (Exception e) {
			}
		}

		if (connection != null) {
			try {
				connection.close();
			} catch (Exception e) {
			}
		}
	}
}
