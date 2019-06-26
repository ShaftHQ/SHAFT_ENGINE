package testPackage01;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import com.shaft.db.DatabaseActions;

public class Test_dbConnection {

    DatabaseActions dbActions;
    ResultSet queryResult;

    @Test
    public void test_mySQLConnection() {
	dbActions = new DatabaseActions("mysql", "72.55.136.25", "3306", "menna", "incorta", "incorta_123");
	queryResult = dbActions.executeSelectQuery("select * from DASHBOARD;");

	try {
	    while (queryResult.next()) {
		System.out.print(queryResult.getString(2)); // or rs.getString("column name");
	    }
	} catch (SQLException e) {
	    e.printStackTrace();
	}
    }

    @Test
    public void test_SQLServerConnection() {
	dbActions = new DatabaseActions("sqlserver", "72.55.136.25", "1432", "salesdb", "sa", "1nc0rta_123");
	queryResult = dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");

	try {
	    while (queryResult.next()) {
		System.out.print(queryResult.getString(2)); // or rs.getString("column name");
	    }
	} catch (SQLException e) {
	    e.printStackTrace();
	}
    }

    @Test
    public void test_PostgreSQLConnection() {
	dbActions = new DatabaseActions("postgresql", "72.55.136.25", "5432", "demo", "dev", "dev_incorta");
	queryResult = dbActions.executeSelectQuery("select * from account");

	try {
	    while (queryResult.next()) {
		System.out.print(queryResult.getString(2)); // or rs.getString("column name");
	    }
	} catch (SQLException e) {
	    e.printStackTrace();
	}
    }

    @AfterMethod
    public void afterMethod() {
//	dbActions.closeConnection();
    }

}