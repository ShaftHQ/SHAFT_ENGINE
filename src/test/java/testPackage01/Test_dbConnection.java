package testPackage01;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.testng.annotations.AfterClass;
import org.testng.annotations.Test;

import com.shaft.support.DBActions;

public class Test_dbConnection {
	
	DBActions dbActions;
	ResultSet queryResult;

	//@Test
	public void test_mySQLConnection() {
		dbActions = new DBActions("mysql","72.55.136.25","3306","menna","incorta","incorta_123");
		dbActions.createConnection();
		queryResult= dbActions.executeSelectQuery("select * from DASHBOARD;");
		
		try {
			while(queryResult.next())
			{
				System.out.print(queryResult.getString(2)); //or rs.getString("column name");
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
	
	//@Test
    public void test_SQLServerConnection() {
		dbActions = new DBActions("sqlserver","72.55.136.25","1432","salesdb","sa","1nc0rta_123");
		dbActions.createConnection();
		queryResult= dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");
		
		try {
			while(queryResult.next())
			{
				System.out.print(queryResult.getString(2)); //or rs.getString("column name");
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
    }
	
	@Test
    public void test_PostgreSQLConnection() {
		dbActions = new DBActions("postgresql","72.55.136.25","5432","demo","dev","dev_incorta");
		dbActions.createConnection();
		queryResult= dbActions.executeSelectQuery("select * from account");
		
		try {
			while(queryResult.next())
			{
				System.out.print(queryResult.getString(2)); //or rs.getString("column name");
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
    }

	@AfterClass // Tear-down method, to be run once after the last test
	public void afterMethod() {
		dbActions.closeTheConnection();
	}
}