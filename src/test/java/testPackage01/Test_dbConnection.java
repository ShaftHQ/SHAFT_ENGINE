package testPackage01;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.testng.annotations.AfterClass;
import org.testng.annotations.Test;

import com.shaft.support.DBActions;

public class Test_dbConnection {
	
	DBActions dbActions;
	ResultSet QR;

	@Test
	public void test_mySQLConnection() {
		dbActions = new DBActions("mysql","72.55.136.25","3306","menna","incorta","incorta_123");
		dbActions.createConnection();
		QR= dbActions.executeSelectQuery("select * from DASHBOARD;");
		
		try {
			while(QR.next())
			{
				System.out.print(QR.getString(2)); //or rs.getString("column name");
			}
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Test
    public void test_SQLServerConnection() {
		dbActions = new DBActions("sqlserver","72.55.136.25","1432","salesdb","sa","1nc0rta_123");
		dbActions.createConnection();
		QR= dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");
		
		try {
			while(QR.next())
			{
				System.out.print(QR.getString(2)); //or rs.getString("column name");
			}
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

    }

	@AfterClass // Tear-down method, to be run once after the last test
	public void afterMethod() {
		dbActions.closeTheConnection();
	}
}