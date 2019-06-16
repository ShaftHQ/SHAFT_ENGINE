package testPackage01;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.support.DBActions;

public class Test_dbConnection {
	
	DBActions dbActions;
	ResultSet QR;

	@Test
	public void test_mySQLConnection() {
		QR= dbActions.executeQuery("select * from DASHBOARD;");
		
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

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {
		dbActions = new DBActions("mysql","72.55.136.25","3306","menna","incorta","incorta_123");
		dbActions.createConnection();
	}

	@AfterClass // Tear-down method, to be run once after the last test
	public void afterClass() {
		dbActions.closeTheConnection();
	}
}