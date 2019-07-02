package testPackage01;

import java.sql.ResultSet;

import org.testng.annotations.Test;

import com.shaft.database.DBActions;

public class Test_dbConnection {

	DBActions dbActions;
	ResultSet queryResult;

	@Test
	public void test_mySQLConnection() {
		dbActions = new DBActions("mysql", "72.55.136.25", "3306", "menna", "incorta", "incorta_123");
		dbActions.executeSelectQuery("select ID from DASHBOARD where Name='Audit Dashboard' AND PARENT=519;");
		System.out.print(dbActions.retrieveSingleValue("string", 1));

	}

	@Test
	public void test_SQLServerConnection() {
		dbActions = new DBActions("sqlserver", "72.55.136.25", "1432", "salesdb", "sa", "1nc0rta_123");
		dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");
		System.out.print(dbActions.retrieveStringList(1));
	}

//	@Test
//    public void test_PostgreSQLConnection() {
//		dbActions = new DBActions("postgresql","72.55.136.25","5432","demo","dev","dev_incorta");
//		queryResult= dbActions.executeSelectQuery("select * from account");
//		
//		try {
//			while(queryResult.next())
//			{
//				System.out.print(queryResult.getString(2)); //or rs.getString("column name");
//			}
//		} catch (SQLException e) {
//			e.printStackTrace();
//		}
//    }

	@Test
	public void test_updateMySQLConnection() {
		dbActions = new DBActions("mysql", "72.55.136.25", "3306", "test_timezone", "incorta", "incorta_123");
		dbActions.executeUpdateQuery("delete from test_timezone.test where d = '2017-08-02'");
	}
}