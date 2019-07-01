package testPackage01;

import java.sql.ResultSet;

import org.testng.annotations.Test;

import com.shaft.db.DatabaseActions;
import com.shaft.validation.Assertions;

public class Test_dbConnection {

    DatabaseActions dbActions;
    ResultSet queryResult;

    @Test
    public void test_mySQLConnection() {
	dbActions = new DatabaseActions("mysql", "72.55.136.25", "3306", "menna", "incorta", "incorta_123");
	queryResult = dbActions.executeSelectQuery("select * from DASHBOARD;");

	Assertions.assertEquals("C87184_sql_spark_HAVING Dashboard", DatabaseActions.getResultSetValue(queryResult), 3,
		true);
    }

    @Test
    public void test_SQLServerConnection() {
	dbActions = new DatabaseActions("sqlserver", "72.55.136.25", "1432", "salesdb", "sa", "1nc0rta_123");
	queryResult = dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");

	Assertions.assertEquals("AutomationTest", DatabaseActions.getResultSetValue(queryResult), 3, true);
    }

    @Test
    public void test_PostgreSQLConnection() {
	dbActions = new DatabaseActions("postgresql", "72.55.136.25", "5432", "demo", "dev", "dev_incorta");
	queryResult = dbActions.executeSelectQuery("select * from account");

	Assertions.assertEquals("ahmed", DatabaseActions.getResultSetValue(queryResult), 3, true);
    }
}