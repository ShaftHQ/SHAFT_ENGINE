package testPackage01;

import java.sql.ResultSet;

import org.testng.annotations.Test;

import com.shaft.db.DatabaseActions2;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;

public class Test_databaseActions {
    DatabaseActions2 dbActions;
    ResultSet queryResult;

    @Test
    public void test_mySQLConnection() {
	dbActions = new DatabaseActions2("mysql", "72.55.136.25", "3306", "menna", "incorta", "incorta_123");
	queryResult = dbActions.executeSelectQuery("select * from DASHBOARD;");

	Assertions.assertEquals("C87184_sql_spark_HAVING Dashboard", DatabaseActions2.getStringValue(queryResult), 3,
		true);
    }

    @Test
    public void test_SQLServerConnection() {
	dbActions = new DatabaseActions2("sqlserver", "72.55.136.25", "1432", "salesdb", "sa", "1nc0rta_123");
	queryResult = dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");

	Assertions.assertEquals("AutomationTest", DatabaseActions2.getStringValue(queryResult), 3, true);
    }

    @Test
    public void test_PostgreSQLConnection() {
	dbActions = new DatabaseActions2("postgresql", "72.55.136.25", "5432", "demo", "dev", "dev_incorta");
	queryResult = dbActions.executeSelectQuery("select * from account");

	Assertions.assertEquals("ahmed", DatabaseActions2.getStringValue(queryResult), 3, true);
	Assertions.assertEquals(1, DatabaseActions2.getRowCount(queryResult), AssertionComparisonType.LITERAL,
		AssertionType.POSITIVE);
    }
}
