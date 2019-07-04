package testPackage01;

import java.sql.ResultSet;

import org.testng.annotations.Test;

import com.shaft.db.DatabaseActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;

public class Test_databaseActions {
    DatabaseActions dbActions;
    ResultSet queryResult;

    @Test
    public void test_mySQLConnection() {
	dbActions = new DatabaseActions("mysql", "72.55.136.25", "3306", "menna", "incorta", "incorta_123");
	queryResult = dbActions.executeSelectQuery("select * from DASHBOARD limit 5;");

	Assertions.assertEquals("Audit Dashboard", DatabaseActions.getResult(queryResult),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals("Audit Dashboard", DatabaseActions.getRow(queryResult, "id", "3462"),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals("3462", DatabaseActions.getColumn(queryResult, "id"), AssertionComparisonType.CONTAINS,
		AssertionType.POSITIVE);
	Assertions.assertEquals(5, DatabaseActions.getRowCount(queryResult), AssertionComparisonType.LITERAL,
		AssertionType.POSITIVE);
    }

    @Test
    public void test_SQLServerConnection() {
	dbActions = new DatabaseActions("sqlserver", "72.55.136.25", "1432", "salesdb", "sa", "1nc0rta_123");
	queryResult = dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");

	Assertions.assertEquals("AutomationTest", DatabaseActions.getResult(queryResult),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals("1", DatabaseActions.getRow(queryResult, "name", "ahmed"),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals("ahmed", DatabaseActions.getColumn(queryResult, "name"),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals(4, DatabaseActions.getRowCount(queryResult), AssertionComparisonType.LITERAL,
		AssertionType.POSITIVE);
    }

    @Test
    public void test_PostgreSQLConnection() {
	dbActions = new DatabaseActions("postgresql", "72.55.136.25", "5432", "demo", "dev", "dev_incorta");
	queryResult = dbActions.executeSelectQuery("select * from account");

	Assertions.assertEquals("ahmed", DatabaseActions.getResult(queryResult), AssertionComparisonType.CONTAINS,
		AssertionType.POSITIVE);
	Assertions.assertEquals("1", DatabaseActions.getRow(queryResult, "name", "ahmed"),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals("ahmed", DatabaseActions.getColumn(queryResult, "name"),
		AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	Assertions.assertEquals(1, DatabaseActions.getRowCount(queryResult), AssertionComparisonType.LITERAL,
		AssertionType.POSITIVE);
    }
}
