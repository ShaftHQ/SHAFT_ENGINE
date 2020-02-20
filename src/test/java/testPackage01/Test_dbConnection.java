package testPackage01;

import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import org.testng.annotations.Test;

import java.sql.ResultSet;

public class Test_dbConnection {
    @Test
    public void test_mySQLConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.MY_SQL, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select TOP 10 from DASHBOARD;");
        Assertions.assertEquals("", DatabaseActions.getResult(queryResult), AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);

    }

    @Test
    public void test_SQLServerConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.SQL_SERVER, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");
        Assertions.assertEquals("", DatabaseActions.getResult(queryResult), AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }

    @Test
    public void test_PostgreSQLConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.POSTGRE_SQL, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select * from account");
        Assertions.assertEquals("", DatabaseActions.getResult(queryResult), AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }

    @Test
    public void test_OracleConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.ORACLE, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select 1 from dual");
        Assertions.assertEquals("", DatabaseActions.getResult(queryResult), AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }
}