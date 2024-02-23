package testPackage;

import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

import java.sql.ResultSet;

public class DbConnectionTests {
    @Test
    public void test_mySQLConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.MY_SQL, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select TOP 10 from DASHBOARD;");
        Validations.assertThat().object(DatabaseActions.getResult(queryResult))
                .contains("")
                .perform();
    }

    @Test
    public void test_SQLServerConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.SQL_SERVER, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select * from abdelsalam.dbo.automation_table");
        Validations.assertThat().object(DatabaseActions.getResult(queryResult))
                .contains("")
                .perform();
    }

    @Test
    public void test_PostgreSQLConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.POSTGRES_SQL, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select * from account");
        Validations.assertThat().object(DatabaseActions.getResult(queryResult))
                .contains("")
                .perform();
    }

    @Test
    public void test_OracleConnection() {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.ORACLE, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select 1 from dual");
        Validations.assertThat().object(DatabaseActions.getResult(queryResult))
                .contains("")
                .perform();
    }
}