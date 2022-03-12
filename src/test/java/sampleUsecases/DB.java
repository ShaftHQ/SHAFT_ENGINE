package sampleUsecases;

import com.shaft.db.DatabaseDriver;
import com.shaft.db.DatabaseActions;
import com.shaft.db.OracleDBConnection;
import com.shaft.db.ResultObject;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

import java.sql.ResultSet;

public class DB {

    @Test
    public void before()
    {
        DatabaseActions dbActions = new DatabaseActions(DatabaseActions.DatabaseType.ORACLE, "", "", "", "", "");
        ResultSet queryResult = dbActions.executeSelectQuery("select 1 from dual");
        Validations.assertThat().object( DatabaseActions.getResult(queryResult)).isNotNull();
    }

    @Test
    public void after()
    {
        DatabaseDriver dataBaseDriver = new DatabaseDriver(new OracleDBConnection("localhost", "1521","DataDB","Admin","Admin123"));
        ResultObject resultObject = dataBaseDriver.executeQuery("SELECT * FROM Employees where id=101");
        Validations.assertThat().object( resultObject.getString()).isNotNull();
    }
}
