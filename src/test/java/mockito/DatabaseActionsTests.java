package mockito;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

import java.sql.ResultSet;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class DatabaseActionsTests {
    // mock creation
    SHAFT.DB mockedDatabaseInstance = mock();
    ResultSet selectResults = mock();

    @Test
    public void select() {
        // using mock object
        mockedDatabaseInstance.executeSelectQuery("SELECT");
        //stubbing
        when(mockedDatabaseInstance.executeSelectQuery("SELECT")).thenReturn(selectResults);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.executeSelectQuery("SELECT")).isEqualTo(selectResults).perform();
    }

    @Test
    public void insert() {
        // using mock object
        mockedDatabaseInstance.executeInsertQuery("INSERT");
        //stubbing
        when(mockedDatabaseInstance.executeInsertQuery("INSERT")).thenReturn(0);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.executeInsertQuery("INSERT")).isEqualTo(0).perform();
    }

    @Test
    public void update() {
        // using mock object
        mockedDatabaseInstance.executeUpdateQuery("UPDATE");
        //stubbing
        when(mockedDatabaseInstance.executeUpdateQuery("UPDATE")).thenReturn(0);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.executeUpdateQuery("UPDATE")).isEqualTo(0).perform();
    }

    @Test
    public void delete() {
        // using mock object
        mockedDatabaseInstance.executeDeleteQuery("DELETE");
        //stubbing
        when(mockedDatabaseInstance.executeDeleteQuery("DELETE")).thenReturn(0);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.executeDeleteQuery("DELETE")).isEqualTo(0).perform();
    }
}
