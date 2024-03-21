package mockito;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.sql.ResultSet;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class DatabaseActionsTests {
    // mock creation
    ThreadLocal<SHAFT.DB> mockedDatabaseInstance = new ThreadLocal<>();

    @BeforeMethod
    public void setupMocks() {
        mockedDatabaseInstance.set(mock());
    }
    @Test
    public void select() {
        ThreadLocal<ResultSet> selectResults = new ThreadLocal<>();
        selectResults.set(mock());
        // using mock object
        mockedDatabaseInstance.get().executeSelectQuery("SELECT");
        //stubbing
        when(mockedDatabaseInstance.get().executeSelectQuery("SELECT")).thenReturn(selectResults.get());
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.get().executeSelectQuery("SELECT")).isEqualTo(selectResults.get()).perform();
    }

    @Test
    public void insert() {
        // using mock object
        mockedDatabaseInstance.get().executeInsertQuery("INSERT");
        //stubbing
        when(mockedDatabaseInstance.get().executeInsertQuery("INSERT")).thenReturn(0);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.get().executeInsertQuery("INSERT")).isEqualTo(0).perform();
    }

    @Test
    public void update() {
        // using mock object
        mockedDatabaseInstance.get().executeUpdateQuery("UPDATE");
        //stubbing
        when(mockedDatabaseInstance.get().executeUpdateQuery("UPDATE")).thenReturn(0);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.get().executeUpdateQuery("UPDATE")).isEqualTo(0).perform();
    }

    @Test
    public void delete() {
        // using mock object
        mockedDatabaseInstance.get().executeDeleteQuery("DELETE");
        //stubbing
        when(mockedDatabaseInstance.get().executeDeleteQuery("DELETE")).thenReturn(0);
        //verifying
        SHAFT.Validations.assertThat().object(mockedDatabaseInstance.get().executeDeleteQuery("DELETE")).isEqualTo(0).perform();
    }
}
