package testPackage.mockedTests;

import com.shaft.db.DatabaseActions;
import com.shaft.db.DatabaseActions.DatabaseType;
import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.sql.rowset.CachedRowSet;
import javax.sql.rowset.RowSetProvider;
import javax.sql.rowset.RowSetFactory;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.sql.*;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Mocked unit tests for DatabaseActions class to increase code coverage.
 * These tests use Mockito to mock database connections and avoid actual database connectivity.
 */
public class DatabaseActionsMockedTests {
    
    private Connection mockConnection;
    private Statement mockStatement;
    private ResultSet mockResultSet;
    private CachedRowSet mockCachedRowSet;
    private ResultSetMetaData mockMetaData;
    private RowSetFactory mockRowSetFactory;
    private MockedStatic<DriverManager> driverManagerMock;
    private MockedStatic<RowSetProvider> rowSetProviderMock;

    @BeforeMethod
    public void beforeMethod() throws SQLException {
        // Mock database components
        mockConnection = mock(Connection.class);
        mockStatement = mock(Statement.class);
        mockResultSet = mock(ResultSet.class);
        mockCachedRowSet = mock(CachedRowSet.class);
        mockMetaData = mock(ResultSetMetaData.class);
        mockRowSetFactory = mock(RowSetFactory.class);
        
        // Setup basic mocks
        driverManagerMock = mockStatic(DriverManager.class);
        rowSetProviderMock = mockStatic(RowSetProvider.class);
        
        // Configure mock behaviors
        driverManagerMock.when(() -> DriverManager.getConnection(anyString())).thenReturn(mockConnection);
        driverManagerMock.when(() -> DriverManager.getConnection(anyString(), anyString(), anyString()))
                .thenReturn(mockConnection);
        when(mockConnection.createStatement(anyInt(), anyInt())).thenReturn(mockStatement);
        when(mockStatement.executeQuery(anyString())).thenReturn(mockResultSet);
        when(mockStatement.executeUpdate(anyString())).thenReturn(1);
        when(mockResultSet.getMetaData()).thenReturn(mockMetaData);
        when(mockCachedRowSet.getMetaData()).thenReturn(mockMetaData);
        when(mockMetaData.getColumnCount()).thenReturn(2);
        when(mockMetaData.getColumnName(1)).thenReturn("id");
        when(mockMetaData.getColumnName(2)).thenReturn("name");
        when(mockResultSet.findColumn("id")).thenReturn(1);
        when(mockResultSet.findColumn("name")).thenReturn(2);
        when(mockCachedRowSet.findColumn("id")).thenReturn(1);
        when(mockCachedRowSet.findColumn("name")).thenReturn(2);

        mockScrollableResultSet(mockResultSet);
        mockScrollableResultSet(mockCachedRowSet);

        rowSetProviderMock.when(RowSetProvider::newFactory).thenReturn(mockRowSetFactory);
        when(mockRowSetFactory.createCachedRowSet()).thenReturn(mockCachedRowSet);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driverManagerMock != null) {
            driverManagerMock.close();
        }
        if (rowSetProviderMock != null) {
            rowSetProviderMock.close();
        }
    }

    @Test
    public void testDatabaseActionsConstructorWithAllParameters() {
        DatabaseActions dbActions = new DatabaseActions(
            DatabaseType.MY_SQL,
            "localhost",
            "3306",
            "testdb",
            "testuser",
            "testpass"
        );
        Assert.assertNotNull(dbActions);
    }

    @Test
    public void testDatabaseActionsConstructorWithCustomConnectionString() {
        DatabaseActions dbActions = new DatabaseActions("jdbc:mysql://localhost:3306/testdb");
        Assert.assertNotNull(dbActions);
    }

    @Test
    public void testGetInstanceWithAllParameters() {
        DatabaseActions dbActions = DatabaseActions.getInstance(
            DatabaseType.POSTGRES_SQL,
            "127.0.0.1",
            "5432",
            "postgres",
            "admin",
            "admin123"
        );
        Assert.assertNotNull(dbActions);
    }

    @Test
    public void testGetInstanceWithCustomConnectionString() {
        DatabaseActions dbActions = DatabaseActions.getInstance("jdbc:postgresql://localhost:5432/testdb");
        Assert.assertNotNull(dbActions);
    }

    @Test(expectedExceptions = {Exception.class})
    public void testConstructorWithEmptyParameters() {
        new DatabaseActions(DatabaseType.MY_SQL, "", "", "", "", "");
    }

    @Test(expectedExceptions = {Exception.class})
    public void testConstructorWithEmptyCustomConnectionString() {
        new DatabaseActions("");
    }

    @Test
    public void testGetResultWithMockedResultSet() throws SQLException {
        String result = DatabaseActions.getResult(mockResultSet);
        Assert.assertEquals(result, "1\tMona\n2\tOctocat");
    }

    @Test
    public void testGetRowColumnAndRowCountWithMockedResultSet() {
        Assert.assertEquals(DatabaseActions.getRow(mockResultSet, "name", "Octocat"), "2\tOctocat");
        Assert.assertEquals(DatabaseActions.getColumn(mockResultSet, "name"), "Mona\nOctocat");
        Assert.assertEquals(DatabaseActions.getRowCount(mockResultSet), 2);
    }

    @Test
    public void testExecuteQueryMethodsAndCleanup() throws Exception {
        DatabaseActions dbActions = new DatabaseActions(DatabaseType.POSTGRES_SQL, "localhost", "5432", "testdb", "user", "pass");
        ResultSet selectResult = dbActions.executeSelectQuery("SELECT * FROM student");
        Assert.assertNotNull(selectResult);
        Assert.assertEquals(dbActions.getResult(), "1\tMona\n2\tOctocat");
        Assert.assertEquals(dbActions.getRow("name", "Mona"), "1\tMona");
        Assert.assertEquals(dbActions.getColumn("id"), "1\n2");
        Assert.assertEquals(dbActions.getRowCount(), 2);

        Assert.assertEquals(dbActions.executeInsertQuery("INSERT"), 1);
        Assert.assertEquals(dbActions.executeUpdateQuery("UPDATE"), 1);
        Assert.assertEquals(dbActions.executeDeleteQuery("DELETE"), 1);
        dbActions.executeDDLStatement("CREATE TABLE x");
        Assert.assertEquals(dbActions.getRowCount(), 1);

        dbActions.cleanup();
        Assert.assertNull(readThreadLocalField(dbActions, "resultSetThreadLocal"));
        Assert.assertNull(readThreadLocalField(dbActions, "rowCountThreadLocal"));
    }

    @Test
    public void testCreateConnectionCoversDatabaseTypesAndPrivateMessageMasking() throws Exception {
        for (DatabaseType dbType : DatabaseType.values()) {
            DatabaseActions dbActions = new DatabaseActions(dbType, "127.0.0.1", "1234", "db", "user", "pass");
            Assert.assertEquals(dbActions.executeUpdateQuery("UPDATE test"), 1);
        }
        verify(mockConnection, atLeastOnce()).setNetworkTimeout(any(), anyInt());

        DatabaseActions customConnectionDatabase = new DatabaseActions("jdbc:sqlserver://localhost;databaseName=db;User=sa;Password=secret");
        Assert.assertEquals(customConnectionDatabase.executeUpdateQuery("UPDATE test"), 1);

        Method getReportMessageMethod = DatabaseActions.class.getDeclaredMethod("getReportMessage", String.class, String.class);
        getReportMessageMethod.setAccessible(true);
        String customReportMessage = (String) getReportMessageMethod.invoke(customConnectionDatabase, "UPDATE", "UPDATE test");
        Assert.assertTrue(customReportMessage.contains("User=****"));
        Assert.assertTrue(customReportMessage.contains("Password=*****"));
    }

    @Test
    public void testCreateStatementHandlesPostgresFeatureNotSupportedMessage() throws SQLException {
        doThrow(new SQLFeatureNotSupportedException("org.postgresql.jdbc4.Jdbc4Statement.setQueryTimeout"))
                .when(mockStatement).setQueryTimeout(anyInt());

        DatabaseActions dbActions = new DatabaseActions(DatabaseType.POSTGRES_SQL, "localhost", "5432", "testdb", "user", "pass");
        Assert.assertEquals(dbActions.executeUpdateQuery("UPDATE student SET id=1"), 1);
    }

    @Test
    public void testDatabaseTypeEnum() {
        Assert.assertNotNull(DatabaseType.MY_SQL);
        Assert.assertNotNull(DatabaseType.SQL_SERVER);
        Assert.assertNotNull(DatabaseType.POSTGRES_SQL);
        Assert.assertNotNull(DatabaseType.ORACLE);
        Assert.assertNotNull(DatabaseType.ORACLE_SERVICE_NAME);
        Assert.assertNotNull(DatabaseType.IBM_DB2);
    }

    private void mockScrollableResultSet(ResultSet resultSet) throws SQLException {
        AtomicInteger currentRow = new AtomicInteger(0);

        doAnswer(invocation -> {
            currentRow.set(0);
            return null;
        }).when(resultSet).beforeFirst();
        when(resultSet.last()).thenAnswer(invocation -> {
            currentRow.set(2);
            return true;
        });
        when(resultSet.getRow()).thenAnswer(invocation -> currentRow.get());
        when(resultSet.absolute(anyInt())).thenAnswer(invocation -> {
            currentRow.set(invocation.getArgument(0));
            return true;
        });
        when(resultSet.getString(anyInt())).thenAnswer(invocation -> {
            int rowIndex = currentRow.get();
            int columnIndex = invocation.getArgument(0);
            if (rowIndex == 1 && columnIndex == 1) return "1";
            if (rowIndex == 1) return "Mona";
            if (rowIndex == 2 && columnIndex == 1) return "2";
            return "Octocat";
        });
    }

    private Object readThreadLocalField(DatabaseActions dbActions, String fieldName) throws Exception {
        Field field = DatabaseActions.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<Object> threadLocal = (ThreadLocal<Object>) field.get(dbActions);
        return threadLocal.get();
    }
}
