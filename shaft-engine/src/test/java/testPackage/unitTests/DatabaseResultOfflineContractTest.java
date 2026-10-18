package testPackage.unitTests;

import com.shaft.db.DatabaseActions;
import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class DatabaseResultOfflineContractTest {
    @Test
    public void resultReadersFormatRowsColumnsAndCounts() throws SQLException {
        ResultSet results = table(List.of(new String[] {"1", "Ada"}, new String[] {"2", "Grace"}));

        Assert.assertEquals(SHAFT.DB.getResult(results), "1\tAda\n2\tGrace");
        Assert.assertEquals(SHAFT.DB.getColumn(results, "name"), "Ada\nGrace");
        Assert.assertEquals(SHAFT.DB.getRow(results, "name", " Ada "), "1\tAda");
        Assert.assertEquals(SHAFT.DB.getRowCount(results), 2);
    }

    @Test
    public void emptyAndBrokenResultsStayOffline() throws SQLException {
        ResultSet empty = table(List.of());
        Assert.assertEquals(SHAFT.DB.getResult(empty), "");
        Assert.assertEquals(SHAFT.DB.getColumn(empty, "name"), "");
        Assert.assertEquals(SHAFT.DB.getRowCount(empty), 0);
        RuntimeException missingRow = Assert.expectThrows(
                RuntimeException.class,
                () -> SHAFT.DB.getRow(empty, "name", "Ada"));
        Assert.assertTrue(missingRow.getMessage().contains("name"), missingRow.getMessage());
        Assert.assertTrue(missingRow.getMessage().contains("Ada"), missingRow.getMessage());

        RuntimeException missingResult = Assert.expectThrows(
                RuntimeException.class,
                () -> SHAFT.DB.getResult(null));
        Assert.assertTrue(missingResult.getMessage().toLowerCase().contains("failed"), missingResult.getMessage());

        ResultSet closed = mock(ResultSet.class);
        doThrow(new SQLException("closed cursor")).when(closed).beforeFirst();
        RuntimeException closedResult = Assert.expectThrows(
                RuntimeException.class,
                () -> SHAFT.DB.getColumn(closed, "name"));
        Assert.assertTrue(closedResult.getCause() instanceof SQLException, String.valueOf(closedResult.getCause()));
        Assert.assertTrue(closedResult.getCause().getMessage().contains("closed cursor"), closedResult.getCause().getMessage());
    }

    @Test
    public void constructionRejectsBlankConnectionDataWithoutOpeningADatabase() {
        RuntimeException blankField = Assert.expectThrows(
                RuntimeException.class,
                () -> new SHAFT.DB(DatabaseActions.DatabaseType.POSTGRES_SQL, "", "5432", "db", "user", "secret"));
        Assert.assertFalse(blankField.getMessage().contains("secret"), blankField.getMessage());
        Assert.assertTrue(blankField.getMessage().contains("******"), blankField.getMessage());

        RuntimeException blankUrl = Assert.expectThrows(RuntimeException.class, () -> SHAFT.DB.getInstance(""));
        Assert.assertTrue(blankUrl.getMessage().contains("Custom Connection String"), blankUrl.getMessage());

        SHAFT.DB configured = SHAFT.DB.getInstance(
                DatabaseActions.DatabaseType.MY_SQL, "127.0.0.1", "3306", "offline", "user", "secret");
        SHAFT.DB custom = new SHAFT.DB("jdbc:shaft:offline");
        Assert.assertNotNull(configured);
        Assert.assertNotNull(custom);
    }

    private static ResultSet table(List<String[]> rows) throws SQLException {
        ResultSet results = mock(ResultSet.class);
        ResultSetMetaData metadata = mock(ResultSetMetaData.class);
        when(metadata.getColumnCount()).thenReturn(2);
        when(results.getMetaData()).thenReturn(metadata);
        when(results.findColumn("id")).thenReturn(1);
        when(results.findColumn("name")).thenReturn(2);
        AtomicInteger cursor = new AtomicInteger();
        doAnswer(invocation -> {
            cursor.set(0);
            return null;
        }).when(results).beforeFirst();
        when(results.last()).thenAnswer(invocation -> {
            if (rows.isEmpty()) {
                return false;
            }
            cursor.set(rows.size());
            return true;
        });
        when(results.getRow()).thenAnswer(invocation -> cursor.get());
        when(results.absolute(anyInt())).thenAnswer(invocation -> {
            cursor.set(invocation.getArgument(0));
            return true;
        });
        when(results.getString(anyInt())).thenAnswer(invocation -> {
            int column = invocation.getArgument(0);
            return rows.get(cursor.get() - 1)[column - 1];
        });
        return results;
    }
}
