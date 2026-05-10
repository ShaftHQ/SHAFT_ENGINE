package com.shaft.db;

import org.testng.annotations.Test;

import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertNotNull;

public class DatabaseActionsInstantiationTest {

    @Test
    public void shouldInstantiateWithoutRealDatabase() {
        // Constructor just sets fields — no connection attempt at construction time.
        DatabaseActions db = new DatabaseActions(
            DatabaseActions.DatabaseType.MY_SQL,
            "localhost", "3306", "testdb", "user", "pass");
        assertNotNull(db, "DatabaseActions must construct without a real database connection");
    }

    @Test
    public void mySqlJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        assertNotNull(Class.forName("com.mysql.cj.jdbc.Driver"), "MySQL JDBC driver must be on classpath");
    }

    @Test
    public void postgresJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        assertNotNull(Class.forName("org.postgresql.Driver"), "PostgreSQL JDBC driver must be on classpath");
    }

    @Test
    public void sqlServerJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        assertNotNull(Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver"), "SQL Server JDBC driver must be on classpath");
    }

    @Test
    public void shouldNotLeakClassNotFoundOnBadConnectionAttempt() {
        // Constructing with valid-looking params succeeds (no connection at construction time).
        // If executeSelectQuery fails, it must be a SQL/IO error — never ClassNotFoundException.
        DatabaseActions db = new DatabaseActions(
            DatabaseActions.DatabaseType.MY_SQL,
            "127.0.0.1", "19999", "nonexistent", "user", "pass");
        try {
            db.executeSelectQuery("SELECT 1");
        } catch (Exception e) {
            assertNotEquals(e.getClass(), ClassNotFoundException.class,
                "Must not throw ClassNotFoundException: " + e);
        }
    }
}
