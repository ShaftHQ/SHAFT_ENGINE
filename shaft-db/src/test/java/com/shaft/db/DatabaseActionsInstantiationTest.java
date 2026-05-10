package com.shaft.db;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class DatabaseActionsInstantiationTest {

    @Test
    void shouldInstantiateWithoutRealDatabase() {
        // Constructor just sets fields — no connection attempt at construction time.
        DatabaseActions db = new DatabaseActions(
            DatabaseActions.DatabaseType.MY_SQL,
            "localhost", "3306", "testdb", "user", "pass");
        assertNotNull(db, "DatabaseActions must construct without a real database connection");
    }

    @Test
    void mySqlJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        assertNotNull(Class.forName("com.mysql.cj.jdbc.Driver"), "MySQL JDBC driver must be on classpath");
    }

    @Test
    void postgresJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        assertNotNull(Class.forName("org.postgresql.Driver"), "PostgreSQL JDBC driver must be on classpath");
    }

    @Test
    void sqlServerJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        assertNotNull(Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver"), "SQL Server JDBC driver must be on classpath");
    }

    @Test
    void shouldNotLeakClassNotFoundOnBadConnectionAttempt() {
        // Constructing with valid-looking params succeeds (no connection at construction time).
        // If executeSelectQuery fails, it must be a SQL/IO error — never ClassNotFoundException.
        DatabaseActions db = new DatabaseActions(
            DatabaseActions.DatabaseType.MY_SQL,
            "127.0.0.1", "19999", "nonexistent", "user", "pass");
        try {
            db.executeSelectQuery("SELECT 1");
        } catch (ClassNotFoundException e) {
            fail("Must not throw ClassNotFoundException: " + e);
        } catch (Exception e) {
            // Any other exception (SQL, IO, runtime) is acceptable.
        }
    }
}
