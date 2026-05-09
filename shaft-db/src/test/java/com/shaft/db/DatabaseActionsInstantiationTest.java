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
        Class.forName("com.mysql.cj.jdbc.Driver");
    }

    @Test
    void postgresJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        Class.forName("org.postgresql.Driver");
    }

    @Test
    void sqlServerJdbcDriverShouldBeResolvableOnClasspath() throws ClassNotFoundException {
        Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver");
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
        } catch (Exception e) {
            assertFalse(e instanceof ClassNotFoundException,
                "Must not throw ClassNotFoundException: " + e);
            // Any other exception (SQL, IO, runtime) is acceptable.
        }
    }
}
