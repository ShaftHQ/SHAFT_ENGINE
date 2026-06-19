package junitTestPackage;

import com.shaft.db.DatabaseActions.DatabaseType;
import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class JunitDatabaseSmokeTest {
    private static SHAFT.DB database;

    @BeforeAll
    static void createSmokeTable() {
        database = new SHAFT.DB(DatabaseType.POSTGRES_SQL, "localhost", "5432", "postgres", "postgres", "postgres");
        database.executeDDLStatement("DROP TABLE IF EXISTS shaft_junit_smoke");
        database.executeDDLStatement("CREATE TABLE shaft_junit_smoke(id SERIAL PRIMARY KEY, label VARCHAR(40) NOT NULL)");
    }

    @AfterAll
    static void dropSmokeTable() {
        if (database != null) {
            database.executeDDLStatement("DROP TABLE IF EXISTS shaft_junit_smoke");
        }
    }

    @Test
    void shaftDbShouldExecutePostgresCrudSmoke() {
        database.executeInsertQuery("INSERT INTO shaft_junit_smoke(label) VALUES('junit-smoke')");
        database.executeSelectQuery("SELECT label FROM shaft_junit_smoke");

        SHAFT.Validations.assertThat().number(database.getRowCount()).isEqualTo(1).perform();
        SHAFT.Validations.assertThat().object(database.getColumn("label")).isEqualTo("junit-smoke").perform();

        database.executeUpdateQuery("UPDATE shaft_junit_smoke SET label = 'junit-smoke-updated'");
        database.executeSelectQuery("SELECT label FROM shaft_junit_smoke");
        SHAFT.Validations.assertThat().object(database.getResult()).contains("junit-smoke-updated").perform();

        database.executeDeleteQuery("DELETE FROM shaft_junit_smoke");
        database.executeSelectQuery("SELECT label FROM shaft_junit_smoke");
        SHAFT.Validations.assertThat().number(database.getRowCount()).isEqualTo(0).perform();
    }
}
