package testPackage.db;

import com.shaft.db.DatabaseActions;
import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class DatabaseTests {
    /*
    Test Database credentials
    Account: https://www.freemysqlhosting.net/account/ - mohab.mohieeldeen@outlook.com - YUCU4vtnYmfGy7S
    Management: https://www.phpmyadmin.co/
    Your account number is: 758021
    visit once a week to extend: https://www.freemysqlhosting.net/extension/?acc=yblb9%2F8EzxAc1UnxeCHwBg%3D%3D
    */
    SHAFT.DB driver;

    @Test
    public void insert() {
        driver.executeInsertQuery("""
                INSERT INTO SHAFT (TestID, Data)
                VALUES (1, 'this is the inserted data');""");
        SHAFT.Validations.assertThat().number(driver.getRowCount()).isEqualTo(1).perform();
    }

    @Test(dependsOnMethods = {"insert"})
    public void update() {
        driver.executeUpdateQuery("UPDATE SHAFT SET Data = 'this is the updated data' WHERE TestID = 1;");
        SHAFT.Validations.assertThat().number(driver.getRowCount()).isEqualTo(1).perform();
    }

    @Test(dependsOnMethods = {"update"})
    public void select() {
        driver.executeSelectQuery("SELECT Data FROM SHAFT WHERE TestID = 1;");
        SHAFT.Validations.assertThat().object(driver.getResult()).isEqualTo("this is the updated data").perform();
    }

    @Test(dependsOnMethods = {"select"})
    public void delete() {
        driver.executeDeleteQuery("DELETE FROM SHAFT WHERE TestID = 1;");
        SHAFT.Validations.assertThat().number(driver.getRowCount()).isEqualTo(1).perform();
    }

    @BeforeClass
    public void setup() {
        driver = SHAFT.DB.getInstance(DatabaseActions.DatabaseType.MY_SQL, "sql7.freemysqlhosting.net", "3306", "sql7609487", "sql7609487", "CP2BKjCEKH");
        driver.executeDDLStatement("""
                CREATE TABLE SHAFT
                    (
                    TestID int,
                    Data varchar(255)
                    );""");
    }

    @AfterClass(alwaysRun = true)
    public void teardown() {
        driver.executeDDLStatement("DROP TABLE SHAFT;");
    }
}
