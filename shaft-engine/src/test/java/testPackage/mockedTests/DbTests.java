package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.db.DatabaseActions.*;
import org.testng.annotations.*;

public class DbTests {
    SHAFT.DB databaseInstance;

    @BeforeClass
    public void beforeMethod(){
        databaseInstance = new SHAFT.DB(DatabaseType.POSTGRES_SQL, "localhost","5432","postgres","postgres","postgres");
    }

    @Test
    public void select(){
        databaseInstance.executeSelectQuery("SELECT * FROM student");
        SHAFT.Validations.assertThat().object(databaseInstance.getResult()).contains("Octocat");
    }

    @Test(dependsOnMethods = {"select"})
    public void insert() {
        databaseInstance.executeInsertQuery("INSERT INTO student(firstname, lastname, age, address, email) VALUES('Ahmed the', 'Octocat', 9, '88 Colin P Kelly Jr St, San Francisco, CA 94107, United States', 'ahmedoctocat@github.com')");
        databaseInstance.executeSelectQuery("SELECT * FROM student");
        SHAFT.Validations.assertThat().object(databaseInstance.getResult()).contains("Ahmed");
    }

    @Test(dependsOnMethods = {"select","insert"})
    public void update() {
        databaseInstance.executeUpdateQuery("UPDATE student SET age = 8 WHERE email = 'octocat@github.com';");
        databaseInstance.executeSelectQuery("SELECT * FROM student");
        SHAFT.Validations.assertThat().object(databaseInstance.getResult()).contains("8");
    }

    @Test(dependsOnMethods = {"select","insert","update"})
    public void delete() {
        databaseInstance.executeDeleteQuery("DELETE FROM student WHERE email = 'octocat@github.com';");
        databaseInstance.executeSelectQuery("SELECT * FROM student");
        SHAFT.Validations.assertThat().object(databaseInstance.getResult()).doesNotContain("Mona");
    }

}
