package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.db.DatabaseActions.*;
import org.testng.annotations.*;

public class DbTests {
    ThreadLocal<SHAFT.DB> databaseInstance = new ThreadLocal<>();

    @BeforeClass
    public void beforeMethod(){
        databaseInstance.set(new SHAFT.DB(DatabaseType.POSTGRES_SQL, "localhost","5432","postgres","postgres","postgres"));
    }

    @AfterClass
    public void afterMethod(){
        databaseInstance.remove();
    }

    @Test
    public void select(){
        var instance = databaseInstance.get();
        instance.executeSelectQuery("SELECT * FROM student");
        SHAFT.Validations.assertThat().object(instance.getResult()).contains("Octocat");
    }

    @Test(dependsOnMethods = {"select"})
    public void insert() {
        var instance = databaseInstance.get();
        instance.executeInsertQuery("INSERT INTO student(firstname, lastname, age, address, email) VALUES('Ahmed the', 'Octocat', 9, '88 Colin P Kelly Jr St, San Francisco, CA 94107, United States', 'ahmedoctocat@github.com')");
        SHAFT.Validations.assertThat().object(instance.getResult()).contains("Ahmed");
    }

    @Test(dependsOnMethods = {"select","insert"})
    public void update() {
        var instance = databaseInstance.get();
        instance.executeUpdateQuery("UPDATE student SET age = 8 WHERE email = octocat@github.com';");
        SHAFT.Validations.assertThat().object(instance.getResult()).contains("8");
    }

    @Test(dependsOnMethods = {"select","insert","update"})
    public void delete() {
        var instance = databaseInstance.get();
        instance.executeDeleteQuery("DELETE FROM student WHERE email = octocat@github.com';");
        SHAFT.Validations.assertThat().object(instance.getResult()).doesNotContain("8");
    }

}
