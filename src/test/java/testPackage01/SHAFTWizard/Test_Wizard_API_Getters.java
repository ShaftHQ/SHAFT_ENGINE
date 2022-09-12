package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

import java.util.List;


public class Test_Wizard_API_Getters {
    SHAFT.API driver;

    @Test
    public void test_getResponseBody() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        String body = driver.getResponseBody();
        SHAFT.Validations.assertThat().object(body).contains("Beverly Hills").perform();
    }

    @Test
    public void test_getResponseStatusCode() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        int statusCode = driver.getResponseStatusCode();
        SHAFT.Validations.assertThat().number(statusCode).isEqualTo(200).perform();
    }

    @Test
    public void test_getResponseTime() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        long responseTime = driver.getResponseTime();
        SHAFT.Validations.verifyThat().number(responseTime).isGreaterThanOrEquals(1.1).perform();
        SHAFT.Validations.verifyThat().number(responseTime).isLessThanOrEquals(10000).perform();
    }

    @Test
    public void test_getResponseJSONValue() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/users").perform();
        String value = driver.getResponseJSONValue("$[?(@.name=='Ervin Howell')].address.street");
        SHAFT.Validations.assertThat().object(value).isEqualTo("Victor Plains").perform();
    }

    @Test
    public void test_getResponseJSONValueAsList() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/todos").perform();
        List<Object> completedList = driver.getResponseJSONValueAsList("$[?(@.completed==true)].completed");
        for (Object completed : completedList) {
            SHAFT.Validations.verifyThat().object(completed.toString()).isEqualTo("true").perform();
        }
    }

}
