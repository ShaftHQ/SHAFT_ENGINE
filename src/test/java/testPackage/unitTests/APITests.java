package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

import java.util.List;

public class APITests {
    SHAFT.API driver;

    @Test
    public void test_jsonValueAdvancedEvaluation_filterExpression() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/users").perform();
        driver.assertThatResponse().extractedJsonValue("$[?(@.name=='Chelsey Dietrich')].id").isEqualTo("5").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_arrayIndex() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/users").perform();
        driver.assertThatResponse().extractedJsonValue("$[3].address.city").isEqualTo("South Elvis").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_numbers() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/users").perform();
        driver.assertThatResponse().extractedJsonValue("$[3].id").isEqualTo("4").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_boolean() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/todos").perform();
        driver.assertThatResponse().extractedJsonValue("$[1].completed").isEqualTo("false").perform();
    }

    @Test
    public void test_jsonValue_htmlResponse() {
        driver = new SHAFT.API("https://automationexercise.com/api");
        driver.get("/productsList").perform();
        driver.assertThatResponse().extractedJsonValue("responseCode").isEqualTo("200").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_htmlResponse() {
        driver = new SHAFT.API("https://automationexercise.com/api");
        driver.get("/productsList").perform();
        driver.assertThatResponse().extractedJsonValue("$.products[?(@.id=='2')].name").isEqualTo("Men Tshirt").perform();
    }

    /////////////////////////  Getters  /////////////////////////
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
        SHAFT.Validations.verifyThat().number(responseTime).isLessThanOrEquals(100000).perform();
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

    @Test
    public void test_getResponseJSONValueAsList_validation() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/todos").perform();
        driver.verifyThatResponse().extractedJsonValueAsList("$[?(@.completed==true)].completed").isEqualTo("true").perform();
    }

    @Test
    public void test_validationWizard_getResponseBody() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        driver.assertThatResponse().body().contains("Beverly Hills").perform();
    }

    @Test
    public void test_validationWizard_getResponseTime() {
        driver = new SHAFT.API("https://jsonplaceholder.typicode.com");
        driver.get("/users").perform();
        driver.verifyThatResponse().time().isGreaterThanOrEquals(1).perform();
        driver.assertThatResponse().time().isLessThanOrEquals(100000.1).perform();
    }

}
