package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.List;

@SuppressWarnings("unchecked")
public class Test_Wizard_API {
    SHAFT.API driver;

    @Test
    public void test_get_matchesSchema() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        driver.assertThatResponse().matchesSchema("schema.json").perform();
    }

    @Test
    public void test_post_withBody_string() {
        driver = new SHAFT.API("https://reqres.in/");
        String body = """
                {
                    "name": "morpheus",
                    "job": "leader"
                }""";
        driver.post("api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_hashMap() {
        driver = new SHAFT.API("https://reqres.in/");
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        driver.post("api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_JSONObject() {
        driver = new SHAFT.API("https://reqres.in/");
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.JSONObject bodyObj = new org.json.JSONObject(body);
        driver.post("api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_JSONObject_simple() {
        driver = new SHAFT.API("https://reqres.in/");
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.simple.JSONObject bodyObj = new org.json.simple.JSONObject(body);
        driver.post("api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_assertResponseEqualsIgnoringOrder() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        driver.assertThatResponse().isEqualToFileContentIgnoringOrder("test_assertResponseEqualsIgnoringOrder.json").perform();
    }

    @Test
    public void test_assertResponseDoesNotEqualFileContent() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        driver.assertThatResponse().doesNotEqualFileContent("test_assertResponseEqualsIgnoringOrder.json").perform();
    }

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
