package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

public class APITests {
    ThreadLocal<SHAFT.API> driver = new ThreadLocal<>();
    private LocalApiServer localApiServer;
    private String localApiBaseUrl;

    @BeforeClass
    public void startLocalApiServer() {
        localApiServer = LocalApiServer.start();
        localApiBaseUrl = localApiServer.baseUrl();
    }

    @AfterClass(alwaysRun = true)
    public void stopLocalApiServer() {
        if (localApiServer != null) {
            localApiServer.close();
        }
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_filterExpression() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/users").perform();
        driver.get().assertThatResponse().extractedJsonValue("$[?(@.name=='Chelsey Dietrich')].id").isEqualTo("5").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_arrayIndex() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/users").perform();
        driver.get().assertThatResponse().extractedJsonValue("$[3].address.city").isEqualTo("South Elvis").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_numbers() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/users").perform();
        driver.get().assertThatResponse().extractedJsonValue("$[3].id").isEqualTo("4").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_boolean() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/todos").perform();
        driver.get().assertThatResponse().extractedJsonValue("$[1].completed").isEqualTo("false").perform();
    }

    @Test
    public void test_jsonValue_jsonResponse() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/posts/1").perform();
        driver.get().assertThatResponse().extractedJsonValue("id").isEqualTo("1").perform();
    }

    @Test
    public void test_jsonValueAdvancedEvaluation_jsonResponse() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/users").perform();
        driver.get().assertThatResponse().extractedJsonValue("$[?(@.id=='2')].name").isEqualTo("Ervin Howell").perform();
    }

    /////////////////////////  Getters  /////////////////////////
    @Test
    public void test_getResponseBody() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        String body = driver.get().getResponseBody();
        SHAFT.Validations.assertThat().object(body).contains("Beverly Hills").perform();
    }

    @Test
    public void test_getResponseStatusCode() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        int statusCode = driver.get().getResponseStatusCode();
        SHAFT.Validations.assertThat().number(statusCode).isEqualTo(200).perform();
    }

    @Test
    public void test_getResponseTime() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        long responseTime = driver.get().getResponseTime();
        SHAFT.Validations.verifyThat().number(responseTime).isGreaterThanOrEquals(1.1).perform();
        SHAFT.Validations.verifyThat().number(responseTime).isLessThanOrEquals(100000).perform();
    }

    @Test
    public void test_getResponseJSONValue() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/users").perform();
        String value = driver.get().getResponseJSONValue("$[?(@.name=='Ervin Howell')].address.street");
        SHAFT.Validations.assertThat().object(value).isEqualTo("Victor Plains").perform();
    }

    @Test
    public void test_getResponseJSONValueAsList() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/todos").perform();
        List<Object> completedList = driver.get().getResponseJSONValueAsList("$[?(@.completed==true)].completed");
        for (Object completed : completedList) {
            SHAFT.Validations.verifyThat().object(completed.toString()).isEqualTo("true").perform();
        }
    }

    @Test
    public void test_getResponseJSONValueAsList_validation() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/todos").perform();
        driver.get().verifyThatResponse().extractedJsonValueAsList("$[?(@.completed==true)].completed").isEqualTo("true").perform();
    }

    @Test
    public void testGetInstanceFactoryMethod() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.get("/users").perform();
        List<Object> names = api.getResponseJSONValueAsList("$..name");
        SHAFT.Validations.verifyThat().number(names.size()).isGreaterThanOrEquals(5).perform();
    }

    @Test
    public void testPatchRequestShouldTargetPatchEndpoint() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.patch("/method/patch").setRequestBody(Map.of("operation", "patch")).perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("method")).isEqualTo("PATCH").perform();
    }

    @Test
    public void testPutRequestShouldTargetPutEndpoint() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.put("/method/put").setRequestBody(Map.of("operation", "put")).perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("method")).isEqualTo("PUT").perform();
    }

    @Test
    public void testDeleteRequestShouldTargetDeleteEndpoint() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.delete("/method/delete").perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("method")).isEqualTo("DELETE").perform();
    }

    @Test
    public void testSendGraphQlRequestShouldSendBasicQuery() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.sendGraphQlRequest("/graphql", "query { ping }").perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("data.ping")).isEqualTo("pong").perform();
    }

    @Test
    public void testSendGraphQlRequestShouldSendQueryWithVariables() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.sendGraphQlRequest("/graphql", "query $id { ping(id: $id) }", Map.of("id", 42)).perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("data.ping")).isEqualTo("pong").perform();
    }

    @Test
    public void testSendGraphQlRequestShouldSendQueryWithVariablesAndFragment() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.sendGraphQlRequest("/graphql", "query { ping }", Map.of("id", 42), "fragment Ping on Query { ping }").perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("data.ping")).isEqualTo("pong").perform();
    }

    @Test
    public void testAddHeaderAndCookieShouldBeSentForSubsequentRequests() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.addHeader("x-custom-header", "header-value");
        api.addCookie("session", "abc123");
        api.get("/inspect").perform();

        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("header")).isEqualTo("header-value").perform();
        SHAFT.Validations.assertThat().object(api.getResponseJSONValue("cookie")).contains("session=abc123").perform();
    }

    @Test
    public void testGetResponseXMLValue() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.get("/xml/items").perform();
        String firstName = api.getResponseXMLValue("items.item[0].name");
        SHAFT.Validations.assertThat().object(firstName).isEqualTo("alpha").perform();
    }

    @Test
    public void testGetResponseXMLValueAsList() {
        SHAFT.API api = SHAFT.API.getInstance(localApiBaseUrl);
        api.get("/xml/items").perform();
        List<Object> itemNames = api.getResponseXMLValueAsList("items.item.name");
        SHAFT.Validations.assertThat().object(itemNames.size()).isEqualTo(2).perform();
        SHAFT.Validations.assertThat().object(itemNames.get(0).toString()).isEqualTo("alpha").perform();
        SHAFT.Validations.assertThat().object(itemNames.get(1).toString()).isEqualTo("beta").perform();
    }

    @Test
    public void test_validationWizard_getResponseBody() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        driver.get().assertThatResponse().body().contains("Beverly Hills").perform();
    }

    @Test
    public void test_validationWizard_getResponseTime() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/users").perform();
        driver.get().verifyThatResponse().time().isGreaterThanOrEquals(1).perform();
        driver.get().assertThatResponse().time().isLessThanOrEquals(100000.1).perform();
    }

}
