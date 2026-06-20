package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import io.restassured.http.ContentType;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import testPackage.unitTests.LocalApiServer;

import java.util.HashMap;

@SuppressWarnings("unchecked")
public class APIWizardTests {
    private final ThreadLocal<SHAFT.API> driver = new ThreadLocal<>();
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
    public void test_get_matchesSchema() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        driver.get().assertThatResponse().matchesSchema("schema.json").perform();
    }

    @Test
    public void test_post_withBody_string() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        String body = """
                {
                    "name": "morpheus",
                    "job": "leader"
                }""";
        driver.get().post("/api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_fromFile() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().post("/api/users").setRequestBodyFromFile("test_post_withBody_fromFile.json").setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_hashMap() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        driver.get().post("/api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_JSONObject() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.JSONObject bodyObj = new org.json.JSONObject(body);
        driver.get().post("/api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withBody_JSONObject_simple() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.JSONObject bodyObj = new org.json.JSONObject(body);
        driver.get().post("/api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_assertResponseEqualsIgnoringOrder() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        driver.get().assertThatResponse().isEqualToFileContentIgnoringOrder("test_assertResponseEqualsIgnoringOrder.json").perform();
    }

    @Test
    public void test_assertResponseDoesNotEqualFileContent() {
        driver.set(new SHAFT.API(localApiBaseUrl));
        driver.get().get("/us/90210").perform();
        driver.get().assertThatResponse().doesNotEqualFileContent("test_assertResponseEqualsIgnoringOrder.json").perform();
    }
}
