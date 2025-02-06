package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.HashMap;

@SuppressWarnings("unchecked")
public class APIWizardTests {
    private ThreadLocal<SHAFT.API> driver = new ThreadLocal<>();

    @Test
    public void test_get_matchesSchema() {
        driver.set(new SHAFT.API("http://api.zippopotam.us/"));
        driver.get().get("us/90210").perform();
        driver.get().assertThatResponse().matchesSchema("schema.json").perform();
    }

    @Test(enabled = false)
    public void test_post_withBody_string() {
        driver.set(new SHAFT.API("https://reqres.in/"));
        String body = """
                {
                    "name": "morpheus",
                    "job": "leader"
                }""";
        driver.get().post("api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test(enabled = false)
    public void test_post_withBody_fromFile() {
        driver.set(new SHAFT.API("https://reqres.in/"));
        driver.get().post("api/users").setRequestBodyFromFile("test_post_withBody_fromFile.json").setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test(enabled = false)
    public void test_post_withBody_hashMap() {
        driver.set(new SHAFT.API("https://reqres.in/"));
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        driver.get().post("api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test(enabled = false)
    public void test_post_withBody_JSONObject() {
        driver.set(new SHAFT.API("https://reqres.in/"));
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.JSONObject bodyObj = new org.json.JSONObject(body);
        driver.get().post("api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test(enabled = false)
    public void test_post_withBody_JSONObject_simple() {
        driver.set(new SHAFT.API("https://reqres.in/"));
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.simple.JSONObject bodyObj = new org.json.simple.JSONObject(body);
        driver.get().post("api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.get().assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_assertResponseEqualsIgnoringOrder() {
        driver.set(new SHAFT.API("http://api.zippopotam.us/"));
        driver.get().get("us/90210").perform();
        driver.get().assertThatResponse().isEqualToFileContentIgnoringOrder("test_assertResponseEqualsIgnoringOrder.json").perform();
    }

    @Test
    public void test_assertResponseDoesNotEqualFileContent() {
        driver.set(new SHAFT.API("http://api.zippopotam.us/"));
        driver.get().get("us/90210").perform();
        driver.get().assertThatResponse().doesNotEqualFileContent("test_assertResponseEqualsIgnoringOrder.json").perform();
    }
}
