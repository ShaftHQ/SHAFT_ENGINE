package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.HashMap;

public class Test_Wizard_API {
    SHAFT.API driver;

    @Test
    public void test() {
        driver = new SHAFT.API("http://api.zippopotam.us/");
        driver.get("us/90210").perform();
        driver.assertThatResponse().matchesSchema("schema.json").perform();
    }

    @Test
    public void test_post_withbody_string() {
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
    public void test_post_withbody_hashMap() {
        driver = new SHAFT.API("https://reqres.in/");
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        driver.post("api/users").setRequestBody(body).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withbody_JSONObject() {
        driver = new SHAFT.API("https://reqres.in/");
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.JSONObject bodyObj = new org.json.JSONObject(body);
        driver.post("api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }

    @Test
    public void test_post_withbody_JSONObject_simple() {
        driver = new SHAFT.API("https://reqres.in/");
        HashMap body = new HashMap<>();
        body.put("name", "morpheus");
        body.put("job", "leader");
        org.json.simple.JSONObject bodyObj = new org.json.simple.JSONObject(body);
        driver.post("api/users").setRequestBody(bodyObj).setTargetStatusCode(201).setContentType(ContentType.JSON).perform();
        driver.assertThatResponse().extractedJsonValue("name").isEqualTo("morpheus").withCustomReportMessage("Check that Morpheus exists.").perform();
    }
}
