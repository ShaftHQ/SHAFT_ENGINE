package testPackage.legacy;

import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.*;

public class BasicAPITests {
    SHAFT.API api;

    @Test
    public void apiTest() {
        api = new SHAFT.API("https://fakerestapi.azurewebsites.net");
        api.get("/api/v1/Authors")
                .performRequest();
    }

    @Test
    public void apiTest2() {
        List<List<Object>> queryParameters = Arrays.asList(Arrays.asList("FirstName", "Abdelrahman"), Arrays.asList("LastName", "Fahd"));
        String body = "{\n" +
                "\"Body1\": \"Abdelrahman\",\n" +
                "\"Body2\": \"Fahd\"\n" +
                "}";
        api = new SHAFT.API("https://httpbin.org/");
        api.post("post").setRequestBody(body).setParameters(queryParameters, RestActions.ParametersType.QUERY).perform();

        api.assertThatResponse().extractedJsonValue("args.FirstName")
                .isEqualTo("Abdelrahman")
                .perform();
        api.assertThatResponse().extractedJsonValue("args.LastName")
                .isEqualTo("Fahd")
                .perform();
    }

    @Test
    public void apiTest3() {
        Map <String, Object> queryParameters = Map.of("FirstName", "Abdelrahman",
                "LastName", "Fahd");
        String body = "{\n" +
                "\"Body1\": \"Abdelrahman\",\n" +
                "\"Body2\": \"Fahd\"\n" +
                "}";
        api = new SHAFT.API("https://httpbin.org/");
        api.post("post").setRequestBody(body).setParameters(queryParameters, RestActions.ParametersType.QUERY).perform();

        api.assertThatResponse().extractedJsonValue("args.FirstName")
                .isEqualTo("Abdelrahman")
                .perform();
        api.assertThatResponse().extractedJsonValue("args.LastName")
                .isEqualTo("Fahd")
                .perform();

    }

    @Test
    public void apiTest4() {
        Map <String, Object> formParameters = Map.of("name", "SHAFT",
                                                     "job", "Automation Engineer");
        api = new SHAFT.API("https://reqres.in/api");

        api.post("users")
                .setParameters(formParameters, RestActions.ParametersType.FORM)
                .setTargetStatusCode(200)
                .perform();
    }
}
