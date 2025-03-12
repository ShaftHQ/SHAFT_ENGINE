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
        Map<String, Object> queryParameters = Map.of("FirstName", "Abdelrahman",
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
        Map<String, Object> formParameters = Map.of("name", "SHAFT",
                "job", "Automation Engineer");
        api = new SHAFT.API("https://reqres.in/api/");

        api.post("users")
                .setParameters(formParameters, RestActions.ParametersType.FORM)
                .setTargetStatusCode(201)
                .perform();
    }

    @Test(description = "This test is used to Test add headers to the request")
    public void addHeadersMethodTest() {
        Map<String, String> headers = Map.of("Firstheader", "FirstHeaderValue",
                "Secondheader", "SecondHeaderValue");
        api = new SHAFT.API("https://httpbin.org/");
        api.get("get")
                .addHeaders(headers)
                .setTargetStatusCode(200)
                .perform();
        api.assertThatResponse().extractedJsonValue("headers.Firstheader")
                .isEqualTo("FirstHeaderValue")
                .perform();
        api.assertThatResponse().extractedJsonValue("headers.Secondheader")
                .isEqualTo("SecondHeaderValue")
                .perform();
    }

    @Test(description = "This test is used to Test add headers to the request")
    public void addEmptyHeadersMethodTest() {
        Map<String, String> headers = Map.of();
        api = new SHAFT.API("https://httpbin.org/");
        api.get("get")
                .addHeaders(headers)
                .setTargetStatusCode(200)
                .perform();
    }

    @Test(description = "This test is used to Test add cookies to the request")
    public void addCookiesMethodTest() {
        Map<String, String> cookies = Map.of("FirstCookie", "FirstCookieValue",
                "SecondCookie", "SecondCookieValue");
        api = new SHAFT.API("https://httpbin.org/");
        api.get("get")
                .addCookies(cookies)
                .setTargetStatusCode(200)
                .perform();
        api.assertThatResponse().extractedJsonValue("headers.Cookie")
                .contains("FirstCookie=FirstCookieValue")
                .perform();
        api.assertThatResponse().extractedJsonValue("headers.Cookie")
                .contains("SecondCookie=SecondCookieValue")
                .perform();
    }

    @Test(description = "This test is used to Test add cookies to the request")
    public void addEmptyCookiesMethodTest() {
        Map<String, String> cookies = Map.of();
        api = new SHAFT.API("https://httpbin.org/");
        api.get("get")
                .addCookies(cookies)
                .setTargetStatusCode(200)
                .perform();
    }

}
