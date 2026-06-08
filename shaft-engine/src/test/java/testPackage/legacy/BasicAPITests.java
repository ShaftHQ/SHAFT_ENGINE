package testPackage.legacy;

import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import com.sun.net.httpserver.HttpServer;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class BasicAPITests {
    private String apiFixtureUrl;
    private HttpServer apiFixture;
    SHAFT.API api;

    @BeforeClass
    public void setUpApiFixture() throws IOException {
        apiFixture = HttpServer.create(new InetSocketAddress(0), 0);
        apiFixture.createContext("/get", exchange -> {
            try {
                byte[] response = buildHttpBinResponse(exchange.getRequestURI().getRawQuery(), "", exchange.getRequestHeaders()).getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, response.length);
                try (OutputStream responseBody = exchange.getResponseBody()) {
                    responseBody.write(response);
                }
            } finally {
                exchange.close();
            }
        });
        apiFixture.createContext("/users", exchange -> {
            try {
                byte[] response = "[{\"id\":1,\"name\":\"SHAFT Fixture User\"}]".getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, response.length);
                try (OutputStream responseBody = exchange.getResponseBody()) {
                    responseBody.write(response);
                }
            } finally {
                exchange.close();
            }
        });
        apiFixture.createContext("/post", exchange -> {
            try {
                String requestBody = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
                byte[] response = buildHttpBinResponse(exchange.getRequestURI().getRawQuery(), requestBody, exchange.getRequestHeaders()).getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, response.length);
                try (OutputStream responseBody = exchange.getResponseBody()) {
                    responseBody.write(response);
                }
            } finally {
                exchange.close();
            }
        });
        apiFixture.start();
        apiFixtureUrl = "http://localhost:" + apiFixture.getAddress().getPort() + "/";
    }

    @AfterClass(alwaysRun = true)
    public void tearDownApiFixture() {
        if (apiFixture != null) {
            apiFixture.stop(0);
        }
    }

    @Test
    public void apiTest() {
        api = new SHAFT.API(apiFixtureUrl);
        api.get("users")
                .performRequest();
    }

    @Test
    public void apiTest2() {
        Map<String, Object> queryParameters = Map.of("FirstName", "Abdelrahman", "LastName", "Fahd");
        String body = "{\n" +
                "\"Body1\": \"Abdelrahman\",\n" +
                "\"Body2\": \"Fahd\"\n" +
                "}";
        api = new SHAFT.API(apiFixtureUrl);
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
        api = new SHAFT.API(apiFixtureUrl);
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
        api = new SHAFT.API(apiFixtureUrl);
        api.post("post")
                .setParameters(formParameters, RestActions.ParametersType.FORM)
                .perform();
    }

    @Test(description = "This test is used to Test add headers to the request")
    public void addHeadersMethodTest() {
        Map<String, String> headers = Map.of("Firstheader", "FirstHeaderValue",
                "Secondheader", "SecondHeaderValue");
        api = new SHAFT.API(apiFixtureUrl);
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
    public void addSingleHeadersMethodTest() {
        Map<String, String> headers = Map.of("Firstheader", "FirstHeaderValue");
        api = new SHAFT.API(apiFixtureUrl);
        api.get("get")
                .addHeaders(headers)
                .setTargetStatusCode(200)
                .perform();
        api.assertThatResponse().extractedJsonValue("headers.Firstheader")
                .isEqualTo("FirstHeaderValue")
                .perform();
    }

    @Test(description = "This test is used to Test add headers to the request")
    public void addEmptyHeadersMethodTest() {
        Map<String, String> headers = Map.of();
        api = new SHAFT.API(apiFixtureUrl);
        api.get("get")
                .addHeaders(headers)
                .setTargetStatusCode(200)
                .perform();
    }

    @Test(description = "This test is used to Test add cookies to the request")
    public void addCookiesMethodTest() {
        Map<String, String> cookies = Map.of("FirstCookie", "FirstCookieValue",
                "SecondCookie", "SecondCookieValue");
        api = new SHAFT.API(apiFixtureUrl);
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
    public void addSingleCookiesMethodTest() {
        Map<String, String> cookies = Map.of("FirstCookie", "FirstCookieValue");
        api = new SHAFT.API(apiFixtureUrl);
        api.get("get")
                .addCookies(cookies)
                .setTargetStatusCode(200)
                .perform();
        api.assertThatResponse().extractedJsonValue("headers.Cookie")
                .contains("FirstCookie=FirstCookieValue")
                .perform();
    }

    @Test(description = "This test is used to Test add cookies to the request")
    public void addEmptyCookiesMethodTest() {
        Map<String, String> cookies = Map.of();
        api = new SHAFT.API(apiFixtureUrl);
        api.get("get")
                .addCookies(cookies)
                .setTargetStatusCode(200)
                .perform();
    }

    private String buildHttpBinResponse(String rawQuery, String requestBody, Map<String, List<String>> requestHeaders) {
        return "{"
                + "\"args\":" + toJsonObject(decodeParameters(rawQuery)) + ","
                + "\"form\":" + toJsonObject(decodeParameters(requestBody)) + ","
                + "\"headers\":" + toJsonObject(flattenHeaders(requestHeaders))
                + "}";
    }

    private Map<String, String> flattenHeaders(Map<String, List<String>> requestHeaders) {
        Map<String, String> headers = new LinkedHashMap<>();
        requestHeaders.forEach((headerName, headerValues) -> headers.put(headerName, String.join(", ", headerValues)));
        return headers;
    }

    private Map<String, String> decodeParameters(String encodedParameters) {
        Map<String, String> parameters = new LinkedHashMap<>();
        if (encodedParameters == null || encodedParameters.isBlank()) {
            return parameters;
        }
        for (String parameter : encodedParameters.split("&")) {
            String[] keyValue = parameter.split("=", 2);
            String key = decodeUrlValue(keyValue[0]);
            String value = keyValue.length > 1 ? decodeUrlValue(keyValue[1]) : "";
            parameters.put(key, value);
        }
        return parameters;
    }

    private String decodeUrlValue(String value) {
        return URLDecoder.decode(value, StandardCharsets.UTF_8);
    }

    private String toJsonObject(Map<String, String> values) {
        return values.entrySet().stream()
                .map(entry -> "\"" + escapeJson(entry.getKey()) + "\":\"" + escapeJson(entry.getValue()) + "\"")
                .collect(Collectors.joining(",", "{", "}"));
    }

    private String escapeJson(String value) {
        return value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
    }

}
