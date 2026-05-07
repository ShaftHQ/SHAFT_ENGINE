package com.shaft.api;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.RestAssuredConfig;
import io.restassured.http.ContentType;
import io.restassured.http.Cookie;
import io.restassured.http.Cookies;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import io.restassured.specification.FilterableRequestSpecification;
import io.restassured.specification.RequestSpecification;
import com.shaft.validation.Validations;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import com.sun.net.httpserver.HttpServer;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class RestActionsCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void constructorWithDriverAndPrepareRequestUrlAreCovered() {
        SHAFT.API mockDriver = Mockito.mock(SHAFT.API.class);
        RestActions actions = new RestActions("http://localhost/", mockDriver);

        Validations.assertThat().object(actions.getDriver()).isEqualTo(mockDriver).perform();
        Validations.assertThat().object(actions.prepareRequestURL("http://localhost/", "a=1", "users"))
                .isEqualTo("http://localhost/users?a=1").perform();
        Validations.assertThat().object(actions.prepareRequestURL("http://localhost/", "", "users"))
                .isEqualTo("http://localhost/users").perform();
    }

    @Test
    public void prepareRequestSpecsCoversBodyMapAndMultipartPaths() throws Exception {
        RestActions actions = new RestActions("http://localhost/");
        Map<String, String> headers = new LinkedHashMap<>();
        headers.put("Content-Type", "application/json");
        Map<String, Object> cookies = new LinkedHashMap<>();
        cookies.put("sid", "123");

        RequestSpecification bodySpec = actions.prepareRequestSpecs(
                null, null, "{\"name\":\"shaft\"}", ContentType.JSON, cookies, headers, RestAssuredConfig.config(), true, true);
        Validations.assertThat().object(bodySpec).isNotNull().perform();

        Map<String, Object> queryParams = new LinkedHashMap<>();
        queryParams.put("q", "value");
        RequestSpecification querySpec = actions.prepareRequestSpecs(
                queryParams, RestActions.ParametersType.QUERY, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, false);
        Validations.assertThat().object(querySpec).isNotNull().perform();

        Map<String, Object> formParams = new LinkedHashMap<>();
        formParams.put("username", "shaft");
        formParams.put("page", 1);
        RequestSpecification formSpec = actions.prepareRequestSpecs(
                formParams, RestActions.ParametersType.FORM, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, true);
        Validations.assertThat().object(formSpec).isNotNull().perform();
        String formContentType = ((FilterableRequestSpecification) formSpec).getContentType();
        Validations.assertThat().object(formContentType).doesNotContain("multipart/form-data").perform();

        Path tempFile = Files.createTempFile("shaft-rest", ".txt");
        try {
            Files.writeString(tempFile, "payload");
            Map<String, Object> multipartParams = new LinkedHashMap<>();
            multipartParams.put("file", tempFile.toFile());
            multipartParams.put("description", "text-part");
            RequestSpecification multipartSpec = actions.prepareRequestSpecs(
                    multipartParams, RestActions.ParametersType.MULTIPART, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, true);
            Validations.assertThat().object(multipartSpec).isNotNull().perform();
            String multipartContentType = ((FilterableRequestSpecification) multipartSpec).getContentType();
            Validations.assertThat().object(multipartContentType).contains("multipart/form-data").perform();
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }

    @Test
    public void sendRequestSupportsAllHttpMethods() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/resource", exchange -> {
            try (InputStream requestBody = exchange.getRequestBody()) {
                requestBody.readAllBytes();
            }
            byte[] body = "{\"ok\":true}".getBytes();
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });
        server.start();

        try {
            int port = server.getAddress().getPort();
            String url = "http://127.0.0.1:" + port + "/resource";
            RestActions actions = new RestActions("http://127.0.0.1:" + port + "/");
            RequestSpecification specs = new RequestSpecBuilder().build();

            Validations.assertThat().object(actions.sendRequest(RestActions.RequestType.GET, url, specs).getStatusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(actions.sendRequest(RestActions.RequestType.POST, url, specs).getStatusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(actions.sendRequest(RestActions.RequestType.PATCH, url, specs).getStatusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(actions.sendRequest(RestActions.RequestType.PUT, url, specs).getStatusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(actions.sendRequest(RestActions.RequestType.DELETE, url, specs).getStatusCode()).isEqualTo(200).perform();
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void prepareReportMessageExtractsCookiesHeadersAndBearerToken() {
        RestActions actions = new RestActions("http://localhost/");
        Response response = Mockito.mock(Response.class);
        JsonPath jsonPath = Mockito.mock(JsonPath.class);

        Mockito.when(response.getDetailedCookies())
                .thenReturn(new Cookies(new Cookie.Builder("XSRF-TOKEN", "csrf-value").build()));
        Mockito.when(response.getHeaders())
                .thenReturn(new Headers(new Header("Set-Cookie", "a=b"), new Header("X-XSRF-TOKEN", "csrf-header")));
        Mockito.when(response.jsonPath()).thenReturn(jsonPath);
        Mockito.when(jsonPath.getString("type")).thenReturn("bearer");
        Mockito.when(response.asPrettyString()).thenReturn("{\"type\":\"bearer\",\"token\":\"abc123\"}");
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenReturn(25L);

        String message = actions.prepareReportMessage(response, 200, RestActions.RequestType.GET, "users", ContentType.JSON, "a=1");

        Validations.assertThat().object(message).contains("GET").perform();
        Validations.assertThat().object(actions.getSessionHeaders().get("Authorization")).isEqualTo("Bearer abc123").perform();
        Validations.assertThat().object(actions.getSessionHeaders().get("X-XSRF-TOKEN")).isEqualTo("csrf-header").perform();
    }

    @Test
    public void prepareReportMessageReturnsEmptyWhenResponseIsNull() {
        RestActions actions = new RestActions("http://localhost/");
        String message = actions.prepareReportMessage(null, 200, RestActions.RequestType.GET, "users", ContentType.JSON, null);
        Validations.assertThat().object(message).isEqualTo("").perform();
    }

    @Test
    public void evaluateResponseStatusCodeAndStaticActionHelpersAreCovered() {
        RestActions actions = new RestActions("http://localhost/");
        Response ok = Mockito.mock(Response.class);
        Mockito.when(ok.getStatusCode()).thenReturn(200);
        Mockito.when(ok.getStatusLine()).thenReturn("HTTP/1.1 200 OK");
        Validations.assertThat().object(actions.evaluateResponseStatusCode(ok, 0)).isTrue().perform();

        Response mismatch = Mockito.mock(Response.class);
        Mockito.when(mismatch.getStatusCode()).thenReturn(500);
        Mockito.when(mismatch.getStatusLine()).thenReturn("HTTP/1.1 500 Internal Server Error");
        org.testng.Assert.assertThrows(RuntimeException.class, () -> actions.evaluateResponseStatusCode(mismatch, 200));

        RestActions.passAction("covered-path");
        org.testng.Assert.assertThrows(RuntimeException.class,
                () -> RestActions.failAction("force-failure", new RuntimeException("boom")));
    }

    @Test
    public void getResponseJsonValueSupportsMultipleInputShapes() throws Exception {
        Validations.assertThat().object(RestActions.getResponseJSONValue("{\"name\":\"shaft\"}", "$.name")).isEqualTo("shaft").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(new org.json.JSONObject("{\"id\":7}"), "id")).isEqualTo("7").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(new org.json.JSONArray("[{\"value\":\"x\"}]"), "[0].value")).isEqualTo("x").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(List.of(Map.of("k", "v")), "[0].k")).isEqualTo("v").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(new java.util.HashMap<>(Map.of("key", "value")), "key")).isEqualTo("value").perform();
    }

    @Test
    public void parseBodyToJsonCoversJacksonObjectAndArrayNodes() {
        ObjectMapper mapper = new ObjectMapper();
        ObjectNode objectNode = mapper.createObjectNode().put("name", "shaft");
        ArrayNode arrayNode = mapper.createArrayNode().add("a").add("b");

        Validations.assertThat().object(RestActions.parseBodyToJson(objectNode)).isNotNull().perform();
        Validations.assertThat().object(RestActions.parseBodyToJson(arrayNode)).isNotNull().perform();
        Validations.assertThat().object(RestActions.parseBodyToJson(Map.of("x", 1))).isNotNull().perform();
    }

    @Test
    public void graphQlOverloadsAreCoveredAgainstLocalServer() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/graphql", exchange -> {
            try (InputStream requestBody = exchange.getRequestBody()) {
                requestBody.readAllBytes();
            }
            byte[] body = "{\"data\":{\"ok\":true}}".getBytes();
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });
        server.start();

        try {
            String baseUri = "http://127.0.0.1:" + server.getAddress().getPort() + "/";
            Validations.assertThat().object(RestActions.sendGraphQlRequest(baseUri, "{ ping }").statusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(RestActions.sendGraphQlRequest(baseUri, "{ ping }", "{\"id\":1}").statusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(RestActions.sendGraphQlRequest(baseUri, "{ ping }", "{\"id\":1}", "fragment").statusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(RestActions.sendGraphQlRequestWithHeader(baseUri, "{ ping }", "Authorization", "Bearer token").statusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(RestActions.sendGraphQlRequestWithHeader(baseUri, "{ ping }", "{\"id\":1}", "Authorization", "Bearer token").statusCode()).isEqualTo(200).perform();
            Validations.assertThat().object(RestActions.sendGraphQlRequestWithHeader(baseUri, "{ ping }", "{\"id\":1}", "fragment", "Authorization", "Bearer token").statusCode()).isEqualTo(200).perform();
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void compareJsonCoversEqualsContainsAndIgnoringOrderStrategies() throws Exception {
        Path equalsFile = Files.createTempFile("restactions-expected-equals", ".json");
        Path containsFile = Files.createTempFile("restactions-expected-contains", ".json");
        Path orderFile = Files.createTempFile("restactions-expected-order", ".json");
        try {
            Files.writeString(equalsFile, "{\"id\":1,\"name\":\"shaft\"}");
            Files.writeString(containsFile, "{\"name\":\"shaft\"}");
            Files.writeString(orderFile, "[{\"name\":\"a\"},{\"name\":\"b\"}]");

            Response objectResponse = Mockito.mock(Response.class);
            Mockito.when(objectResponse.asString()).thenReturn("{\"id\":1,\"name\":\"shaft\"}");
            Mockito.when(objectResponse.asPrettyString()).thenReturn("{\"id\":1,\"name\":\"shaft\"}");

            Response arrayResponse = Mockito.mock(Response.class);
            Mockito.when(arrayResponse.asString()).thenReturn("[{\"name\":\"b\"},{\"name\":\"a\"}]");
            Mockito.when(arrayResponse.asPrettyString()).thenReturn("[{\"name\":\"b\"},{\"name\":\"a\"}]");

            Validations.assertThat().object(RestActions.compareJSON(
                    objectResponse, equalsFile.toString(), RestActions.ComparisonType.EQUALS)).isTrue().perform();
            Validations.assertThat().object(RestActions.compareJSON(
                    objectResponse, containsFile.toString(), RestActions.ComparisonType.CONTAINS)).isTrue().perform();
            Validations.assertThat().object(RestActions.compareJSON(
                    arrayResponse, orderFile.toString(), RestActions.ComparisonType.EQUALS_IGNORING_ORDER)).isTrue().perform();
        } finally {
            Files.deleteIfExists(equalsFile);
            Files.deleteIfExists(containsFile);
            Files.deleteIfExists(orderFile);
        }
    }
}
