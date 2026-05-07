package com.shaft.api;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.EncoderConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.http.ContentType;
import io.restassured.http.Cookie;
import io.restassured.http.Cookies;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import com.sun.net.httpserver.HttpServer;

import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
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

        Assert.assertSame(actions.getDriver(), mockDriver);
        Assert.assertEquals(actions.prepareRequestURL("http://localhost/", "a=1", "users"), "http://localhost/users?a=1");
        Assert.assertEquals(actions.prepareRequestURL("http://localhost/", "", "users"), "http://localhost/users");
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
        Assert.assertNotNull(bodySpec);

        Map<String, Object> queryParams = new LinkedHashMap<>();
        queryParams.put("q", "value");
        RequestSpecification querySpec = actions.prepareRequestSpecs(
                queryParams, RestActions.ParametersType.QUERY, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, false);
        Assert.assertNotNull(querySpec);

        Path tempFile = Files.createTempFile(Path.of("/tmp"), "shaft-rest", ".txt");
        Files.writeString(tempFile, "payload");
        Map<String, Object> multipartParams = new LinkedHashMap<>();
        multipartParams.put("file", tempFile.toFile());
        multipartParams.put("description", "text-part");
        RequestSpecification multipartSpec = actions.prepareRequestSpecs(
                multipartParams, RestActions.ParametersType.MULTIPART, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, true);
        Assert.assertNotNull(multipartSpec);
    }

    @Test
    public void privateRequestBuilderHelpersAreCoveredViaReflection() throws Exception {
        RestActions actions = new RestActions("http://localhost/");

        RequestSpecBuilder builder = new RequestSpecBuilder();
        Method setConfigs = RestActions.class.getDeclaredMethod("setConfigs", RequestSpecBuilder.class, List.class);
        setConfigs.setAccessible(true);
        List<RestAssuredConfig> configs = List.of(
                RestAssuredConfig.config(),
                RestAssuredConfig.config().encoderConfig(new EncoderConfig().appendDefaultContentCharsetToContentTypeIfUndefined(false))
        );
        Object returnedBuilder = setConfigs.invoke(actions, builder, configs);
        Assert.assertNotNull(returnedBuilder);

        Method prepareRequestBodyList = RestActions.class.getDeclaredMethod(
                "prepareRequestBody", RequestSpecBuilder.class, List.class, RestActions.ParametersType.class);
        prepareRequestBodyList.setAccessible(true);
        Method prepareRequestBodyMap = RestActions.class.getDeclaredMethod(
                "prepareRequestBody", RequestSpecBuilder.class, Map.class, RestActions.ParametersType.class);
        prepareRequestBodyMap.setAccessible(true);

        Path tempFile = Files.createTempFile(Path.of("/tmp"), "shaft-legacy-form", ".bin");
        Files.writeString(tempFile, "legacy");
        List<List<Object>> params = new ArrayList<>();
        params.add(List.of("filePart", tempFile.toFile()));
        params.add(List.of("textPart", "value"));

        prepareRequestBodyList.invoke(actions, new RequestSpecBuilder(), params, RestActions.ParametersType.FORM);
        prepareRequestBodyList.invoke(actions, new RequestSpecBuilder(), List.of(List.of("q", "1")), RestActions.ParametersType.QUERY);
        prepareRequestBodyMap.invoke(actions, new RequestSpecBuilder(), Map.of("k", "v"), RestActions.ParametersType.FORM);
    }

    @Test
    public void sendRequestSupportsAllHttpMethods() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/resource", exchange -> {
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

            Assert.assertEquals(actions.sendRequest(RestActions.RequestType.GET, url, specs).getStatusCode(), 200);
            Assert.assertEquals(actions.sendRequest(RestActions.RequestType.POST, url, specs).getStatusCode(), 200);
            Assert.assertEquals(actions.sendRequest(RestActions.RequestType.PATCH, url, specs).getStatusCode(), 200);
            Assert.assertEquals(actions.sendRequest(RestActions.RequestType.PUT, url, specs).getStatusCode(), 200);
            Assert.assertEquals(actions.sendRequest(RestActions.RequestType.DELETE, url, specs).getStatusCode(), 200);
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

        Assert.assertTrue(message.contains("GET"));
        Assert.assertEquals(actions.getSessionHeaders().get("Authorization"), "Bearer abc123");
        Assert.assertEquals(actions.getSessionHeaders().get("X-XSRF-TOKEN"), "csrf-header");
    }

    @Test
    public void evaluateResponseStatusCodeAndStaticActionHelpersAreCovered() {
        RestActions actions = new RestActions("http://localhost/");
        Response ok = Mockito.mock(Response.class);
        Mockito.when(ok.getStatusCode()).thenReturn(200);
        Mockito.when(ok.getStatusLine()).thenReturn("HTTP/1.1 200 OK");
        Assert.assertTrue(actions.evaluateResponseStatusCode(ok, 0));

        Response mismatch = Mockito.mock(Response.class);
        Mockito.when(mismatch.getStatusCode()).thenReturn(500);
        Mockito.when(mismatch.getStatusLine()).thenReturn("HTTP/1.1 500 Internal Server Error");
        Assert.assertThrows(RuntimeException.class, () -> actions.evaluateResponseStatusCode(mismatch, 200));

        RestActions.passAction("covered-path");
        Assert.assertThrows(RuntimeException.class,
                () -> RestActions.failAction("force-failure", new RuntimeException("boom")));
    }

    @Test
    public void getResponseJsonValueSupportsMultipleInputShapes() throws Exception {
        Assert.assertEquals(RestActions.getResponseJSONValue("{\"name\":\"shaft\"}", "$.name"), "shaft");
        Assert.assertEquals(RestActions.getResponseJSONValue(new org.json.JSONObject("{\"id\":7}"), "id"), "7");
        Assert.assertEquals(RestActions.getResponseJSONValue(new org.json.JSONArray("[{\"value\":\"x\"}]"), "[0].value"), "x");
        Assert.assertEquals(RestActions.getResponseJSONValue(List.of(Map.of("k", "v")), "[0].k"), "v");
        Assert.assertEquals(RestActions.getResponseJSONValue(new java.util.HashMap<>(Map.of("key", "value")), "key"), "value");
    }

    @Test
    public void parseBodyToJsonCoversJacksonObjectAndArrayNodes() {
        ObjectMapper mapper = new ObjectMapper();
        ObjectNode objectNode = mapper.createObjectNode().put("name", "shaft");
        ArrayNode arrayNode = mapper.createArrayNode().add("a").add("b");

        Assert.assertNotNull(RestActions.parseBodyToJson(objectNode));
        Assert.assertNotNull(RestActions.parseBodyToJson(arrayNode));
        Assert.assertNotNull(RestActions.parseBodyToJson(Map.of("x", 1)));
    }

    @Test
    public void graphQlOverloadsAreCoveredAgainstLocalServer() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/graphql", exchange -> {
            byte[] body = "{\"data\":{\"ok\":true}}".getBytes();
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });
        server.start();

        try {
            String baseUri = "http://127.0.0.1:" + server.getAddress().getPort() + "/";
            Assert.assertEquals(RestActions.sendGraphQlRequest(baseUri, "{ ping }").statusCode(), 200);
            Assert.assertEquals(RestActions.sendGraphQlRequest(baseUri, "{ ping }", "{\"id\":1}").statusCode(), 200);
            Assert.assertEquals(RestActions.sendGraphQlRequest(baseUri, "{ ping }", "{\"id\":1}", "fragment").statusCode(), 200);
            Assert.assertEquals(RestActions.sendGraphQlRequestWithHeader(baseUri, "{ ping }", "Authorization", "Bearer token").statusCode(), 200);
            Assert.assertEquals(RestActions.sendGraphQlRequestWithHeader(baseUri, "{ ping }", "{\"id\":1}", "Authorization", "Bearer token").statusCode(), 200);
            Assert.assertEquals(RestActions.sendGraphQlRequestWithHeader(baseUri, "{ ping }", "{\"id\":1}", "fragment", "Authorization", "Bearer token").statusCode(), 200);
        } finally {
            server.stop(0);
        }
    }
}
