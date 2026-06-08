package com.shaft.api;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.Validations;
import com.sun.net.httpserver.HttpServer;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.RestAssuredConfig;
import io.restassured.http.ContentType;
import io.restassured.http.Cookie;
import io.restassured.http.Cookies;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import io.restassured.specification.FilterableRequestSpecification;
import io.restassured.specification.RequestSpecification;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class RestActionsCoverageUnitTest {
    private static final String COVERAGE_TEST_DATA = "src/test/resources/testDataFiles/restActionsCoverage/";

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
    public void buildNewRequestCreatesBuildersForEveryRequestType() {
        RestActions actions = new RestActions("http://localhost/");
        for (RestActions.RequestType requestType : RestActions.RequestType.values()) {
            RequestBuilder sessionBuilder = actions.buildNewRequest("resource", requestType);
            RequestBuilder staticBuilder = RestActions.buildNewRequest("http://localhost/", "resource", requestType);

            Validations.assertThat().object(sessionBuilder.getRequestType()).isEqualTo(requestType).perform();
            Validations.assertThat().object(sessionBuilder.getServiceName()).isEqualTo("resource").perform();
            Validations.assertThat().object(sessionBuilder.getServiceURI()).isEqualTo("http://localhost/").perform();
            Validations.assertThat().object(staticBuilder.getRequestType()).isEqualTo(requestType).perform();
        }
    }

    @Test
    public void requestBuilderHeaderCookieQueryAndFormHelpersMutateRequestState() {
        RestActions actions = new RestActions("http://localhost/");
        Map<String, Object> queryParameters = new LinkedHashMap<>();
        queryParameters.put("q", "shaft api");
        queryParameters.put("page", 2);
        Map<String, String> headers = new LinkedHashMap<>();
        headers.put("X-Trace", "trace-1");
        Map<String, String> cookies = new LinkedHashMap<>();
        cookies.put("feature", "enabled");

        RequestBuilder queryBuilder = actions.buildNewRequest("users/{userId}", RestActions.RequestType.GET)
                .setPathParameters(Map.of("userId", 10))
                .setUrlArguments("sort=asc")
                .setParameters(queryParameters, RestActions.ParametersType.QUERY)
                .addHeader("X-Request", "single")
                .addHeaders(headers)
                .addCookie("session", "abc")
                .addCookies(cookies)
                .appendDefaultContentCharsetToContentTypeIfUndefined(false)
                .enableUrlEncoding(false);

        Validations.assertThat().object(queryBuilder.getServiceName()).isEqualTo("users/10").perform();
        Validations.assertThat().object(queryBuilder.getUrlArguments()).isEqualTo("sort=asc").perform();
        Validations.assertThat().object(queryBuilder.getParametersType()).isEqualTo(RestActions.ParametersType.QUERY).perform();
        Validations.assertThat().object(queryBuilder.getParametersMap().get("q")).isEqualTo("shaft api").perform();
        Validations.assertThat().object(queryBuilder.getSessionHeaders().get("X-Request")).isEqualTo("single").perform();
        Validations.assertThat().object(queryBuilder.getSessionHeaders().get("X-Trace")).isEqualTo("trace-1").perform();
        Validations.assertThat().object(queryBuilder.getSessionCookies().get("session")).isEqualTo("abc").perform();
        Validations.assertThat().object(queryBuilder.getSessionCookies().get("feature")).isEqualTo("enabled").perform();

        Map<String, Object> formParameters = new LinkedHashMap<>();
        formParameters.put("username", "shaft");
        RequestBuilder formBuilder = actions.buildNewRequest("login", RestActions.RequestType.POST)
                .setParameters(formParameters, RestActions.ParametersType.FORM)
                .setContentType("application/json");

        Validations.assertThat().object(formBuilder.getParametersType()).isEqualTo(RestActions.ParametersType.FORM).perform();
        Validations.assertThat().object(formBuilder.getContentType()).isEqualTo(ContentType.JSON).perform();
        Assert.assertThrows(IllegalArgumentException.class,
                () -> actions.buildNewRequest("users", RestActions.RequestType.GET).setPathParameters("10"));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> actions.buildNewRequest("users/{id}", RestActions.RequestType.GET).setPathParameters(Map.of("missing", 10)));
    }

    @Test
    public void prepareRequestSpecsCoversBodyMapQueryFormAndMultipartParameterBranches() throws Exception {
        RestActions actions = new RestActions("http://localhost/");
        Map<String, String> headers = new LinkedHashMap<>();
        headers.put("Content-Type", "application/json");
        Map<String, Object> cookies = new LinkedHashMap<>();
        cookies.put("sid", "123");

        RequestSpecification bodySpec = actions.prepareRequestSpecs(
                null, null, "{\"name\":\"shaft\"}", ContentType.JSON, cookies, headers, RestAssuredConfig.config(), true, true);
        Validations.assertThat().object(bodySpec).isNotNull().perform();
        Validations.assertThat().object(actions.prepareRequestSpecs(
                null, null, new org.json.JSONObject(Map.of("name", "shaft")), ContentType.JSON, cookies, headers,
                RestAssuredConfig.config(), true, true)).isNotNull().perform();
        Validations.assertThat().object(actions.prepareRequestSpecs(
                null, null, new org.json.JSONArray(List.of("alpha", "beta")), ContentType.JSON, cookies, headers,
                RestAssuredConfig.config(), true, true)).isNotNull().perform();

        Map<String, Object> queryParams = new LinkedHashMap<>();
        queryParams.put("q", "value");
        RequestSpecification querySpec = actions.prepareRequestSpecs(
                queryParams, RestActions.ParametersType.QUERY, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, false);
        Validations.assertThat().object(((FilterableRequestSpecification) querySpec).getQueryParams().get("q"))
                .isEqualTo("value").perform();

        Map<String, Object> formParams = new LinkedHashMap<>();
        formParams.put("username", "shaft");
        formParams.put("page", 1);
        RequestSpecification formSpec = actions.prepareRequestSpecs(
                formParams, RestActions.ParametersType.FORM, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, true);
        FilterableRequestSpecification filterableFormSpec = (FilterableRequestSpecification) formSpec;
        Validations.assertThat().object(filterableFormSpec.getFormParams().get("username")).isEqualTo("shaft").perform();
        Validations.assertThat().object(filterableFormSpec.getContentType()).doesNotContain("multipart/form-data").perform();

        Path tempFile = Files.createTempFile("shaft-rest", ".txt");
        try {
            Files.writeString(tempFile, "payload");
            Map<String, Object> multipartParams = new LinkedHashMap<>();
            multipartParams.put("file", tempFile.toFile());
            multipartParams.put("description", "text-part");
            RequestSpecification multipartSpec = actions.prepareRequestSpecs(
                    multipartParams, RestActions.ParametersType.MULTIPART, null, ContentType.ANY, cookies, headers, RestAssuredConfig.config(), true, true);
            String multipartContentType = ((FilterableRequestSpecification) multipartSpec).getContentType();
            Validations.assertThat().object(multipartContentType).contains("multipart/form-data").perform();
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }

    @Test
    public void sendRequestSupportsAllHttpMethodsAgainstLocalServer() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/resource", exchange -> {
            try (InputStream requestBody = exchange.getRequestBody()) {
                requestBody.readAllBytes();
            }
            byte[] body = ("{\"method\":\"" + exchange.getRequestMethod() + "\"}").getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
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

            for (RestActions.RequestType requestType : RestActions.RequestType.values()) {
                Validations.assertThat().object(actions.sendRequest(requestType, url, specs).getStatusCode())
                        .isEqualTo(200).perform();
            }
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void responseMetadataAndXmlHelpersUseLocalServerResponses() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/xml", exchange -> {
            byte[] body = "<root><item id=\"1\" name=\"alpha\"/><item id=\"2\" name=\"beta\"/></root>"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/xml");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });
        server.start();

        try {
            String url = "http://127.0.0.1:" + server.getAddress().getPort() + "/xml";
            Response response = new RestActions("http://127.0.0.1:" + server.getAddress().getPort() + "/")
                    .sendRequest(RestActions.RequestType.GET, url, new RequestSpecBuilder().build());

            Validations.assertThat().object(RestActions.getResponseStatusCode(response)).isEqualTo(200).perform();
            Validations.assertThat().number(RestActions.getResponseTime(response)).isGreaterThanOrEquals(0).perform();
            Validations.assertThat().object(RestActions.getResponseBody(response)).contains("alpha").perform();
            Validations.assertThat().object(RestActions.getResponseXMLValue(response, "root.item[0].@name")).isEqualTo("alpha").perform();
            List<Object> nodes = RestActions.getResponseXMLValueAsList(response, "root.item");
            Validations.assertThat().object(nodes.size()).isEqualTo(2).perform();
            Validations.assertThat().object(RestActions.getResponseXMLValue(nodes.get(1), "name")).isEqualTo("beta").perform();
            Validations.assertThat().object(RestActions.formatXML("<root><child>value</child></root>")).contains(System.lineSeparator()).perform();
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
        Mockito.when(response.asString()).thenReturn("{\"type\":\"bearer\",\"token\":\"abc123\"}");
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenReturn(25L);

        String message = actions.prepareReportMessage(response, 200, RestActions.RequestType.GET, "users", ContentType.JSON, "a=1");

        Validations.assertThat().object(message).contains("GET").perform();
        Validations.assertThat().object(actions.getSessionHeaders().get("Authorization")).isEqualTo("Bearer abc123").perform();
        Validations.assertThat().object(actions.getSessionHeaders().get("X-XSRF-TOKEN")).isEqualTo("csrf-header").perform();
        Validations.assertThat().object(actions.getSessionCookies().get("XSRF-TOKEN")).isEqualTo("csrf-value").perform();
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
        Assert.assertThrows(RuntimeException.class, () -> actions.evaluateResponseStatusCode(mismatch, 200));

        RestActions.passAction("covered-path");
        Assert.assertThrows(RuntimeException.class,
                () -> RestActions.failAction("force-failure", new RuntimeException("boom")));
    }

    @Test
    public void getResponseJsonValueSupportsStringsCollectionsFiltersAndMockedResponses() throws Exception {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.asPrettyString()).thenReturn("{\"items\":[{\"id\":1,\"name\":\"alpha\"},{\"id\":2,\"name\":\"beta\"}]}");

        Validations.assertThat().object(RestActions.getResponseJSONValue("{\"name\":\"shaft\"}", "$.name")).isEqualTo("shaft").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(new org.json.JSONObject("{\"id\":7}"), "id")).isEqualTo("7").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(new org.json.JSONArray("[{\"value\":\"x\"}]"), "[0].value")).isEqualTo("x").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(List.of(Map.of("k", "v")), "[0].k")).isEqualTo("v").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(new java.util.HashMap<>(Map.of("key", "value")), "key")).isEqualTo("value").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValue(response, "$.items[?(@.id==2)].name")).isEqualTo("beta").perform();
        Validations.assertThat().object(RestActions.getResponseJSONValueFromList(response, "$.items", "name", "id", "1"))
                .isEqualTo("alpha").perform();
    }

    @Test
    public void parseBodyToJsonCoversStringsStreamsJacksonNodesMapsAndMockedResponseBodies() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        ObjectNode objectNode = mapper.createObjectNode().put("name", "shaft");
        ArrayNode arrayNode = mapper.createArrayNode().add("a").add("b");
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> responseBody = Mockito.mock(ResponseBody.class);
        Mockito.when(response.getBody()).thenReturn(responseBody);
        Mockito.when(responseBody.asString()).thenReturn("{\"from\":\"response\"}");

        Validations.assertThat().object(new String(RestActions.parseBodyToJson("{\"from\":\"string\"}").readAllBytes(), StandardCharsets.UTF_8))
                .contains("string").perform();
        Validations.assertThat().object(new String(RestActions.parseBodyToJson(
                        new ByteArrayInputStream("{\"from\":\"stream\"}".getBytes(StandardCharsets.UTF_8))).readAllBytes(), StandardCharsets.UTF_8))
                .contains("stream").perform();
        Validations.assertThat().object(new String(RestActions.parseBodyToJson(response).readAllBytes(), StandardCharsets.UTF_8))
                .contains("response").perform();
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
            byte[] body = "{\"data\":{\"ok\":true}}".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
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
    public void compareJsonCoversPositiveAndNegativeLocalFileStrategies() {
        Response objectResponse = Mockito.mock(Response.class);
        Mockito.when(objectResponse.asString()).thenReturn("{\"id\":1,\"name\":\"shaft\",\"active\":true,\"roles\":[\"admin\",\"tester\"]}");
        Mockito.when(objectResponse.asPrettyString()).thenReturn("{\"id\":1,\"name\":\"shaft\",\"active\":true,\"roles\":[\"admin\",\"tester\"]}");

        Response arrayResponse = Mockito.mock(Response.class);
        Mockito.when(arrayResponse.asString()).thenReturn("[{\"id\":2,\"name\":\"beta\"},{\"id\":1,\"name\":\"alpha\"}]");
        Mockito.when(arrayResponse.asPrettyString()).thenReturn("[{\"id\":2,\"name\":\"beta\"},{\"id\":1,\"name\":\"alpha\"}]");

        Response nestedArrayResponse = Mockito.mock(Response.class);
        Mockito.when(nestedArrayResponse.asString()).thenReturn("{\"data\":[{\"id\":1,\"name\":\"alpha\"},{\"id\":2,\"name\":\"beta\"},{\"id\":3,\"name\":\"gamma\"}]}");
        Mockito.when(nestedArrayResponse.asPrettyString()).thenReturn("{\"data\":[{\"id\":1,\"name\":\"alpha\"},{\"id\":2,\"name\":\"beta\"},{\"id\":3,\"name\":\"gamma\"}]}");

        Validations.assertThat().object(RestActions.compareJSON(
                objectResponse, COVERAGE_TEST_DATA + "expectedObject.json", RestActions.ComparisonType.EQUALS)).isTrue().perform();
        Validations.assertThat().object(RestActions.compareJSON(
                objectResponse, COVERAGE_TEST_DATA + "containsObject.json", RestActions.ComparisonType.CONTAINS)).isTrue().perform();
        Validations.assertThat().object(RestActions.compareJSON(
                arrayResponse, COVERAGE_TEST_DATA + "orderedArray.json", RestActions.ComparisonType.EQUALS_IGNORING_ORDER)).isTrue().perform();
        Validations.assertThat().object(RestActions.compareJSON(
                nestedArrayResponse, COVERAGE_TEST_DATA + "orderedArray.json", RestActions.ComparisonType.CONTAINS, "$.data")).isTrue().perform();
        Validations.assertThat().object(RestActions.compareJSON(
                objectResponse, COVERAGE_TEST_DATA + "mismatchObject.json", RestActions.ComparisonType.EQUALS)).isFalse().perform();
    }
}
