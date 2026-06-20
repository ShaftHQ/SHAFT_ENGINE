package com.shaft.api;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import io.restassured.config.RestAssuredConfig;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class RequestBuilderTests {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        RequestBuilder.getPerformanceData().clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void constructorAndFluentSettersShouldUpdateBuilderState() {
        RestActions mockSession = createSessionMock();

        RequestBuilder requestBuilder = new RequestBuilder(mockSession, "users", RestActions.RequestType.GET)
                .useRelaxedHTTPSValidation()
                .useRelaxedHTTPSValidation("TLS")
                .setTargetStatusCode(201)
                .setUrlArguments("a=1")
                .setParameters(Map.of("q", "test"), RestActions.ParametersType.QUERY)
                .setRequestBody("payload")
                .setContentType(ContentType.JSON)
                .setContentType("application/xml")
                .appendDefaultContentCharsetToContentTypeIfUndefined(false)
                .enableUrlEncoding(false)
                .addHeader("Authorization", "Bearer token")
                .addHeaders(Map.of("X-Trace", "true"))
                .addCookie("sid", "123")
                .addCookies(Map.of("tenant", "shaft"))
                .setAuthentication("user", "pass", RequestBuilder.AuthenticationType.BASIC);

        Assert.assertEquals(requestBuilder.getServiceName(), "users");
        Assert.assertEquals(requestBuilder.getRequestType(), RestActions.RequestType.GET);
        Assert.assertEquals(requestBuilder.getTargetStatusCode(), 201);
        Assert.assertEquals(requestBuilder.getUrlArguments(), "a=1");
        Assert.assertEquals(requestBuilder.getParametersMap().get("q"), "test");
        Assert.assertEquals(requestBuilder.getParametersType(), RestActions.ParametersType.QUERY);
        Assert.assertEquals(requestBuilder.getRequestBody(), "payload");
        Assert.assertEquals(requestBuilder.getContentType(), ContentType.XML);
        Assert.assertFalse(requestBuilder.isAppendDefaultContentCharsetToContentTypeIfUndefined());
        Assert.assertFalse(requestBuilder.isUrlEncodingEnabled());
        Assert.assertEquals(requestBuilder.getSessionHeaders().get("Authorization"), "Bearer token");
        Assert.assertEquals(requestBuilder.getSessionHeaders().get("X-Trace"), "true");
        Assert.assertEquals(requestBuilder.getSessionCookies().get("sid"), "123");
        Assert.assertEquals(requestBuilder.getSessionCookies().get("tenant"), "shaft");
        Assert.assertEquals(requestBuilder.getAuthenticationType(), RequestBuilder.AuthenticationType.BASIC);
        Assert.assertEquals(requestBuilder.getAuthenticationUsername(), "user");
        Assert.assertEquals(requestBuilder.getAuthenticationPassword(), "pass");

        Mockito.verify(mockSession, Mockito.times(1)).getServiceURI();
        Mockito.verify(mockSession, Mockito.times(1)).getSessionCookies();
        Mockito.verify(mockSession, Mockito.times(1)).getSessionHeaders();
        Mockito.verify(mockSession, Mockito.times(1)).getSessionConfig();
    }

    @Test
    public void setPathParametersShouldReplaceMapAndOrderedPlaceholders() {
        RequestBuilder fromMap = new RequestBuilder(createSessionMock(), "users/{id}/orders/{orderId}", RestActions.RequestType.GET)
                .setPathParameters(Map.of("id", 7, "orderId", "A1"));

        Assert.assertEquals(fromMap.getServiceName(), "users/7/orders/A1");

        RequestBuilder fromValues = new RequestBuilder(createSessionMock(), "users/{id}/orders/{orderId}", RestActions.RequestType.GET)
                .setPathParameters("7", "A1");

        Assert.assertEquals(fromValues.getServiceName(), "users/7/orders/A1");
    }

    @Test
    public void setPathParametersShouldThrowForMissingOrMismatchedPlaceholders() {
        RequestBuilder mapBuilder = new RequestBuilder(createSessionMock(), "users/{id}", RestActions.RequestType.GET);
        Assert.assertThrows(IllegalArgumentException.class, () -> mapBuilder.setPathParameters(Map.of("missing", "x")));

        RequestBuilder noPlaceholderBuilder = new RequestBuilder(createSessionMock(), "users/id", RestActions.RequestType.GET);
        Assert.assertThrows(IllegalArgumentException.class, () -> noPlaceholderBuilder.setPathParameters("1"));

        RequestBuilder mismatchBuilder = new RequestBuilder(createSessionMock(), "users/{id}/orders/{orderId}", RestActions.RequestType.GET);
        Assert.assertThrows(IllegalArgumentException.class, () -> mismatchBuilder.setPathParameters("1"));
    }

    @Test
    public void setRequestBodyFromFileShouldReadFileContents() throws Exception {
        Path tempFile = Files.createTempFile("request-body", ".txt");
        try {
            Files.writeString(tempFile, "{\"name\":\"shaft\"}");
            RequestBuilder builder = new RequestBuilder(createSessionMock(), "users", RestActions.RequestType.POST)
                    .setRequestBodyFromFile(tempFile.toString());
            Assert.assertTrue(builder.getRequestBody().toString().contains("\"name\":\"shaft\""));
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }

    @Test
    public void shaftApiGraphQlFacadeShouldBuildPostJsonRequest() {
        RequestBuilder builder = new SHAFT.API("http://localhost/")
                .sendGraphQlRequest("graphql", "{ ping }", Map.of("id", 1), "fragment Ping on Query { ping }")
                .addHeader("Authorization", "Bearer token");
        Map<?, ?> body = (Map<?, ?>) builder.getRequestBody();

        Assert.assertEquals(builder.getServiceName(), "graphql");
        Assert.assertEquals(builder.getRequestType(), RestActions.RequestType.POST);
        Assert.assertEquals(builder.getContentType(), ContentType.JSON);
        Assert.assertEquals(body.get("query"), "{ ping }");
        Assert.assertEquals(body.get("variables"), Map.of("id", 1));
        Assert.assertEquals(body.get("fragment"), "fragment Ping on Query { ping }");
        Assert.assertEquals(builder.getSessionHeaders().get("Authorization"), "Bearer token");
    }

    @Test
    public void shaftApiGraphQlFacadeShouldOmitUnsetVariablesAndFragment() {
        RequestBuilder builder = new SHAFT.API("http://localhost/")
                .sendGraphQlRequest("graphql", "{ ping }");
        Map<?, ?> body = (Map<?, ?>) builder.getRequestBody();

        Assert.assertEquals(body, Map.of("query", "{ ping }"));
    }

    @Test
    public void performRequestShouldCreateDriverAndStorePerformanceDataWhenDriverIsMissing() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);
        Response response = Mockito.mock(Response.class);

        Mockito.when(mockSession.prepareRequestURL("http://localhost/", null, "users/123/")).thenReturn("http://localhost/users/123/");
        Mockito.when(mockSession.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(), Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(), Mockito.eq(true), Mockito.eq(true))).thenReturn(specs);
        Mockito.when(mockSession.sendRequest(RestActions.RequestType.GET, "http://localhost/users/123/", specs)).thenReturn(response);
        Mockito.when(mockSession.evaluateResponseStatusCode(response, 0)).thenReturn(true);
        Mockito.when(mockSession.prepareReportMessage(response, 0, RestActions.RequestType.GET, "users/123/", ContentType.ANY, null)).thenReturn("ok");
        Mockito.when(mockSession.getDriver()).thenReturn(null);

        SHAFT.API driver = new RequestBuilder(mockSession, "users/123/", RestActions.RequestType.GET).performRequest();

        Assert.assertNotNull(driver);
        Assert.assertTrue(RequestBuilder.getPerformanceData().containsKey("users"));
        Assert.assertFalse(RequestBuilder.getPerformanceData().get("users").isEmpty());
        Mockito.verify(mockSession).setLastResponse(response);
        Mockito.verify(mockSession).setDriver(Mockito.any(SHAFT.API.class));
    }

    @Test
    public void performRequestShouldReturnExistingDriverWhenSessionDriverExists() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);
        Response response = Mockito.mock(Response.class);
        SHAFT.API existingDriver = Mockito.mock(SHAFT.API.class);

        Mockito.when(mockSession.prepareRequestURL("http://localhost/", null, "users")).thenReturn("http://localhost/users");
        Mockito.when(mockSession.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(), Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(), Mockito.eq(true), Mockito.eq(true))).thenReturn(specs);
        Mockito.when(mockSession.sendRequest(RestActions.RequestType.GET, "http://localhost/users", specs)).thenReturn(response);
        Mockito.when(mockSession.evaluateResponseStatusCode(response, 0)).thenReturn(true);
        Mockito.when(mockSession.prepareReportMessage(response, 0, RestActions.RequestType.GET, "users", ContentType.ANY, null)).thenReturn("ok");
        Mockito.when(mockSession.getDriver()).thenReturn(existingDriver);

        SHAFT.API returnedDriver = new RequestBuilder(mockSession, "users", RestActions.RequestType.GET).perform();

        Assert.assertSame(returnedDriver, existingDriver);
        Mockito.verify(mockSession, Mockito.never()).setDriver(Mockito.any());
    }

    @Test
    public void performRequestShouldApplyBasicAuthentication() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = prepareSuccessfulPerformRequestMocks(mockSession, "users", RestActions.RequestType.GET);

        new RequestBuilder(mockSession, "users", RestActions.RequestType.GET)
                .setAuthentication("basicUser", "basicPass", RequestBuilder.AuthenticationType.BASIC)
                .performRequest();

        Mockito.verify(specs.auth().preemptive()).basic("basicUser", "basicPass");
    }

    @Test
    public void performRequestShouldApplyFormAuthentication() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = prepareSuccessfulPerformRequestMocks(mockSession, "users", RestActions.RequestType.POST);

        new RequestBuilder(mockSession, "users", RestActions.RequestType.POST)
                .setAuthentication("formUser", "formPass", RequestBuilder.AuthenticationType.FORM)
                .performRequest();

        Mockito.verify(specs.auth()).form("formUser", "formPass");
    }

    @Test
    public void performRequestShouldThrowWhenRequestTypeIsNotSupported() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);

        Mockito.when(mockSession.prepareRequestURL("http://localhost/", null, "users")).thenReturn("http://localhost/users");
        Mockito.when(mockSession.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(), Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(), Mockito.eq(true), Mockito.eq(true))).thenReturn(specs);

        Assert.assertThrows(RuntimeException.class, () -> new RequestBuilder(mockSession, "users", null).performRequest());
    }

    @Test
    public void performRequestShouldThrowWhenStatusCodeValidationFails() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);
        Response response = Mockito.mock(Response.class);

        Mockito.when(mockSession.prepareRequestURL("http://localhost/", null, "users")).thenReturn("http://localhost/users");
        Mockito.when(mockSession.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(), Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(), Mockito.eq(true), Mockito.eq(true))).thenReturn(specs);
        Mockito.when(mockSession.sendRequest(RestActions.RequestType.GET, "http://localhost/users", specs)).thenReturn(response);
        Mockito.when(mockSession.evaluateResponseStatusCode(response, 0)).thenReturn(false);
        Mockito.when(mockSession.prepareReportMessage(response, 0, RestActions.RequestType.GET, "users", ContentType.ANY, null)).thenReturn("ok");
        Mockito.when(response.getStatusCode()).thenReturn(500);
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenReturn(25L);

        Assert.assertThrows(AssertionError.class, () -> new RequestBuilder(mockSession, "users", RestActions.RequestType.GET).performRequest());
    }

    @Test
    public void performRequestShouldThrowWhenReportMessageIsEmpty() {
        RestActions mockSession = createSessionMock();
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);
        Response response = Mockito.mock(Response.class);

        Mockito.when(mockSession.prepareRequestURL("http://localhost/", null, "users")).thenReturn("http://localhost/users");
        Mockito.when(mockSession.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(), Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(), Mockito.eq(true), Mockito.eq(true))).thenReturn(specs);
        Mockito.when(mockSession.sendRequest(RestActions.RequestType.GET, "http://localhost/users", specs)).thenReturn(response);
        Mockito.when(mockSession.evaluateResponseStatusCode(response, 0)).thenReturn(true);
        Mockito.when(mockSession.prepareReportMessage(response, 0, RestActions.RequestType.GET, "users", ContentType.ANY, null)).thenReturn("");
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenReturn(25L);

        Assert.assertThrows(RuntimeException.class, () -> new RequestBuilder(mockSession, "users", RestActions.RequestType.GET).performRequest());
    }

    private RestActions createSessionMock() {
        RestActions mockSession = Mockito.mock(RestActions.class);
        Mockito.when(mockSession.getServiceURI()).thenReturn("http://localhost/");
        Mockito.when(mockSession.getSessionCookies()).thenReturn(new LinkedHashMap<>());
        Mockito.when(mockSession.getSessionHeaders()).thenReturn(new LinkedHashMap<>());
        Mockito.when(mockSession.getSessionConfig()).thenReturn(RestAssuredConfig.config());
        return mockSession;
    }

    private RequestSpecification prepareSuccessfulPerformRequestMocks(RestActions mockSession, String serviceName,
                                                                      RestActions.RequestType requestType) {
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);
        Response response = Mockito.mock(Response.class);

        Mockito.when(mockSession.prepareRequestURL("http://localhost/", null, serviceName)).thenReturn("http://localhost/" + serviceName);
        Mockito.when(mockSession.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(), Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(), Mockito.eq(true), Mockito.eq(true))).thenReturn(specs);
        Mockito.when(mockSession.sendRequest(requestType, "http://localhost/" + serviceName, specs)).thenReturn(response);
        Mockito.when(mockSession.evaluateResponseStatusCode(response, 0)).thenReturn(true);
        Mockito.when(mockSession.prepareReportMessage(response, 0, requestType, serviceName, ContentType.ANY, null)).thenReturn("ok");
        Mockito.when(mockSession.getDriver()).thenReturn(Mockito.mock(SHAFT.API.class));
        return specs;
    }
}
