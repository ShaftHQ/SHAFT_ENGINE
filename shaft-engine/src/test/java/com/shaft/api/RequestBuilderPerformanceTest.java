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

import java.util.LinkedHashMap;

@Test(singleThreaded = true)
public class RequestBuilderPerformanceTest {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        RequestBuilder.getPerformanceData().clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void performShouldRecordEndpointTimingWhenBudgetIsConfiguredAndReportIsDisabled() {
        SHAFT.Properties.performance.set()
                .generatePerformanceReport(false)
                .apiEndpointPerformanceBudgets("users=1");
        RestActions session = createSessionMock();
        prepareSuccessfulPerformRequestMocks(session, "users/123/", RestActions.RequestType.GET);

        new RequestBuilder(session, "users/123/", RestActions.RequestType.GET).perform();

        Assert.assertTrue(RequestBuilder.getPerformanceData().containsKey("users"));
        Assert.assertFalse(RequestBuilder.getPerformanceData().get("users").isEmpty());
    }

    @Test
    public void performShouldSkipEndpointTimingWhenReportAndBudgetsAreDisabled() {
        SHAFT.Properties.performance.set().generatePerformanceReport(false);
        RestActions session = createSessionMock();
        prepareSuccessfulPerformRequestMocks(session, "users/123/", RestActions.RequestType.GET);

        new RequestBuilder(session, "users/123/", RestActions.RequestType.GET).perform();

        Assert.assertTrue(RequestBuilder.getPerformanceData().isEmpty());
    }

    private RestActions createSessionMock() {
        RestActions session = Mockito.mock(RestActions.class);
        Mockito.when(session.getServiceURI()).thenReturn("http://localhost/");
        Mockito.when(session.getSessionCookies()).thenReturn(new LinkedHashMap<>());
        Mockito.when(session.getSessionHeaders()).thenReturn(new LinkedHashMap<>());
        Mockito.when(session.getSessionConfig()).thenReturn(RestAssuredConfig.config());
        Mockito.when(session.getDriver()).thenReturn(Mockito.mock(SHAFT.API.class));
        return session;
    }

    private void prepareSuccessfulPerformRequestMocks(RestActions session, String serviceName,
                                                      RestActions.RequestType requestType) {
        RequestSpecification specs = Mockito.mock(RequestSpecification.class, Mockito.RETURNS_DEEP_STUBS);
        Response response = Mockito.mock(Response.class);

        Mockito.when(session.prepareRequestURL("http://localhost/", null, serviceName))
                .thenReturn("http://localhost/" + serviceName);
        Mockito.when(session.prepareRequestSpecs(Mockito.isNull(), Mockito.isNull(), Mockito.isNull(),
                        Mockito.eq(ContentType.ANY), Mockito.anyMap(), Mockito.anyMap(), Mockito.any(),
                        Mockito.eq(true), Mockito.eq(true)))
                .thenReturn(specs);
        Mockito.when(session.sendRequest(requestType, "http://localhost/" + serviceName, specs)).thenReturn(response);
        Mockito.when(session.evaluateResponseStatusCode(response, 0)).thenReturn(true);
        Mockito.when(session.prepareReportMessage(response, 0, requestType, serviceName, ContentType.ANY, null))
                .thenReturn("ok");
    }
}
