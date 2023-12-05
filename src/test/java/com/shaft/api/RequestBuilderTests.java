package com.shaft.api;

import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

public class RequestBuilderTests {
    @Test
    public void testInitializeVariables() {
        // Arrange
        RestActions mockSession = Mockito.mock(RestActions.class);
        String serviceName = "testService";
        RestActions.RequestType requestType = RestActions.RequestType.GET;

        // Act
        RequestBuilder requestBuilder = new RequestBuilder(mockSession, serviceName, requestType);

        // Assert
        Assert.assertEquals(requestBuilder.getServiceName(), serviceName);
        Assert.assertEquals(requestBuilder.getRequestType(), requestType);
        Mockito.verify(mockSession, Mockito.times(1)).getServiceURI();
        Mockito.verify(mockSession, Mockito.times(1)).getSessionCookies();
        Mockito.verify(mockSession, Mockito.times(1)).getSessionHeaders();
        Mockito.verify(mockSession, Mockito.times(1)).getSessionConfig();
    }

    // Add more test methods here for other methods in the RequestBuilder class
}
