package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import io.restassured.specification.FilterableRequestSpecification;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

public class HttpContractRecorderTest {
    private Path contractFile;

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        HttpContractRecorder.clear();
        Properties.clearForCurrentThread();
        if (contractFile != null) {
            Files.deleteIfExists(contractFile);
        }
    }

    @Test
    public void apiRecordingShouldWriteRedactedDeterministicContract() throws Exception {
        contractFile = Files.createTempFile("shaft-contract", ".json");

        SHAFT.Contracts.startRecording(contractFile.toString(), "/checkout");
        HttpContractRecorder.handleApiExchange(
                request("POST", "https://shop.test/api/checkout?token=request-token",
                        "{\"password\":\"request-secret\",\"requestId\":\"abc\",\"item\":\"book\"}"),
                response(201, "{\"token\":\"response-token\",\"createdAt\":\"2026-06-26T10:15:30Z\",\"ok\":true}"));
        SHAFT.Contracts.stopRecording();

        String contract = Files.readString(contractFile, StandardCharsets.UTF_8);
        Assert.assertTrue(contract.contains("\"schemaVersion\""));
        Assert.assertTrue(contract.contains("\"method\" : \"POST\""));
        Assert.assertTrue(contract.contains("\"path\" : \"/api/checkout\""));
        Assert.assertTrue(contract.contains("\"token\" : \"********\""));
        Assert.assertTrue(contract.contains("\\\"password\\\" : \\\"********\\\""));
        Assert.assertTrue(contract.contains("\\\"requestId\\\" : \\\"<normalized>\\\""));
        Assert.assertTrue(contract.contains("\\\"createdAt\\\" : \\\"<normalized>\\\""));
        Assert.assertFalse(contract.contains("request-token"));
        Assert.assertFalse(contract.contains("request-secret"));
        Assert.assertFalse(contract.contains("response-token"));
    }

    @Test
    public void assertModeShouldThrowReadableDiffOnMismatch() throws Exception {
        contractFile = Files.createTempFile("shaft-contract", ".json");
        SHAFT.Contracts.startRecording(contractFile.toString());
        HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/checkout", ""),
                response(200, "{\"ok\":true}"));
        SHAFT.Contracts.stopRecording();

        SHAFT.Contracts.startAssertMode(contractFile.toString());
        AssertionError error = Assert.expectThrows(AssertionError.class, () -> HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/checkout", ""),
                response(500, "{\"ok\":false}")));

        Assert.assertTrue(error.getMessage().contains("HTTP contract mismatch"));
        Assert.assertTrue(error.getMessage().contains("Status expected 200 but was 500"));
        Assert.assertTrue(error.getMessage().contains("Trace action:"));
    }

    @Test
    public void verifyModeShouldAttachDiffWithoutThrowing() throws Exception {
        contractFile = Files.createTempFile("shaft-contract", ".json");
        SHAFT.Contracts.startRecording(contractFile.toString());
        HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/profile", ""),
                response(200, "{\"name\":\"Shaft\"}"));
        SHAFT.Contracts.stopRecording();

        SHAFT.Contracts.startVerifyMode(contractFile.toString());
        HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/profile", ""),
                response(200, "{\"name\":\"Changed\"}"));
    }

    private FilterableRequestSpecification request(String method, String uri, String body) {
        FilterableRequestSpecification request = Mockito.mock(FilterableRequestSpecification.class);
        Mockito.when(request.getMethod()).thenReturn(method);
        Mockito.when(request.getURI()).thenReturn(uri);
        Mockito.when(request.getBody()).thenReturn(body);
        Mockito.when(request.getHeaders()).thenReturn(new Headers(
                new Header("Authorization", "Bearer secret"),
                new Header("X-Request-Id", "request-123")));
        Mockito.when(request.getCookies()).thenReturn(new io.restassured.http.Cookies());
        return request;
    }

    private Response response(int statusCode, String bodyText) {
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> body = Mockito.mock(ResponseBody.class);
        Mockito.when(response.getStatusCode()).thenReturn(statusCode);
        Mockito.when(response.getHeaders()).thenReturn(new Headers(
                new Header("Content-Type", "application/json"),
                new Header("Date", "Fri, 26 Jun 2026 10:15:30 GMT")));
        Mockito.when(response.getBody()).thenReturn(body);
        Mockito.when(body.asString()).thenReturn(bodyText);
        return response;
    }
}
