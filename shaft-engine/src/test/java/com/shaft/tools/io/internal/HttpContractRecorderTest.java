package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import io.restassured.http.Cookie;
import io.restassured.http.Cookies;
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

@Test(singleThreaded = true)
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

    @Test
    public void inactiveBlankAndUnreadableContractsStayLocal() throws Exception {
        Assert.assertFalse(HttpContractRecorder.isBrowserContractModeActive());
        HttpContractRecorder.stopRecording();
        HttpContractRecorder.stopValidation();
        HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/idle", ""),
                response(200, "{\"ok\":true}"));
        Assert.expectThrows(IllegalArgumentException.class, () -> SHAFT.Contracts.startRecording(" "));
        Assert.expectThrows(IllegalArgumentException.class, () -> SHAFT.Contracts.startRecording(null));

        contractFile = Files.createTempFile("shaft-contract", ".json");
        Files.writeString(contractFile, "not-json");
        String unreadable = contractFile.toString();
        IllegalStateException readError = Assert.expectThrows(IllegalStateException.class,
                () -> SHAFT.Contracts.startAssertMode(unreadable));
        Assert.assertTrue(readError.getMessage().contains("Could not read HTTP contract"), readError.getMessage());

        Files.writeString(contractFile, "{\"schemaVersion\":\"\",\"interactions\":null}");
        SHAFT.Contracts.startAssertMode(contractFile.toString(), (String[]) null);
        Assert.assertTrue(HttpContractRecorder.isBrowserContractModeActive());
        AssertionError missing = Assert.expectThrows(AssertionError.class, () -> HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/missing", "plain text"),
                response(200, "plain text")));
        Assert.assertTrue(missing.getMessage().contains("No matching contract interaction"), missing.getMessage());
        SHAFT.Contracts.stopValidation();
        Assert.assertFalse(HttpContractRecorder.isBrowserContractModeActive());
    }

    @Test
    public void recordingKeepsByteBodiesCookiesArraysAndClosedResponses() throws Exception {
        contractFile = Files.createTempFile("shaft-contract", ".json");
        SHAFT.Contracts.startRecording(contractFile.toString(), " ", "/api/edge");
        HttpContractRecorder.handleApiExchange(byteRequest(), null);
        HttpContractRecorder.handleApiExchange(
                request("POST", "https://shop.test/api/edge?flag&q=%ZZ", "not-json"),
                arrayResponse());
        HttpContractRecorder.handleApiExchange(
                request("GET", "https://shop.test/api/edge", ""),
                throwingResponse());
        HttpContractRecorder.handleApiExchange(
                request("GET", "https://other.test/api/ignored", ""),
                response(200, "{\"ok\":true}"));
        SHAFT.Contracts.stopRecording();

        String contract = Files.readString(contractFile, StandardCharsets.UTF_8);
        Assert.assertTrue(contract.contains("\\\"password\\\" : \\\"********\\\""), contract);
        Assert.assertTrue(contract.contains("\\\"note\\\" : null"), contract);
        Assert.assertTrue(contract.contains("<normalized>"), contract);
        Assert.assertTrue(contract.contains("Cookie:sid"), contract);
        Assert.assertFalse(contract.contains("request-secret"), contract);
        Assert.assertFalse(contract.contains("/api/ignored"), contract);

        SHAFT.Contracts.startAssertMode(contractFile.toString());
        HttpContractRecorder.handleApiExchange(byteRequest(), null);
        AssertionError headerDiff = Assert.expectThrows(AssertionError.class, () -> HttpContractRecorder.handleApiExchange(
                request("POST", "https://shop.test/api/edge?flag&q=%ZZ", "not-json"),
                arrayResponse("beta")));
        Assert.assertTrue(headerDiff.getMessage().contains("Header X-Trace expected `alpha` but was `beta`"),
                headerDiff.getMessage());
        SHAFT.Contracts.stopValidation();

        Path blocker = Files.createTempFile("shaft-contract-blocker", ".txt");
        Path blockedContract = blocker.resolve("contract.json");
        try {
            SHAFT.Contracts.startRecording(blockedContract.toString());
            HttpContractRecorder.handleApiExchange(
                    request("GET", "https://shop.test/api/edge", ""),
                    response(204, ""));
            IllegalStateException writeError = Assert.expectThrows(IllegalStateException.class,
                    SHAFT.Contracts::stopRecording);
            Assert.assertTrue(writeError.getMessage().contains("Could not write HTTP contract"),
                    writeError.getMessage());
        } finally {
            HttpContractRecorder.clear();
            Files.deleteIfExists(blocker);
        }
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

    private FilterableRequestSpecification byteRequest() {
        FilterableRequestSpecification request = request(
                "put", "https://shop.test/api/edge?flag", "{\"password\":\"request-secret\"}");
        Mockito.when(request.getBody()).thenReturn("{\"password\":\"request-secret\"}".getBytes(StandardCharsets.UTF_8));
        Mockito.when(request.getCookies()).thenReturn(new Cookies(new Cookie.Builder("sid", "abc").build()));
        return request;
    }

    private Response arrayResponse() {
        return arrayResponse("alpha");
    }

    private Response arrayResponse(String trace) {
        return response(200,
                "[{\"password\":\"request-secret\",\"note\":null,\"id\":\"550e8400-e29b-41d4-a716-446655440000\"}]",
                new Header("X-Trace", trace));
    }

    private Response throwingResponse() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.getStatusCode()).thenReturn(200);
        Mockito.when(response.getHeaders()).thenReturn(new Headers());
        Mockito.when(response.getBody()).thenThrow(new IllegalStateException("closed"));
        return response;
    }

    private Response response(int statusCode, String bodyText) {
        return response(statusCode, bodyText, new Header("X-Request-Id", "response-123"));
    }

    private Response response(int statusCode, String bodyText, Header extra) {
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> body = Mockito.mock(ResponseBody.class);
        Mockito.when(response.getStatusCode()).thenReturn(statusCode);
        Mockito.when(response.getHeaders()).thenReturn(new Headers(
                new Header("Content-Type", "application/json"),
                new Header("Date", "Fri, 26 Jun 2026 10:15:30 GMT"),
                extra));
        Mockito.when(response.getBody()).thenReturn(body);
        Mockito.when(body.asString()).thenReturn(bodyText);
        return response;
    }
}
