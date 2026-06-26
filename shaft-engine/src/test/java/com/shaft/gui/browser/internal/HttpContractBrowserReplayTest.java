package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.HttpContractRecorder;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class HttpContractBrowserReplayTest {
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
    public void browserReplayRuleShouldMatchMethodPathQueryAndNormalizedBody() throws Exception {
        contractFile = Files.createTempFile("shaft-browser-contract", ".json");
        SHAFT.Contracts.startRecording(contractFile.toString(), "/api/items");
        HttpContractRecorder.handleBrowserExchange(
                request(HttpMethod.POST, "https://shop.test/api/items?id=123",
                        "{\"requestId\":\"first\",\"sku\":\"A-1\"}"),
                response(202, "{\"createdAt\":\"2026-06-26T10:15:30Z\",\"ok\":true,\"version\":1}"),
                "");
        HttpContractRecorder.handleBrowserExchange(
                request(HttpMethod.POST, "https://shop.test/api/items?id=123",
                        "{\"requestId\":\"first\",\"sku\":\"A-1\"}"),
                response(203, "{\"createdAt\":\"2026-06-26T10:15:31Z\",\"ok\":true,\"version\":2}"),
                "");
        SHAFT.Contracts.stopRecording();

        List<BrowserNetworkInterceptionRule> rules = HttpContractRecorder.browserReplayRules(contractFile.toString());
        BrowserNetworkInterceptionRule rule = rules.getFirst();
        HttpRequest matchingRequest = request(HttpMethod.POST, "https://offline.test/api/items?id=123",
                "{\"requestId\":\"second\",\"sku\":\"A-1\"}");
        HttpRequest wrongQuery = request(HttpMethod.POST, "https://offline.test/api/items?id=999",
                "{\"requestId\":\"second\",\"sku\":\"A-1\"}");

        Assert.assertTrue(rule.matches(matchingRequest));
        Assert.assertFalse(rule.matches(wrongQuery));

        HttpResponse firstReplay = rule.createResponse(matchingRequest);
        String firstBody = new String(Contents.bytes(firstReplay.getContent()), StandardCharsets.UTF_8);
        Assert.assertEquals(firstReplay.getStatus(), 202);
        Assert.assertEquals(firstReplay.getHeader("Content-Type"), "application/json");
        Assert.assertTrue(firstBody.contains("\"createdAt\" : \"<normalized>\""));
        Assert.assertTrue(firstBody.contains("\"version\" : 1"));

        HttpResponse secondReplay = rule.createResponse(matchingRequest);
        String secondBody = new String(Contents.bytes(secondReplay.getContent()), StandardCharsets.UTF_8);
        Assert.assertEquals(secondReplay.getStatus(), 203);
        Assert.assertTrue(secondBody.contains("\"version\" : 2"));
    }

    private HttpRequest request(HttpMethod method, String url, String body) {
        HttpRequest request = new HttpRequest(method, url);
        request.addHeader("Authorization", "Bearer secret");
        request.setContent(Contents.bytes(body.getBytes(StandardCharsets.UTF_8)));
        return request;
    }

    private HttpResponse response(int statusCode, String body) {
        HttpResponse response = new HttpResponse().setStatus(statusCode);
        response.addHeader("Content-Type", "application/json");
        response.addHeader("Set-Cookie", "sid=secret");
        response.setContent(Contents.bytes(body.getBytes(StandardCharsets.UTF_8)));
        return response;
    }
}
