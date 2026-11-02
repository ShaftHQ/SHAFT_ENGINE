package com.shaft.gui.browser.internal;

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

public class HarReplayRulesTest {
    private static final String HAR_JSON = """
            {
              "log": {
                "version": "1.2",
                "creator": {"name": "test", "version": "1.0"},
                "entries": [
                  {
                    "startedDateTime": "2026-01-01T00:00:00.000Z",
                    "request": {"method": "GET", "url": "https://shop.test/api/items?id=123", "headers": []},
                    "response": {
                      "status": 200,
                      "statusText": "OK",
                      "headers": [
                        {"name": "Content-Type", "value": "application/json"},
                        {"name": "Content-Length", "value": "999"}
                      ],
                      "content": {"size": 11, "mimeType": "application/json", "text": "{\\"ok\\":true}"}
                    }
                  },
                  {
                    "request": {"method": "GET", "url": "https://shop.test/api/items?id=123"},
                    "response": {
                      "status": 200,
                      "headers": [{"name": "Content-Type", "value": "application/octet-stream"}],
                      "content": {"mimeType": "application/octet-stream", "text": "aGVsbG8=", "encoding": "base64"}
                    }
                  },
                  {
                    "request": {"method": "POST", "url": "https://shop.test/api/pending"}
                  }
                ]
              }
            }
            """;

    private Path harFile;

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        if (harFile != null) {
            Files.deleteIfExists(harFile);
        }
    }

    @Test
    public void buildRulesShouldReplayInOrderAndSkipEntriesWithoutAResponse() throws Exception {
        harFile = Files.createTempFile("shaft-har-replay", ".har");
        Files.writeString(harFile, HAR_JSON);

        List<BrowserNetworkInterceptionRule> rules = HarReplayRules.buildRules(harFile.toString());
        Assert.assertEquals(rules.size(), 1);
        BrowserNetworkInterceptionRule rule = rules.getFirst();

        HttpRequest matchingRequest = request(HttpMethod.GET, "https://shop.test/api/items?id=123");
        Assert.assertTrue(rule.matches(matchingRequest));

        HttpResponse firstReplay = rule.createResponse(matchingRequest);
        Assert.assertEquals(firstReplay.getStatus(), 200);
        Assert.assertEquals(firstReplay.getHeader("Content-Type"), "application/json");
        Assert.assertNull(firstReplay.getHeader("Content-Length"));
        Assert.assertEquals(bodyOf(firstReplay), "{\"ok\":true}");

        HttpResponse secondReplay = rule.createResponse(matchingRequest);
        Assert.assertEquals(secondReplay.getHeader("Content-Type"), "application/octet-stream");
        Assert.assertEquals(bodyOf(secondReplay), "hello");
    }

    @Test
    public void buildRulesShouldMatchExactUrlOnlyAndIgnoreSkippedEntries() throws Exception {
        harFile = Files.createTempFile("shaft-har-replay", ".har");
        Files.writeString(harFile, HAR_JSON);

        BrowserNetworkInterceptionRule rule = HarReplayRules.buildRules(harFile.toString()).getFirst();

        Assert.assertFalse(rule.matches(request(HttpMethod.GET, "https://shop.test/api/items?id=999")));
        Assert.assertFalse(rule.matches(request(HttpMethod.POST, "https://shop.test/api/items?id=123")));
        Assert.assertFalse(rule.matches(request(HttpMethod.POST, "https://shop.test/api/pending")));
    }

    private HttpRequest request(HttpMethod method, String url) {
        return new HttpRequest(method, url);
    }

    private String bodyOf(HttpResponse response) {
        return new String(Contents.bytes(response.getContent()), StandardCharsets.UTF_8);
    }
}
