package com.shaft.driver.internal;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.nio.charset.StandardCharsets;

/**
 * Native BrowserStack app binaries must match {@code targetOperatingSystem} (#7899). No network.
 */
public class BrowserStackAppPlatformTest {

    @Test
    public void apkMatchesAndroid() {
        BrowserStackAppPlatform.requireMatch("/apps/demo.apk", "ANDROID", null);
        BrowserStackAppPlatform.requireMatch("/apps/demo.aab", "Android", null);
        BrowserStackAppPlatform.requireMatch("/apps/demo.ipa", "IOS", null);
    }

    @Test
    public void apkDoesNotMatchIos() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> BrowserStackAppPlatform.requireMatch("/apps/demo.apk", "IOS", null));
        Assert.assertTrue(failure.getMessage().contains("ANDROID"), failure.getMessage());
        Assert.assertTrue(failure.getMessage().contains("IOS"), failure.getMessage());
        Assert.assertFalse(failure.getMessage().contains("demo.apk"));
    }

    @Test
    public void browserStackIdUsesTheStubbedAppNameAndDoesNotPrintTheId() {
        String id = "bs://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        BrowserStackAppPlatform.requireMatch(id, "IOS", "HRSD-release.ipa");

        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> BrowserStackAppPlatform.requireMatch(id, "ANDROID", "HRSD-release.ipa"));
        Assert.assertTrue(failure.getMessage().contains("IOS"), failure.getMessage());
        Assert.assertTrue(failure.getMessage().contains("ANDROID"), failure.getMessage());
        Assert.assertFalse(failure.getMessage().contains(id.substring("bs://".length())), failure.getMessage());
        Assert.assertFalse(failure.getMessage().toLowerCase().contains("accesskey"), failure.getMessage());
    }

    @Test
    public void nestedSuffixUsesTheFileNameNotAnEarlierSegment() {
        Assert.assertEquals(BrowserStackAppPlatform.platformFromName("build.ipa/app.apk"), "ANDROID");
        Assert.assertEquals(BrowserStackAppPlatform.platformFromName("app.aab?x=.ipa"), "ANDROID");
        Assert.assertEquals(BrowserStackAppPlatform.platformFromName("HRSD-release.ipa"), "IOS");
    }

    @Test
    public void missingCredentialFailureDoesNotEchoTheSecret() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> BrowserStackAppPlatform.lookupAppName(
                        "bs://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "user-name", ""));
        Assert.assertFalse(failure.getMessage().contains("user-name"), failure.getMessage());
        Assert.assertFalse(failure.getMessage().contains("bs://"), failure.getMessage());
        Assert.assertFalse(failure.getMessage().toLowerCase().contains("accesskey"), failure.getMessage());
    }

    @Test
    public void recentAppsJsonSuppliesTheStubbedNameWithoutANetworkCall() {
        String id = "bs://bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
        String body = """
                [{"app_name":"suite.apk","app_url":"%s","app_id":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}]
                """.formatted(id);
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(body, id), "suite.apk");
        BrowserStackAppPlatform.requireMatch(id, "ANDROID",
                BrowserStackAppPlatform.appNameFromRecentApps(body, id));
    }

    @Test
    public void recentAppsHttpUsesTheStatusAndDoesNotEchoSecrets() throws IOException {
        String username = "coverage-user";
        String accessKey = "coverage-access-key";
        String body = """
                [{"app_name":"suite.apk","app_url":"bs://eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"}]
                """;
        HttpServer ok = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        ok.createContext("/", exchange -> respond(exchange, 200, body));
        ok.start();
        try {
            String read = BrowserStackAppPlatform.readRecentApps(local(ok), username, accessKey);
            Assert.assertEquals(read, body);
            Assert.assertFalse(read.contains(accessKey), read);
        } finally {
            ok.stop(0);
        }

        HttpServer failed = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        failed.createContext("/", exchange -> respond(exchange, 500, "unavailable"));
        failed.start();
        try {
            IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                    () -> BrowserStackAppPlatform.readRecentApps(local(failed), username, accessKey));
            Assert.assertTrue(failure.getMessage().contains("500"), failure.getMessage());
            assertNoSecrets(failure.getMessage(), username, accessKey);
        } finally {
            failed.stop(0);
        }

        int closedPort;
        try (ServerSocket socket = new ServerSocket()) {
            socket.bind(new InetSocketAddress("127.0.0.1", 0));
            closedPort = socket.getLocalPort();
        }
        IllegalStateException refused = Assert.expectThrows(IllegalStateException.class,
                () -> BrowserStackAppPlatform.readRecentApps(
                        URI.create("http://127.0.0.1:" + closedPort + "/"), username, accessKey));
        assertNoSecrets(refused.getMessage(), username, accessKey);
    }

    @Test
    public void appNameFromRecentAppsCoversBlankInvalidObjectFallbackAndUnknownId() {
        String accessKey = "coverage-access-key";
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(null, "bs://id"), "");
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(" ", "bs://id"), "");
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps("[]", null), "");
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps("[]", " "), "");

        IllegalArgumentException invalid = Assert.expectThrows(IllegalArgumentException.class,
                () -> BrowserStackAppPlatform.appNameFromRecentApps("{" + accessKey, "bs://id"));
        Assert.assertFalse(invalid.getMessage().contains(accessKey), invalid.getMessage());

        String id = "bs://cccccccccccccccccccccccccccccccccccccccc";
        String objectBody = """
                {"apps":[{"app_name":"from-object.apk","app_url":"%s"}]}
                """.formatted(id);
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(objectBody, id), "from-object.apk");

        String fallback = """
                [{"app_name":"","app_url":"%s","filename":"fallback.apk"}]
                """.formatted(id);
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(fallback, id), "fallback.apk");
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(objectBody, "bs://unknown"), "");
    }

    @Test
    public void requireMatchIgnoresNonMobileAndUnclassifiedPathsAndFailsClosedOnBlankName() {
        BrowserStackAppPlatform.requireMatch("/apps/demo.apk", "WINDOWS", null);
        BrowserStackAppPlatform.requireMatch("/tmp/readme.txt", "ANDROID", null);
        String id = "bs://dddddddddddddddddddddddddddddddddddddddd";
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> BrowserStackAppPlatform.requireMatch(id, "IOS", " "));
        Assert.assertFalse(failure.getMessage().contains(id.substring("bs://".length())), failure.getMessage());
    }

    @Test
    public void isMobileTargetAndPlatformFromNameCoverUnclassifiedNames() {
        Assert.assertTrue(BrowserStackAppPlatform.isMobileTarget("ANDROID"));
        Assert.assertFalse(BrowserStackAppPlatform.isMobileTarget("WINDOWS"));
        Assert.assertEquals(BrowserStackAppPlatform.platformFromName(null), "");
        Assert.assertEquals(BrowserStackAppPlatform.platformFromName("readme.txt"), "");
    }

    private static URI local(HttpServer server) {
        return URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/");
    }

    private static void respond(HttpExchange exchange, int status, String body) throws IOException {
        byte[] payload = body.getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(status, payload.length);
        exchange.getResponseBody().write(payload);
        exchange.close();
    }

    private static void assertNoSecrets(String message, String username, String accessKey) {
        Assert.assertFalse(message.contains(username), message);
        Assert.assertFalse(message.contains(accessKey), message);
    }
}
