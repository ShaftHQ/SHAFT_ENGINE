package com.shaft.driver.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

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
    public void recentAppsJsonSuppliesTheStubbedNameWithoutANetworkCall() {
        String id = "bs://bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
        String body = """
                [{"app_name":"suite.apk","app_url":"%s","app_id":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}]
                """.formatted(id);
        Assert.assertEquals(BrowserStackAppPlatform.appNameFromRecentApps(body, id), "suite.apk");
        BrowserStackAppPlatform.requireMatch(id, "ANDROID",
                BrowserStackAppPlatform.appNameFromRecentApps(body, id));
    }
}
