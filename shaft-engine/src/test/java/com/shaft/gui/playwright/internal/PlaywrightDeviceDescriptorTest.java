package com.shaft.gui.playwright.internal;

import com.google.gson.JsonParser;
import com.microsoft.playwright.Browser;
import org.testng.Assert;
import org.testng.annotations.Test;

public class PlaywrightDeviceDescriptorTest {
    @Test
    public void registryDescriptorShouldApplyContextOptions() {
        var descriptors = JsonParser.parseString("""
                [{
                  "name": "Future Phone",
                  "descriptor": {
                    "userAgent": "future-agent",
                    "viewport": {"width": 401, "height": 802},
                    "screen": {"width": 402, "height": 803},
                    "deviceScaleFactor": 2.5,
                    "isMobile": true,
                    "hasTouch": true,
                    "defaultBrowserType": "webkit"
                  }
                }]
                """).getAsJsonArray();

        var descriptor = PlaywrightDeviceDescriptor.fromRegistry(descriptors, " future   phone ").orElseThrow();
        var options = new Browser.NewContextOptions();

        descriptor.applyTo(options);

        Assert.assertEquals(descriptor.defaultBrowserType(), "webkit");
        Assert.assertEquals(options.userAgent, "future-agent");
        Assert.assertEquals(options.viewportSize.get().width, 401);
        Assert.assertEquals(options.viewportSize.get().height, 802);
        Assert.assertEquals(options.screenSize.width, 402);
        Assert.assertEquals(options.screenSize.height, 803);
        Assert.assertEquals(options.deviceScaleFactor, 2.5);
        Assert.assertEquals(options.isMobile, Boolean.TRUE);
        Assert.assertEquals(options.hasTouch, Boolean.TRUE);
    }

    @Test
    public void latestDeviceAliasesShouldResolveWithoutRegistry() {
        var galaxy = PlaywrightDeviceDescriptor.resolve(null, "Galaxy S26 Ultra").orElseThrow();
        var iphone = PlaywrightDeviceDescriptor.resolve(null, "iPhone 17 Pro Max").orElseThrow();

        Assert.assertEquals(galaxy.defaultBrowserType(), "chromium");
        Assert.assertEquals(iphone.defaultBrowserType(), "webkit");
    }
}
