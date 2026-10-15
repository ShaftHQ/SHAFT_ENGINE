package com.shaft.driver.internal.DriverFactory;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/** #6972: any BrowserStack SDK option reaches browserstack.yml without per-feature SHAFT code. */
public class BrowserStackSdkPassThroughUnitTest {

    @Test
    @SuppressWarnings("unchecked")
    public void sdkPrefixedPropertiesAreTypedNestedAndOverrideGeneratedValues() {
        var properties = new Properties();
        properties.setProperty("browserStack.sdk.testObservability", "true");
        properties.setProperty("browserStack.sdk.accessibility", "false");
        properties.setProperty("browserStack.sdk.accessibilityOptions.wcagVersion", "wcag21aa");
        properties.setProperty("browserStack.sdk.percyOptions", "{\"enabled\":true}");
        properties.setProperty("browserStack.sdk.idleTimeout", "300");
        properties.setProperty("browserStack.sdk.debug", "true");
        properties.setProperty("browserStack.sdk.userName", "leak");
        properties.setProperty("browserStack.sdk.accessKey", "leak");
        properties.setProperty("browserStack.userName", "not-sdk");
        var config = new LinkedHashMap<String, Object>();
        config.put("debug", false);

        BrowserStackSdkHelper.addSdkPassThroughOptions(config, properties);

        Assert.assertEquals(config.get("testObservability"), Boolean.TRUE);
        Assert.assertEquals(config.get("accessibility"), Boolean.FALSE);
        Assert.assertEquals(((Map<String, Object>) config.get("accessibilityOptions")).get("wcagVersion"), "wcag21aa");
        Assert.assertEquals(((Map<String, Object>) config.get("percyOptions")).get("enabled"), Boolean.TRUE);
        Assert.assertEquals(config.get("idleTimeout"), 300);
        Assert.assertEquals(config.get("debug"), Boolean.TRUE);
        Assert.assertFalse(config.containsKey("userName"));
        Assert.assertFalse(config.containsKey("accessKey"));
        Assert.assertFalse(config.toString().contains("leak"));
        Assert.assertFalse(config.toString().contains("not-sdk"));
    }

    @Test
    public void jsonArrayAndInvalidJsonKeepUsableValues() {
        var properties = new Properties();
        properties.setProperty("browserStack.sdk.customTags", "[\"smoke\",\"nightly\"]");
        properties.setProperty("browserStack.sdk.note", "{not json");
        var config = new LinkedHashMap<String, Object>();

        BrowserStackSdkHelper.addSdkPassThroughOptions(config, properties);

        Assert.assertEquals(config.get("customTags"), List.of("smoke", "nightly"));
        Assert.assertEquals(config.get("note"), "{not json");
    }
}
