package com.shaft.properties.internal;

import com.shaft.driver.SHAFT;
import org.aeonbits.owner.ConfigFactory;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

/**
 * Raw string values for the SHAFT API flag family (#6326 sweep): case handling, surrounding
 * whitespace, invalid, empty and non-numeric values, and source precedence. Values are injected
 * the way a properties file or {@code -D} option would supply them, as strings.
 */
@Test(singleThreaded = true)
public class ApiPropertyRawValueUnitTest {
    private static final String FLAG = "automaticallyAssertResponseStatusCode";

    @AfterMethod(alwaysRun = true)
    public void restore() {
        synchronized (Properties.class) {
            ThreadLocalPropertiesManager.setGlobalProperty(FLAG, "true");
            Properties.baseFlags = ConfigFactory.create(Flags.class, ThreadLocalPropertiesManager.getGlobalOverrides());
        }
        System.clearProperty(FLAG);
        Properties.clearForCurrentThread();
    }

    private static void rawFlag(String value) {
        synchronized (Properties.class) {
            ThreadLocalPropertiesManager.setGlobalProperty(FLAG, value);
            Properties.baseFlags = ConfigFactory.create(Flags.class, ThreadLocalPropertiesManager.getGlobalOverrides());
        }
    }

    private static void rawTimeout(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.timeoutsOverride.set(ConfigFactory.create(Timeouts.class, ThreadLocalPropertiesManager.getOverrides()));
    }

    private static void rawApi(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.apiOverride.set(ConfigFactory.create(API.class, ThreadLocalPropertiesManager.getOverrides()));
    }

    private static boolean effectiveAutoAssert() {
        return com.shaft.api.internal.ApiSettings.automaticallyAssertResponseStatusCode();
    }

    @DataProvider
    public Object[][] acceptedBooleans() {
        return new Object[][]{
                {"true", true}, {"TRUE", true}, {"True", true}, {" true ", true}, {"true\t", true},
                {"false", false}, {"FALSE", false}, {"False", false}, {" false", false}};
    }

    @Test(dataProvider = "acceptedBooleans", description = "true/false are accepted in any case and with surrounding whitespace")
    public void acceptedBooleanSpellings(String raw, boolean expected) {
        rawFlag(raw);
        Assert.assertEquals(effectiveAutoAssert(), expected);
    }

    @DataProvider
    public Object[][] rejectedBooleans() {
        return new Object[][]{{"yes"}, {"no"}, {"on"}, {"1"}, {"0"}, {"ture"}, {""}, {"   "}};
    }

    @Test(dataProvider = "rejectedBooleans", description = "anything else fails with the key, the value and a fix-next line")
    public void rejectedBooleanSpellingsFailClearly(String raw) {
        rawFlag(raw);
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.API("http://127.0.0.1:9/").get("x").perform());
        Assert.assertTrue(error.getMessage().contains(FLAG), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("'" + raw + "'"), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix: set " + FLAG + "=true or " + FLAG + "=false"), error.getMessage());
    }

    @Test(description = "precedence: SHAFT.Properties.flags.set() beats a -D system property")
    public void programmaticSetBeatsSystemProperty() {
        System.setProperty(FLAG, "false");
        SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(true);
        Assert.assertTrue(effectiveAutoAssert());
        SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(false);
        Assert.assertFalse(effectiveAutoAssert());
    }

    @Test(description = "precedence: a -D system property beats the default when nothing overrides it")
    public void systemPropertyBeatsDefault() {
        System.setProperty(FLAG, "false");
        Assert.assertFalse(ConfigFactory.create(Flags.class).automaticallyAssertResponseStatusCode());
        System.clearProperty(FLAG);
        Assert.assertTrue(ConfigFactory.create(Flags.class).automaticallyAssertResponseStatusCode());
    }

    @DataProvider
    public Object[][] rejectedTimeouts() {
        return new Object[][]{
                {"apiSocketTimeout", "abc"}, {"apiConnectionTimeout", ""}, {"apiConnectionManagerTimeout", "1.5"},
                {"apiSocketTimeout", "   "}, {"apiConnectionTimeout", "-1"}, {"apiConnectionManagerTimeout", "99999999999"}};
    }

    @Test(dataProvider = "rejectedTimeouts", description = "non-numeric, empty, negative or overflowing timeouts fail clearly")
    public void rejectedTimeoutsFailClearly(String key, String raw) {
        rawTimeout(key, raw);
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.API("http://127.0.0.1:9/").get("x").perform());
        Assert.assertTrue(error.getMessage().contains(key), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("'" + raw + "'") || error.getMessage().contains(raw.trim()), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }

    @Test(description = "timeouts with surrounding whitespace are accepted")
    public void timeoutWithWhitespaceIsAccepted() {
        rawTimeout("apiSocketTimeout", " 12 ");
        Assert.assertEquals(com.shaft.api.internal.ApiSettings.apiSocketTimeoutSeconds(), 12);
    }

    @DataProvider
    public Object[][] rejectedSwaggerFlags() {
        return new Object[][]{{"yes"}, {""}, {"enabled"}};
    }

    @Test(dataProvider = "rejectedSwaggerFlags", description = "an invalid swagger.validation.enabled value fails clearly")
    public void rejectedSwaggerFlagFailsClearly(String raw) {
        rawApi("swagger.validation.enabled", raw);
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.API("http://127.0.0.1:9/").get("x").perform());
        Assert.assertTrue(error.getMessage().contains("swagger.validation.enabled"), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }

    @Test(description = "swagger.validation.enabled accepts upper case")
    public void swaggerFlagUpperCaseAccepted() {
        rawApi("swagger.validation.enabled", "FALSE");
        Assert.assertFalse(com.shaft.api.internal.ApiSettings.swaggerValidationEnabled());
    }

    @Test(description = "an invalid openapi.coverage.report.enabled value fails clearly")
    public void rejectedCoverageFlagFailsClearly() {
        rawApi("openapi.coverage.report.enabled", "maybe");
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.API("http://127.0.0.1:9/").get("x").perform());
        Assert.assertTrue(error.getMessage().contains("openapi.coverage.report.enabled"), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }

    @Test(description = "a non-numeric openapi.coverage.threshold fails clearly when coverage is on")
    public void rejectedCoverageThresholdFailsClearly() {
        rawApi("openapi.coverage.report.enabled", "true");
        rawApi("swagger.validation.url", "https://example.invalid/openapi.json");
        rawApi("openapi.coverage.threshold", "eighty");
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.API("http://127.0.0.1:9/").get("x").perform());
        Assert.assertTrue(error.getMessage().contains("openapi.coverage.threshold"), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }
}
