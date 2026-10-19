package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Map;

@Test(singleThreaded = true)
public class ReportSecretsTest {
    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        ReportSecrets.clear();
    }

    @Test
    public void secretFilesAreRecognisedByName() {
        Assert.assertTrue(ReportSecrets.isSecretFile("secrets.properties"));
        Assert.assertTrue(ReportSecrets.isSecretFile("API-Secret"));
        Assert.assertFalse(ReportSecrets.isSecretFile("custom.properties"));
    }

    @Test
    public void shortAndBooleanValuesAreNeverRegistered() {
        ReportSecrets.registerAll(Map.of("a", "abc", "b", "true", "c", "longEnoughValue"));
        Assert.assertEquals(ReportSecrets.redact("abc true longEnoughValue"), "abc true " + ReportSecrets.MASK);
        Assert.assertTrue(ReportSecrets.isSecretKey("a"));
    }

    @Test
    public void longerValuesAreMaskedBeforeTheirPrefixes() {
        ReportSecrets.registerValue("token1");
        ReportSecrets.registerValue("token12345");
        Assert.assertEquals(ReportSecrets.redact("x token12345 y"), "x " + ReportSecrets.MASK + " y");
    }

    @Test
    public void sensitiveKeysAreMaskedUnlessBoolean() {
        Assert.assertEquals(ReportSecrets.publishableValue("browserStack.accessKey", "abcdef"), ReportSecrets.MASK);
        Assert.assertEquals(ReportSecrets.publishableValue("captureStoreSecretsLocally", "false"), "false");
        Assert.assertEquals(ReportSecrets.publishableValue("targetBrowserName", "chrome"), "chrome");
    }

    @Test
    public void propertiesContentKeepsLayoutAndMasksCredentials() {
        String content = "# db\r\ndb.password = hunter22\r\nurl=https://example.test\r\n";
        Assert.assertEquals(ReportSecrets.redactProperties(content),
                "# db\r\ndb.password =" + ReportSecrets.MASK + "\r\nurl=https://example.test\r\n");
    }

    @Test
    public void jwtShapedTokensAreMaskedWithoutRegistration() {
        String jwt = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyZXBybyJ9.c2lnbmF0dXJl";
        Assert.assertEquals(ReportSecrets.redact("Bearer " + jwt), "Bearer " + ReportSecrets.MASK);
        Assert.assertEquals(ReportSecrets.redact("eyJustText"), "eyJustText");
    }
}
