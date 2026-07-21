package com.shaft.properties.internal;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class PropertyFileManagerInternalUnitTest {
    @Test(description = "Log4j config path normalization should preserve jar resource URLs")
    public void normalizeLog4jConfigPathPreservesJarResourceUrl() {
        String jarResourcePath = "jar:file:/C:/test/shaft-engine.jar!/resources/properties/default/log4j2.properties";

        Assert.assertEquals(PropertyFileManager.normalizeLog4jConfigPath(jarResourcePath), jarResourcePath);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        ThreadLocalPropertiesManager.clear();
    }

    @Test(description = "A Windows-style absolute mobile_app path must be left unchanged regardless of the host OS running the JVM")
    public void getAppiumDesiredCapabilitiesLeavesWindowsAbsolutePathUnchanged() {
        ThreadLocalPropertiesManager.setProperty("mobile_app", "C:\\Windows\\System32\\notepad.exe");

        var capabilities = PropertyFileManager.getAppiumDesiredCapabilities();

        Assert.assertEquals(capabilities.get("mobile_app"), "C:\\Windows\\System32\\notepad.exe");
    }

    @Test(description = "A Unix-style absolute mobile_app path must be left unchanged regardless of the host OS running the JVM")
    public void getAppiumDesiredCapabilitiesLeavesUnixAbsolutePathUnchanged() {
        ThreadLocalPropertiesManager.setProperty("mobile_app", "/opt/apps/foo.apk");

        var capabilities = PropertyFileManager.getAppiumDesiredCapabilities();

        Assert.assertEquals(capabilities.get("mobile_app"), "/opt/apps/foo.apk");
    }

    @Test(description = "A genuinely relative mobile_app path is still resolved against user.dir")
    public void getAppiumDesiredCapabilitiesResolvesRelativePathAgainstUserDir() {
        ThreadLocalPropertiesManager.setProperty("mobile_app", "apps/foo.apk");

        var capabilities = PropertyFileManager.getAppiumDesiredCapabilities();

        String expected = new java.io.File(System.getProperty("user.dir"), "apps/foo.apk").getAbsolutePath();
        Assert.assertEquals(capabilities.get("mobile_app"), expected);
    }
}
