package com.shaft.properties.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

public class PropertyFileManagerInternalUnitTest {
    @Test(description = "Log4j config path normalization should preserve jar resource URLs")
    public void normalizeLog4jConfigPathPreservesJarResourceUrl() {
        String jarResourcePath = "jar:file:/C:/test/shaft-engine.jar!/resources/properties/default/log4j2.properties";

        Assert.assertEquals(PropertyFileManager.normalizeLog4jConfigPath(jarResourcePath), jarResourcePath);
    }
}
