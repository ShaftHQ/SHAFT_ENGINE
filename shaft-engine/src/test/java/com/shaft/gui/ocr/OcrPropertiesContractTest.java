package com.shaft.gui.ocr;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class OcrPropertiesContractTest {
    @AfterMethod
    public void clearProperties() {
        com.shaft.properties.internal.Properties.clearForCurrentThread();
    }

    @Test
    public void exposesTypedDefaultsAndCurrentThreadOverrides() {
        Assert.assertTrue(SHAFT.Properties.ocr.downloadEnabled());

        SHAFT.Properties.ocr.set().downloadEnabled(false).cacheDirectory("build/ocr-cache");

        Assert.assertFalse(SHAFT.Properties.ocr.downloadEnabled());
        Assert.assertEquals(SHAFT.Properties.ocr.cacheDirectory(), "build/ocr-cache");
    }
}
