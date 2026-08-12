package com.shaft.ocr.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class OcrLanguageRegistryTest {
    @Test
    public void mapsChineseNamesToActualTesseractModelCodes() {
        Assert.assertEquals(OcrLanguageRegistry.resolve(List.of("Chinese")), List.of("chi_sim"));
        Assert.assertEquals(OcrLanguageRegistry.resolve(List.of("Traditional Chinese")), List.of("chi_tra"));
    }
    @Test
    public void defaultsToEnglishAndArabicAndAcceptsNamesOrCodes() {
        Assert.assertEquals(OcrLanguageRegistry.resolve(List.of()), List.of("eng", "ara"));
        Assert.assertEquals(OcrLanguageRegistry.resolve(List.of("Arabic", "English")), List.of("ara", "eng"));
        Assert.assertEquals(OcrLanguageRegistry.resolve(List.of("deu", "French")), List.of("deu", "fra"));
    }

    @Test
    public void rejectsUnknownLanguageWithSupportedValueGuidance() {
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> OcrLanguageRegistry.resolve(List.of("Klingon")));
        Assert.assertTrue(error.getMessage().contains("Klingon"));
        Assert.assertTrue(error.getMessage().contains("English"));
    }
}
