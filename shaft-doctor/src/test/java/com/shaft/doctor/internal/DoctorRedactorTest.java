package com.shaft.doctor.internal;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorRedactorTest {

    @Test
    void extractSensitiveRegionsDetectsPasswordFields() {
        DoctorRedactor redactor = new DoctorRedactor();
        String htmlWithPassword = "<html><body><input type=\"password\" value=\"secret\"></body></html>";
        List<String> regions = redactor.extractSensitiveRegions(htmlWithPassword);
        assertEquals(List.of("WHOLE_IMAGE"), regions, "Password field should trigger whole-image mask marker");
    }

    @Test
    void extractSensitiveRegionsDetectsAutocompletePasswordFields() {
        DoctorRedactor redactor = new DoctorRedactor();
        String htmlWithAutocomplete = "<html><body><input autocomplete=\"current-password\"></body></html>";
        List<String> regions = redactor.extractSensitiveRegions(htmlWithAutocomplete);
        assertEquals(List.of("WHOLE_IMAGE"), regions, "Autocomplete password field should trigger whole-image mask");
    }

    @Test
    void extractSensitiveRegionsDetectsSensitiveAttributes() {
        DoctorRedactor redactor = new DoctorRedactor();
        String htmlWithSensitiveAttribute = "<html><body><div api-key=\"secret123\"></div></body></html>";
        List<String> regions = redactor.extractSensitiveRegions(htmlWithSensitiveAttribute);
        assertEquals(List.of("WHOLE_IMAGE"), regions, "Sensitive attribute should trigger whole-image mask");
    }

    @Test
    void extractSensitiveRegionsReturnsEmptyListForBenignHtml() {
        DoctorRedactor redactor = new DoctorRedactor();
        String benignHtml = "<html><body><h1>Hello World</h1><p>Normal content</p></body></html>";
        List<String> regions = redactor.extractSensitiveRegions(benignHtml);
        assertEquals(List.of(), regions, "Benign HTML should return empty list (no masking needed)");
    }

    @Test
    void extractSensitiveRegionsHandlesNullInput() {
        DoctorRedactor redactor = new DoctorRedactor();
        List<String> regions = redactor.extractSensitiveRegions(null);
        assertEquals(List.of(), regions, "Null input should return empty list");
    }

    @Test
    void extractSensitiveRegionsHandlesBlankInput() {
        DoctorRedactor redactor = new DoctorRedactor();
        List<String> regions = redactor.extractSensitiveRegions("   ");
        assertEquals(List.of(), regions, "Blank input should return empty list");
    }

    @Test
    void redactScreenshotAppliesFullImageMaskWhenWholeImageMarkerPresent() throws IOException {
        DoctorRedactor redactor = new DoctorRedactor();
        byte[] originalImage = createMinimalPngImage();
        List<String> regions = List.of("WHOLE_IMAGE");
        byte[] maskedImage = redactor.redactScreenshot(originalImage, regions);
        assertNotEquals(originalImage, maskedImage, "Masked image should differ from original");
        assertFalse(java.util.Arrays.equals(originalImage, maskedImage), "Image bytes should be different after masking");
        assertTrue(maskedImage.length > 0, "Masked image should not be empty");
    }

    @Test
    void redactScreenshotDoesNotMaskWhenRegionsEmpty() throws IOException {
        DoctorRedactor redactor = new DoctorRedactor();
        byte[] originalImage = createMinimalPngImage();
        List<String> regions = List.of();
        byte[] result = redactor.redactScreenshot(originalImage, regions);
        assertTrue(java.util.Arrays.equals(originalImage, result),
                "Image should be returned unchanged when no regions are marked for masking");
    }

    @Test
    void redactScreenshotHandlesNullInput() throws IOException {
        DoctorRedactor redactor = new DoctorRedactor();
        List<String> regions = List.of("WHOLE_IMAGE");
        byte[] result = redactor.redactScreenshot(null, regions);
        assertEquals(null, result, "Null screenshot should return null");
    }

    @Test
    void redactScreenshotHandlesEmptyInput() throws IOException {
        DoctorRedactor redactor = new DoctorRedactor();
        byte[] emptyImage = new byte[0];
        List<String> regions = List.of("WHOLE_IMAGE");
        byte[] result = redactor.redactScreenshot(emptyImage, regions);
        assertEquals(emptyImage, result, "Empty screenshot should be returned unchanged");
    }

    @Test
    void redactScreenshotHandlesInvalidImageData() throws IOException {
        DoctorRedactor redactor = new DoctorRedactor();
        byte[] invalidImage = "not an image".getBytes();
        List<String> regions = List.of("WHOLE_IMAGE");
        byte[] result = redactor.redactScreenshot(invalidImage, regions);
        assertEquals(invalidImage, result, "Invalid image data should be returned unchanged (graceful fallback)");
    }

    @Test
    void redactScreenshotMasksPixelsAsOpaque() throws IOException {
        DoctorRedactor redactor = new DoctorRedactor();
        byte[] originalImage = createMinimalPngImage();
        List<String> regions = List.of("WHOLE_IMAGE");
        byte[] maskedImage = redactor.redactScreenshot(originalImage, regions);
        java.awt.image.BufferedImage image = javax.imageio.ImageIO.read(
                new java.io.ByteArrayInputStream(maskedImage));
        if (image != null) {
            int expectedBlack = 0xFF000000;
            for (int y = 0; y < image.getHeight(); y++) {
                for (int x = 0; x < image.getWidth(); x++) {
                    int pixel = image.getRGB(x, y);
                    assertEquals(expectedBlack, pixel, "All pixels should be opaque black");
                }
            }
        }
    }

    private byte[] createMinimalPngImage() throws IOException {
        java.awt.image.BufferedImage image = new java.awt.image.BufferedImage(
                10, 10, java.awt.image.BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < 10; y++) {
            for (int x = 0; x < 10; x++) {
                image.setRGB(x, y, 0xFFFFFFFF);
            }
        }
        java.io.ByteArrayOutputStream output = new java.io.ByteArrayOutputStream();
        javax.imageio.ImageIO.write(image, "png", output);
        return output.toByteArray();
    }
}
