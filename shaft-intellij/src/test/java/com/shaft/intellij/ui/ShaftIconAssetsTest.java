package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftIconAssetsTest {
    @Test
    void toolWindowIconsUseSmallLightAndDarkPngAssets() throws IOException {
        BufferedImage light = ImageIO.read(Path.of("src/main/resources/icons/shaftToolWindow.png").toFile());
        BufferedImage dark = ImageIO.read(Path.of("src/main/resources/icons/shaftToolWindow_dark.png").toFile());

        assertAll(
                () -> assertEquals(16, light.getWidth()),
                () -> assertEquals(16, light.getHeight()),
                () -> assertEquals(16, dark.getWidth()),
                () -> assertEquals(16, dark.getHeight()),
                () -> assertTrue(hasNonTransparentPixel(light)),
                () -> assertTrue(hasNonTransparentPixel(dark)));
    }

    @Test
    void pluginDescriptorRegistersDarkModeIconAndRestartRequirement() throws IOException {
        String descriptor = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));
        String javaDescriptor = Files.readString(Path.of(
                "src/main/resources/META-INF/io.github.shafthq.shaft-withJava.xml"));

        assertAll(
                () -> assertTrue(descriptor.contains("require-restart=\"true\"")),
                () -> assertTrue(descriptor.contains("iconDark=\"/icons/shaftToolWindow_dark.png\"")),
                () -> assertTrue(descriptor.contains("optional=\"true\"")),
                () -> assertTrue(descriptor.contains("config-file=\"io.github.shafthq.shaft-withJava.xml\"")),
                () -> assertTrue(javaDescriptor.contains("Shaft.RecordJavaTarget")),
                () -> assertTrue(javaDescriptor.contains("EditorPopupMenu")));
    }

    @Test
    void toolWindowIconsKeepVisiblePixelsOnSelectedToolWindowBackgrounds() throws IOException {
        BufferedImage light = ImageIO.read(Path.of("src/main/resources/icons/shaftToolWindow.png").toFile());
        BufferedImage dark = ImageIO.read(Path.of("src/main/resources/icons/shaftToolWindow_dark.png").toFile());

        assertAll(
                () -> assertTrue(hasContrastingPixel(light, new Color(0xD6E7FF)), "light icon lacks selected-light contrast"),
                () -> assertTrue(hasContrastingPixel(dark, new Color(0x2F5D9F)), "dark icon lacks selected-dark contrast"));
    }

    private static boolean hasNonTransparentPixel(BufferedImage image) {
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                if (((image.getRGB(x, y) >>> 24) & 0xFF) > 0) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean hasContrastingPixel(BufferedImage image, Color background) {
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                int argb = image.getRGB(x, y);
                if (((argb >>> 24) & 0xFF) > 0 && contrast(new Color(argb, true), background) >= 3.0) {
                    return true;
                }
            }
        }
        return false;
    }

    private static double contrast(Color first, Color second) {
        double lighter = Math.max(luminance(first), luminance(second));
        double darker = Math.min(luminance(first), luminance(second));
        return (lighter + 0.05) / (darker + 0.05);
    }

    private static double luminance(Color color) {
        return 0.2126 * channel(color.getRed()) + 0.7152 * channel(color.getGreen()) + 0.0722 * channel(color.getBlue());
    }

    private static double channel(int value) {
        double normalized = value / 255.0;
        return normalized <= 0.03928 ? normalized / 12.92 : Math.pow((normalized + 0.055) / 1.055, 2.4);
    }

}
