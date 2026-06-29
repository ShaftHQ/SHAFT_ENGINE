package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
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
                () -> assertTrue(hasNonTransparentPixel(dark)),
                () -> assertTrue(averageOpaqueLuminance(dark) > 180,
                        "Dark-mode SHAFT icon should use the white logo for contrast"));
    }

    @Test
    void pluginDescriptorRegistersDarkModeIconAndRestartRequirement() throws IOException {
        String descriptor = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));

        assertAll(
                () -> assertTrue(descriptor.contains("require-restart=\"true\"")),
                () -> assertTrue(descriptor.contains("iconDark=\"/icons/shaftToolWindow_dark.png\"")));
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

    private static double averageOpaqueLuminance(BufferedImage image) {
        double sum = 0;
        int count = 0;
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                int argb = image.getRGB(x, y);
                if (((argb >>> 24) & 0xFF) == 0) {
                    continue;
                }
                int red = (argb >>> 16) & 0xFF;
                int green = (argb >>> 8) & 0xFF;
                int blue = argb & 0xFF;
                sum += 0.2126 * red + 0.7152 * green + 0.0722 * blue;
                count++;
            }
        }
        return count == 0 ? 0 : sum / count;
    }
}
