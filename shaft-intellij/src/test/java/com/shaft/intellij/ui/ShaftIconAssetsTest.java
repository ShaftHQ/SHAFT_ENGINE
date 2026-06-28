package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;
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
}
