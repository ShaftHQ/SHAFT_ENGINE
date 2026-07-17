package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.JLabel;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Coverage for {@link ImagePreviewSupport} (issue #3642), extracted from {@code
 * VisualBaselinesPanel} so {@code AssistantTranscriptView} can reuse the same fail-soft
 * scaled-icon behavior for Doctor/Healer screenshot previews. {@code VisualBaselinesPanelTest}'s
 * pre-existing scan/preview coverage exercises this same logic through the panel; this file adds
 * direct coverage for the extracted pure logic itself.
 */
class ImagePreviewSupportTest {

    @Test
    void readScaledIconScalesDownAnOversizedImageProportionally(@TempDir Path directory) throws IOException {
        Path image = directory.resolve("wide.png");
        writeImage(image, 400, 100);

        Icon icon = ImagePreviewSupport.readScaledIcon(image, 200);

        assertNotNull(icon);
        assertEquals(200, icon.getIconWidth(), "Width is the larger dimension and must be capped to maxDimension");
        assertEquals(50, icon.getIconHeight(), "Height must scale down proportionally with width");
    }

    @Test
    void readScaledIconNeverUpscalesASmallerImage(@TempDir Path directory) throws IOException {
        Path image = directory.resolve("small.png");
        writeImage(image, 40, 20);

        Icon icon = ImagePreviewSupport.readScaledIcon(image, 200);

        assertEquals(40, icon.getIconWidth());
        assertEquals(20, icon.getIconHeight());
    }

    @Test
    void readScaledIconReturnsNullForAMissingFileInsteadOfThrowing(@TempDir Path directory) {
        Icon icon = ImagePreviewSupport.readScaledIcon(directory.resolve("does-not-exist.png"), 200);

        assertNull(icon);
    }

    @Test
    void readScaledIconReturnsNullForAFileThatIsNotAnImage(@TempDir Path directory) throws IOException {
        Path notAnImage = directory.resolve("not-an-image.png");
        Files.writeString(notAnImage, "this is plain text, not PNG bytes");

        Icon icon = ImagePreviewSupport.readScaledIcon(notAnImage, 200);

        assertNull(icon);
    }

    @Test
    void setPreviewFallsBackToUnavailableTextWhenTheImageCannotBeRead(@TempDir Path directory) {
        JLabel label = new JLabel();

        ImagePreviewSupport.setPreview(label, directory.resolve("missing.png"), 200, "No preview available");

        assertNull(label.getIcon());
        assertEquals("No preview available", label.getText());
    }

    @Test
    void setPreviewShowsAScaledIconAndClearsTextWhenTheImageIsReadable(@TempDir Path directory) throws IOException {
        Path image = directory.resolve("ok.png");
        writeImage(image, 40, 40);
        JLabel label = new JLabel();

        ImagePreviewSupport.setPreview(label, image, 200, "No preview available");

        assertNotNull(label.getIcon());
        assertNull(label.getText());
    }

    @Test
    void resetPreviewClearsTheIconAndRestoresThePlaceholderText() {
        JLabel label = new JLabel();
        label.setIcon(ImagePreviewSupport.readScaledIcon(Path.of("unused"), 1));

        ImagePreviewSupport.resetPreview(label, "Select a row to preview");

        assertNull(label.getIcon());
        assertEquals("Select a row to preview", label.getText());
    }

    private static void writeImage(Path path, int width, int height) throws IOException {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        if (!ImageIO.write(image, "png", path.toFile())) {
            throw new IOException("No PNG writer available for " + path);
        }
    }
}
