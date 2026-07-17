package com.shaft.intellij.ui;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Path;

/**
 * Fail-soft image preview helpers shared by panels that render a scaled thumbnail from an
 * on-disk image path: an unreadable/missing image degrades to a placeholder or {@code null}, it
 * never throws. Extracted from {@code VisualBaselinesPanel} (issue #3642) so
 * {@code AssistantTranscriptView} can reuse the same scaling/fail-soft behavior for Doctor/Healer
 * screenshot previews.
 */
final class ImagePreviewSupport {
    private ImagePreviewSupport() {
    }

    /**
     * Reads {@code imagePath} and returns a scaled icon, or {@code null} when the file cannot be
     * read as an image. Safe to call off the EDT: this only performs blocking file IO and image
     * decoding, no Swing component access.
     *
     * @param imagePath candidate image path
     * @param maxDimension largest allowed width/height in pixels; the image is downscaled
     *     proportionally to fit, never upscaled
     * @return scaled icon, or {@code null} when unreadable
     */
    static Icon readScaledIcon(Path imagePath, int maxDimension) {
        try {
            BufferedImage image = ImageIO.read(imagePath.toFile());
            return image == null ? null : scaledIcon(image, maxDimension);
        } catch (IOException | RuntimeException unreadableImage) {
            return null;
        }
    }

    /**
     * Sets {@code label}'s icon from {@code imagePath}, scaled to fit {@code maxDimension}, or
     * falls back to {@code unavailableText} when the image cannot be read.
     *
     * @param label preview label to update
     * @param imagePath candidate image path
     * @param maxDimension largest allowed width/height in pixels
     * @param unavailableText text shown in place of an icon when the image cannot be read
     */
    static void setPreview(JLabel label, Path imagePath, int maxDimension, String unavailableText) {
        Icon icon = readScaledIcon(imagePath, maxDimension);
        if (icon == null) {
            label.setIcon(null);
            label.setText(unavailableText);
        } else {
            label.setIcon(icon);
            label.setText(null);
        }
    }

    /**
     * Clears {@code label} back to its unselected placeholder state.
     *
     * @param label preview label to reset
     * @param placeholderText text shown while nothing is selected
     */
    static void resetPreview(JLabel label, String placeholderText) {
        label.setIcon(null);
        label.setText(placeholderText);
    }

    private static Icon scaledIcon(BufferedImage image, int maxDimension) {
        int width = image.getWidth();
        int height = image.getHeight();
        double scale = Math.min(1.0D, (double) maxDimension / Math.max(1, Math.max(width, height)));
        int scaledWidth = Math.max(1, (int) Math.round(width * scale));
        int scaledHeight = Math.max(1, (int) Math.round(height * scale));
        Image scaled = image.getScaledInstance(scaledWidth, scaledHeight, Image.SCALE_SMOOTH);
        return new ImageIcon(scaled);
    }
}
