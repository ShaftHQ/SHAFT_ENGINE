package com.shaft.tools.io.internal;

import javax.imageio.ImageIO;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/** Shrinks an over-budget trace screenshot, keeping its aspect ratio, so it can stay in the trace. */
final class TraceScreenshotDownscaler {
    private static final int MAX_ATTEMPTS = 8;
    private static final int MIN_EDGE = 16;
    private static final double SAFETY = 0.9;

    private TraceScreenshotDownscaler() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Downscales a PNG until it fits {@code maxBytes}.
     *
     * @param png      encoded image bytes
     * @param maxBytes per-artifact budget
     * @return PNG bytes that fit the budget, or {@code null} when the image cannot be decoded or shrunk enough
     */
    static byte[] fit(byte[] png, long maxBytes) {
        if (png == null || png.length == 0 || maxBytes <= 0) {
            return null;
        }
        if (png.length <= maxBytes) {
            return png;
        }
        BufferedImage source = decode(png);
        if (source == null) {
            return null;
        }
        double scale = Math.sqrt((double) maxBytes / png.length) * SAFETY;
        for (int attempt = 0; attempt < MAX_ATTEMPTS; attempt++) {
            int width = (int) Math.round(source.getWidth() * scale);
            int height = (int) Math.round(source.getHeight() * scale);
            if (width < MIN_EDGE || height < MIN_EDGE) {
                return null;
            }
            byte[] encoded = encode(resize(source, width, height));
            if (encoded != null && encoded.length <= maxBytes) {
                return encoded;
            }
            scale *= encoded == null ? 0.75 : Math.min(0.9, Math.sqrt((double) maxBytes / encoded.length) * SAFETY);
        }
        return null;
    }

    private static BufferedImage decode(byte[] png) {
        try {
            return ImageIO.read(new ByteArrayInputStream(png));
        } catch (IOException | RuntimeException e) {
            return null;
        }
    }

    private static BufferedImage resize(BufferedImage source, int width, int height) {
        boolean alpha = source.getColorModel().hasAlpha();
        BufferedImage target = new BufferedImage(width, height,
                alpha ? BufferedImage.TYPE_INT_ARGB : BufferedImage.TYPE_INT_RGB);
        var graphics = target.createGraphics();
        try {
            graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            graphics.drawImage(source, 0, 0, width, height, null);
        } finally {
            graphics.dispose();
        }
        return target;
    }

    private static byte[] encode(BufferedImage image) {
        try (ByteArrayOutputStream output = new ByteArrayOutputStream()) {
            return ImageIO.write(image, "png", output) ? output.toByteArray() : null;
        } catch (IOException | RuntimeException e) {
            return null;
        }
    }
}
