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
        return source == null ? null : shrink(source, png.length, maxBytes);
    }

    private static byte[] shrink(BufferedImage source, int originalBytes, long maxBytes) {
        double scale = Math.sqrt((double) maxBytes / originalBytes) * SAFETY;
        for (int attempt = 0; attempt < MAX_ATTEMPTS; attempt++) {
            Fitted fitted = encodeAt(source, scale);
            if (fitted.tooSmall()) {
                return null;
            }
            if (fitted.encoded() != null && fitted.encoded().length <= maxBytes) {
                return fitted.encoded();
            }
            scale = nextScale(scale, maxBytes, fitted.encoded());
        }
        return null;
    }

    private static Fitted encodeAt(BufferedImage source, double scale) {
        int width = (int) Math.round(source.getWidth() * scale);
        int height = (int) Math.round(source.getHeight() * scale);
        if (width < MIN_EDGE || height < MIN_EDGE) {
            return new Fitted(null, true);
        }
        return new Fitted(encode(resize(source, width, height)), false);
    }

    private static double nextScale(double scale, long maxBytes, byte[] encoded) {
        if (encoded == null) {
            return scale * 0.75;
        }
        return scale * Math.min(0.9, Math.sqrt((double) maxBytes / encoded.length) * SAFETY);
    }

    private record Fitted(byte[] encoded, boolean tooSmall) {
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
