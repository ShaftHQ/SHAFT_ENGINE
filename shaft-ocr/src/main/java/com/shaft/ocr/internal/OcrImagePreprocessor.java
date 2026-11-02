package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrPreprocessingMode;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

final class OcrImagePreprocessor {
    private OcrImagePreprocessor() {
    }

    static byte[] apply(byte[] source, OcrPreprocessingMode mode) {
        if (mode == OcrPreprocessingMode.NONE) {
            return source;
        }
        try {
            BufferedImage image = ImageIO.read(new ByteArrayInputStream(source));
            if (image == null) {
                throw new IllegalArgumentException("Tesseract could not decode the OCR image.");
            }
            BufferedImage processed = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_BYTE_GRAY);
            int autoThreshold = mode == OcrPreprocessingMode.AUTO ? otsuThreshold(image) : 128;
            for (int y = 0; y < image.getHeight(); y++) {
                for (int x = 0; x < image.getWidth(); x++) {
                    int rgb = image.getRGB(x, y);
                    int gray = luminanceOnWhite(rgb);
                    int value = switch (mode) {
                        case BINARY -> gray >= 128 ? 255 : 0;
                        case AUTO -> gray > autoThreshold ? 255 : 0;
                        case INVERT -> 255 - gray;
                        case GRAYSCALE -> gray;
                        case NONE -> throw new IllegalStateException("NONE returns before image decoding.");
                    };
                    processed.getRaster().setSample(x, y, 0, value);
                }
            }
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            ImageIO.write(processed, "png", output);
            return output.toByteArray();
        } catch (IllegalArgumentException exception) {
            throw exception;
        } catch (Exception exception) {
            throw new IllegalArgumentException("Tesseract could not preprocess the OCR image.", exception);
        }
    }

    private static int otsuThreshold(BufferedImage image) {
        long[] histogram = new long[256];
        long weightedTotal = 0;
        long pixelCount = 0;
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                int argb = image.getRGB(x, y);
                if ((argb >>> 24 & 0xff) == 0) continue;
                int gray = luminanceOnWhite(argb);
                histogram[gray]++;
                weightedTotal += gray;
                pixelCount++;
            }
        }
        if (pixelCount == 0) return 127;
        long backgroundWeight = 0;
        long backgroundSum = 0;
        double bestVariance = -1;
        int threshold = 127;
        for (int value = 0; value < histogram.length; value++) {
            backgroundWeight += histogram[value];
            if (backgroundWeight == 0) continue;
            long foregroundWeight = pixelCount - backgroundWeight;
            if (foregroundWeight == 0) break;
            backgroundSum += (long) value * histogram[value];
            double backgroundMean = (double) backgroundSum / backgroundWeight;
            double foregroundMean = (double) (weightedTotal - backgroundSum) / foregroundWeight;
            double variance = (double) backgroundWeight * foregroundWeight
                    * (backgroundMean - foregroundMean) * (backgroundMean - foregroundMean);
            if (variance > bestVariance) {
                bestVariance = variance;
                threshold = value;
            }
        }
        return threshold;
    }

    private static int luminanceOnWhite(int argb) {
        int alpha = argb >>> 24 & 0xff;
        int red = argb >>> 16 & 0xff;
        int green = argb >>> 8 & 0xff;
        int blue = argb & 0xff;
        int gray = (299 * red + 587 * green + 114 * blue) / 1000;
        return (gray * alpha + 255 * (255 - alpha)) / 255;
    }
}
