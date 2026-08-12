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
            for (int y = 0; y < image.getHeight(); y++) {
                for (int x = 0; x < image.getWidth(); x++) {
                    int rgb = image.getRGB(x, y);
                    int red = rgb >>> 16 & 0xff;
                    int green = rgb >>> 8 & 0xff;
                    int blue = rgb & 0xff;
                    int gray = (299 * red + 587 * green + 114 * blue) / 1000;
                    int value = switch (mode) {
                        case BINARY, AUTO -> gray >= 128 ? 255 : 0;
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
}
