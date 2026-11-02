package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.tools.io.internal.ReportManagerHelper;

import javax.imageio.ImageIO;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Objects;
import java.util.function.Supplier;

/** Provider-neutral OCR orchestration used by SHAFT actions and assertions. */
public final class OcrProcessingActions {
    private OcrProcessingActions() {
    }

    public static OcrResult recognize(byte[] image, OcrOptions options) {
        if (image == null || image.length == 0) {
            throw new IllegalArgumentException("OCR image bytes cannot be null or empty.");
        }
        OcrProcessingProvider provider = OcrProcessingProviderRegistry.requireProvider();
        try {
            OcrResult result = Objects.requireNonNull(
                    provider.recognize(image, Objects.requireNonNull(options, "options")),
                    "OCR provider returned a null result");
            attachEvidence(image, "Provider: " + provider.name() + System.lineSeparator()
                    + "Options: " + options + System.lineSeparator() + "Result: " + result);
            return result;
        } catch (RuntimeException exception) {
            attachEvidence(image, "Provider: " + provider.name() + System.lineSeparator()
                    + "Options: " + options + System.lineSeparator() + "Failure: " + exception);
            throw exception;
        }
    }

    /** Recognizes a rendered PDF page with optional provider-native orientation and deskew analysis. */
    public static OcrDocumentPageAnalysis analyzeDocumentPage(byte[] image, OcrOptions options,
                                                               boolean detectOrientation, boolean deskew) {
        return documentPageAnalysisTask(image, options, detectOrientation, deskew).get();
    }

    /** Captures the effective provider on the caller thread for bounded asynchronous document processing. */
    public static Supplier<OcrDocumentPageAnalysis> documentPageAnalysisTask(byte[] image, OcrOptions options,
                                                                              boolean detectOrientation,
                                                                              boolean deskew) {
        Objects.requireNonNull(image, "image");
        Objects.requireNonNull(options, "options");
        if (image.length == 0) {
            throw new IllegalArgumentException("OCR document page bytes cannot be empty.");
        }
        OcrProcessingProvider provider = OcrProcessingProviderRegistry.requireProvider();
        return () -> Objects.requireNonNull(provider.analyzeDocumentPage(image, options, detectOrientation, deskew),
                "OCR provider returned a null document page analysis.");
    }

    public static OcrMatch find(byte[] image, OcrTarget target) {
        Objects.requireNonNull(target, "target");
        OcrResult originalResult = recognize(image, target.options());
        try {
            return attachSelectedMatch(OcrTargetResolver.resolve(originalResult, target), target);
        } catch (IllegalStateException noMatch) {
            if (!isMissingMatch(noMatch)) {
                throw noMatch;
            }
            byte[] scaled = scaleImage2x(image);
            if (scaled == null) {
                throw noMatch;
            }
            try {
                OcrMatch scaledMatch = OcrTargetResolver.resolve(recognize(scaled, target.options()), target);
                return attachSelectedMatch(mapMatchToOriginalScale(scaledMatch), target);
            } catch (IllegalStateException scaledMiss) {
                throw scaledMiss;
            }
        }
    }

    private static boolean isMissingMatch(IllegalStateException exception) {
        String message = exception.getMessage();
        return message != null && message.startsWith("No OCR match found");
    }

    private static OcrMatch attachSelectedMatch(OcrMatch match, OcrTarget target) {
        ReportManagerHelper.attach("text", "SHAFT OCR selected match", "Target: " + target + System.lineSeparator()
                + "Selected match: " + match);
        return match;
    }

    private static OcrMatch mapMatchToOriginalScale(OcrMatch match) {
        OcrRectangle bounds = match.bounds();
        return new OcrMatch(match.text(), new OcrRectangle(
                bounds.x() / 2,
                bounds.y() / 2,
                Math.max(1, bounds.width() / 2),
                Math.max(1, bounds.height() / 2)), match.confidence());
    }

    private static byte[] scaleImage2x(byte[] image) {
        try {
            BufferedImage source = ImageIO.read(new ByteArrayInputStream(image));
            if (source == null || source.getWidth() <= 0 || source.getHeight() <= 0) {
                return null;
            }
            int width = Math.multiplyExact(source.getWidth(), 2);
            int height = Math.multiplyExact(source.getHeight(), 2);
            BufferedImage scaled = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            Graphics2D graphics = scaled.createGraphics();
            try {
                graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                graphics.drawImage(source, 0, 0, width, height, null);
            } finally {
                graphics.dispose();
            }
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            if (!ImageIO.write(scaled, "png", output)) {
                return null;
            }
            return output.toByteArray();
        } catch (IOException | ArithmeticException ignored) {
            return null;
        }
    }

    private static void attachEvidence(byte[] image, String diagnostics) {
        ReportManagerHelper.attach("screenshot", "SHAFT OCR source image", new ByteArrayInputStream(image));
        ReportManagerHelper.attach("text", "SHAFT OCR recognition details", diagnostics);
    }
}
