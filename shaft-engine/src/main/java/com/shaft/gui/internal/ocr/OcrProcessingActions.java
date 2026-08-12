package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.tools.io.internal.ReportManagerHelper;

import java.io.ByteArrayInputStream;
import java.util.Objects;

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

    public static OcrMatch find(byte[] image, OcrTarget target) {
        Objects.requireNonNull(target, "target");
        OcrMatch match = OcrTargetResolver.resolve(recognize(image, target.options()), target);
        ReportManagerHelper.attach("text", "SHAFT OCR selected match", "Target: " + target + System.lineSeparator()
                + "Selected match: " + match);
        return match;
    }

    private static void attachEvidence(byte[] image, String diagnostics) {
        ReportManagerHelper.attach("screenshot", "SHAFT OCR source image", new ByteArrayInputStream(image));
        ReportManagerHelper.attach("text", "SHAFT OCR recognition details", diagnostics);
    }
}
