package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;

/** SHAFT-owned service-provider contract for optional local OCR engines. */
public interface OcrProcessingProvider {
    /** Recognizes text and geometry from an encoded image. */
    OcrResult recognize(byte[] image, OcrOptions options);

    /** Stable provider name used in diagnostics. */
    default String name() {
        return getClass().getName();
    }

    /** Higher values win when more than one implementation is installed. */
    default int priority() {
        return 0;
    }
}
