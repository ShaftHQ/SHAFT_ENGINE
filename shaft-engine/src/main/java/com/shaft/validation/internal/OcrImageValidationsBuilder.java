package com.shaft.validation.internal;

import com.shaft.gui.internal.ocr.OcrProcessingActions;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.validation.ValidationEnums;

import java.util.Arrays;
import java.util.Objects;

/** Fluent OCR assertions for an encoded standalone image. */
public final class OcrImageValidationsBuilder {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final byte[] image;

    OcrImageValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, byte[] image) {
        this.validationCategory = Objects.requireNonNull(validationCategory, "validationCategory");
        if (image == null || image.length == 0) {
            throw new IllegalArgumentException("OCR image bytes cannot be null or empty.");
        }
        this.image = Arrays.copyOf(image, image.length);
    }

    /** Recognizes text using default OCR options and starts a native string assertion. */
    public NativeValidationsBuilder ocrText() {
        return ocrText(OcrOptions.defaults());
    }

    /** Recognizes text using explicit OCR options and starts a native string assertion. */
    public NativeValidationsBuilder ocrText(OcrOptions options) {
        String text = OcrProcessingActions.recognize(image, options).fullText();
        return new ValidationsBuilder(validationCategory).object(text);
    }
}
