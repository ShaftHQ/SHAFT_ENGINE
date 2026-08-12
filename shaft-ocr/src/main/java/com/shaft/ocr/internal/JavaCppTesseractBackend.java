package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.tesseract.ResultIterator;
import org.bytedeco.tesseract.TessBaseAPI;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.bytedeco.tesseract.global.tesseract.OEM_LSTM_ONLY;
import static org.bytedeco.tesseract.global.tesseract.PSM_AUTO;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_BLOCK;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_LINE;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_WORD;
import static org.bytedeco.tesseract.global.tesseract.PSM_SPARSE_TEXT;
import static org.bytedeco.tesseract.global.tesseract.RIL_TEXTLINE;
import static org.bytedeco.tesseract.global.tesseract.RIL_WORD;

final class JavaCppTesseractBackend implements TesseractBackend {
    @Override
    public OcrResult recognize(byte[] image, Path tessdataDirectory, String languageCodes, OcrOptions options) {
        if (image == null || image.length == 0) {
            throw new IllegalArgumentException("OCR image bytes cannot be null or empty.");
        }
        byte[] processedImage = OcrImagePreprocessor.apply(image, options.preprocessingMode());
        try (TessBaseAPI api = new TessBaseAPI()) {
            int initialization = api.Init(tessdataDirectory.toString(), languageCodes, OEM_LSTM_ONLY);
            if (initialization != 0) {
                throw new IllegalStateException("Tesseract could not initialize languages '" + languageCodes
                        + "' from " + tessdataDirectory + ".");
            }
            api.SetPageSegMode(pageSegmentationMode(options));
            try (org.bytedeco.leptonica.PIX pix = org.bytedeco.leptonica.global.leptonica
                    .pixReadMemPng(processedImage, processedImage.length)) {
                if (pix == null || pix.isNull()) {
                    throw new IllegalArgumentException("Tesseract could not decode the OCR image as PNG.");
                }
                api.SetImage(pix);
                if (options.region() != null) {
                    if (options.region().right() > imageWidth(pix) || options.region().bottom() > imageHeight(pix)) {
                        throw new IllegalArgumentException("OCR region " + options.region()
                                + " exceeds the decoded image bounds " + imageWidth(pix) + "x" + imageHeight(pix) + ".");
                    }
                    api.SetRectangle(options.region().x(), options.region().y(), options.region().width(), options.region().height());
                }
                if (api.Recognize(null) != 0) {
                    throw new IllegalStateException("Tesseract failed to recognize the OCR image.");
                }
                String fullText;
                try (BytePointer text = api.GetUTF8Text()) {
                    fullText = text == null || text.isNull() ? "" : normalize(text.getString());
                }
                List<OcrTextBlock> blocks = new ArrayList<>();
                try (ResultIterator iterator = api.GetIterator()) {
                    if (iterator != null && !iterator.isNull()) {
                        collect(iterator, RIL_TEXTLINE, OcrBlockLevel.LINE, blocks);
                        iterator.Begin();
                        collect(iterator, RIL_WORD, OcrBlockLevel.WORD, blocks);
                    }
                }
                return new OcrResult(fullText, blocks);
            } finally {
                api.End();
            }
        }
    }

    private static int imageWidth(org.bytedeco.leptonica.PIX pix) {
        return org.bytedeco.leptonica.global.leptonica.pixGetWidth(pix);
    }

    private static int imageHeight(org.bytedeco.leptonica.PIX pix) {
        return org.bytedeco.leptonica.global.leptonica.pixGetHeight(pix);
    }

    private static void collect(ResultIterator iterator, int level, OcrBlockLevel blockLevel,
                                List<OcrTextBlock> output) {
        iterator.Begin();
        do {
            try (BytePointer recognized = iterator.GetUTF8Text(level)) {
                String text = recognized == null || recognized.isNull() ? "" : normalize(recognized.getString());
                int[] left = {0};
                int[] top = {0};
                int[] right = {0};
                int[] bottom = {0};
                if (!text.isBlank() && iterator.BoundingBox(level, left, top, right, bottom)
                        && right[0] > left[0] && bottom[0] > top[0]) {
                    output.add(new OcrTextBlock(text,
                            new OcrRectangle(left[0], top[0], right[0] - left[0], bottom[0] - top[0]),
                            Math.clamp(iterator.Confidence(level) / 100.0, 0, 1), blockLevel));
                }
            }
        } while (iterator.Next(level));
    }

    private static int pageSegmentationMode(OcrOptions options) {
        return switch (options.pageSegmentationMode()) {
            case AUTO -> PSM_AUTO;
            case SINGLE_BLOCK -> PSM_SINGLE_BLOCK;
            case SINGLE_LINE -> PSM_SINGLE_LINE;
            case SINGLE_WORD -> PSM_SINGLE_WORD;
            case SPARSE_TEXT -> PSM_SPARSE_TEXT;
        };
    }

    private static String normalize(String text) {
        return text == null ? "" : text.strip().replaceAll("\\R+", "\n");
    }
}
