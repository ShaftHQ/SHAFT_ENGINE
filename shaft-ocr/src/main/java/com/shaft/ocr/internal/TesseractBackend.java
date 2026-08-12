package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;

import java.nio.file.Path;

interface TesseractBackend {
    OcrResult recognize(byte[] image, Path tessdataDirectory, String languageCodes, OcrOptions options);
}
