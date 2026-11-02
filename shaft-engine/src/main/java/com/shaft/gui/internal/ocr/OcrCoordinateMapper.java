package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrMatch;

public final class OcrCoordinateMapper {
    private OcrCoordinateMapper() {
    }

    public static OcrPoint toPointerCenter(OcrMatch match,
                                           int screenshotWidth, int screenshotHeight,
                                           int targetWidth, int targetHeight,
                                           int originX, int originY) {
        if (screenshotWidth <= 0 || screenshotHeight <= 0 || targetWidth <= 0 || targetHeight <= 0) {
            throw new IllegalArgumentException("OCR screenshot and pointer target dimensions must be greater than zero.");
        }
        double xScale = (double) targetWidth / screenshotWidth;
        double yScale = (double) targetHeight / screenshotHeight;
        int x = originX + (int) Math.round(match.bounds().centerX() * xScale);
        int y = originY + (int) Math.round(match.bounds().centerY() * yScale);
        return new OcrPoint(x, y);
    }
}
