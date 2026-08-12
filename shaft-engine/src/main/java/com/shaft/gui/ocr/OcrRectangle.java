package com.shaft.gui.ocr;

/** Pixel-space rectangle within the OCR source image. */
public record OcrRectangle(int x, int y, int width, int height) {
    public OcrRectangle {
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException("OCR rectangle coordinates cannot be negative.");
        }
        if (width <= 0 || height <= 0) {
            throw new IllegalArgumentException("OCR rectangle width and height must be greater than zero.");
        }
    }

    public int right() {
        return Math.addExact(x, width);
    }

    public int bottom() {
        return Math.addExact(y, height);
    }

    public int centerX() {
        return x + width / 2;
    }

    public int centerY() {
        return y + height / 2;
    }

    public OcrRectangle union(OcrRectangle other) {
        if (other == null) {
            throw new IllegalArgumentException("OCR rectangle to union cannot be null.");
        }
        int left = Math.min(x, other.x);
        int top = Math.min(y, other.y);
        int right = Math.max(right(), other.right());
        int bottom = Math.max(bottom(), other.bottom());
        return new OcrRectangle(left, top, right - left, bottom - top);
    }

    public OcrRectangle translate(int xOffset, int yOffset) {
        return new OcrRectangle(Math.addExact(x, xOffset), Math.addExact(y, yOffset), width, height);
    }
}
