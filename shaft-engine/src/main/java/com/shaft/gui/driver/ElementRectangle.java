package com.shaft.gui.driver;

/** Backend-neutral element bounds in CSS pixels. */
public record ElementRectangle(double x, double y, double width, double height) {
    public ElementRectangle {
        if (!Double.isFinite(x) || !Double.isFinite(y)
                || !Double.isFinite(width) || !Double.isFinite(height)
                || width < 0 || height < 0) {
            throw new IllegalArgumentException("Element rectangle values must be finite with non-negative dimensions.");
        }
    }
}
