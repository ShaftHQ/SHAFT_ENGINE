package com.shaft.gui.image;

/**
 * An immutable rectangle expressed in screenshot pixels.
 *
 * @param x      left coordinate
 * @param y      top coordinate
 * @param width  rectangle width
 * @param height rectangle height
 */
public record ImageRectangle(int x, int y, int width, int height) {
    public ImageRectangle {
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException("Image rectangle coordinates cannot be negative.");
        }
        if (width <= 0 || height <= 0) {
            throw new IllegalArgumentException("Image rectangle dimensions must be positive.");
        }
        try {
            Math.addExact(x, width);
            Math.addExact(y, height);
        } catch (ArithmeticException exception) {
            throw new IllegalArgumentException("Image rectangle edges must fit within integer coordinates.", exception);
        }
    }

    public int centerX() {
        return Math.addExact(x, width / 2);
    }

    public int centerY() {
        return Math.addExact(y, height / 2);
    }
}
