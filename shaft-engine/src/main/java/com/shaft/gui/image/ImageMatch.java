package com.shaft.gui.image;

import java.util.Map;
import java.util.Objects;

/**
 * A geometrically located screenshot match with the evidence used to rank it.
 *
 * @param bounds      match bounds in screenshot pixels
 * @param confidence normalized confidence from 0 through 1
 * @param scale       scale of the matched target relative to its source image
 * @param algorithm   algorithm that produced the match
 * @param diagnostics immutable provider-specific diagnostic values
 */
public record ImageMatch(ImageRectangle bounds, double confidence, double scale,
                         ImageMatchingAlgorithm algorithm, Map<String, String> diagnostics) {
    public ImageMatch {
        Objects.requireNonNull(bounds, "Image match bounds cannot be null.");
        Objects.requireNonNull(algorithm, "Image matching algorithm cannot be null.");
        if (!Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("Image match confidence must be a finite value from 0 through 1.");
        }
        if (!Double.isFinite(scale) || scale <= 0) {
            throw new IllegalArgumentException("Image match scale must be a positive finite value.");
        }
        diagnostics = diagnostics == null ? Map.of() : Map.copyOf(diagnostics);
    }

    public int centerX() {
        return bounds.centerX();
    }

    public int centerY() {
        return bounds.centerY();
    }
}
