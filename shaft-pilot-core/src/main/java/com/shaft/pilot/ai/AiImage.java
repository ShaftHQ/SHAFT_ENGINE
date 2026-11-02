package com.shaft.pilot.ai;

import java.util.Arrays;
import java.util.Objects;

/**
 * Immutable image evidence.
 *
 * @param category evidence category
 * @param mediaType image media type
 * @param data encoded image bytes
 */
public record AiImage(EvidenceCategory category, String mediaType, byte[] data) {
    /**
     * Creates an immutable defensive copy of image data.
     */
    public AiImage {
        category = Objects.requireNonNull(category, "category");
        mediaType = Objects.requireNonNullElse(mediaType, "image/png");
        data = data == null ? new byte[0] : Arrays.copyOf(data, data.length);
    }

    @Override
    public byte[] data() {
        return Arrays.copyOf(data, data.length);
    }
}
