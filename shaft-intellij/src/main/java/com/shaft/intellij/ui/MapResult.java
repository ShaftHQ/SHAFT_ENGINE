package com.shaft.intellij.ui;

import java.util.Objects;

/**
 * Three-state result of mapping one structured-stream JSON event to a human-readable line,
 * returned by {@link StreamEventMapper#map}. Replaces the previous design's {@code CONSUMED = ""}
 * string sentinel (issue #3974's follow-up on #3922): an empty string that also had to be
 * distinguished from {@code null} was exactly the kind of magic-value footgun that silently
 * breaks when a future branch forgets the sentinel check.
 *
 * <ul>
 *   <li>{@link Rendered} carries the translated line {@link StructuredStreamParser#accept} should
 *       deliver to the live output consumer.
 *   <li>{@link Consumed} means the event was fully absorbed into parser state (a terminal
 *       result/turn event that becomes the final answer/usage) and must be neither shown nor
 *       passed through raw.
 *   <li>{@link Unknown} means no mapping exists for this event, so {@link
 *       StructuredStreamParser#accept} falls back to raw-JSON passthrough -- visible only in
 *       Verbose mode, so nothing the CLI sent is ever silently hidden.
 * </ul>
 */
sealed interface MapResult {
    Consumed CONSUMED = new Consumed();
    Unknown UNKNOWN = new Unknown();

    static MapResult rendered(String text) {
        return new Rendered(text);
    }

    record Rendered(String text) implements MapResult {
        public Rendered {
            Objects.requireNonNull(text, "text");
        }
    }

    record Consumed() implements MapResult {
    }

    record Unknown() implements MapResult {
    }
}
