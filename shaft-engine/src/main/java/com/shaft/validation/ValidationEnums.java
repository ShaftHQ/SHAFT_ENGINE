package com.shaft.validation;

import lombok.Getter;

/**
 * Defines the enumerations used by SHAFT's validation subsystem to control
 * assertion behavior, comparison semantics, and validation state.
 *
 * <p>Key enums:
 * <ul>
 *   <li>{@link ValidationType} &ndash; Positive vs. negative assertions.</li>
 *   <li>{@link ValidationComparisonType} &ndash; Equals, contains, matches, or case-insensitive.</li>
 *   <li>{@link ValidationCategory} &ndash; Hard assert vs. soft assert.</li>
 *   <li>{@link VisualValidationEngine} &ndash; Visual comparison engine selection.</li>
 *   <li>{@link NumbersComparativeRelation} &ndash; Numeric comparison operators.</li>
 *   <li>{@link ValidationState} &ndash; Passed vs. failed result states.</li>
 * </ul>
 */
public class ValidationEnums {

    /** Controls whether a validation checks for a match (positive) or no match (negative). */
    public enum ValidationType {
        POSITIVE(true), NEGATIVE(false);

        private final Boolean value;

        ValidationType(Boolean type) {
            this.value = type;
        }

        public boolean getValue() {
            return value;
        }
    }

    /** Specifies the string comparison strategy for text-based validations. */
    @Getter
    public enum ValidationComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        ValidationComparisonType(int type) {
            this.value = type;
        }

    }

    /** Supported text directions used in localization and layout validations. */
    @Getter
    public enum TextDirection {
        LTR("ltr"),
        RTL("rtl");

        private final String value;

        TextDirection(String value) {
            this.value = value;
        }
    }

    /**
     * Supported language presets for text-localization validations.
     *
     * <p>Each preset maps to a Unicode-aware regex that checks whether the target text contains
     * at least one character from the selected language family. For broader scenarios, use
     * language code-based assertions in the language builder.</p>
     */
    @Getter
    public enum TextLanguage {
        ARABIC("ar", "(?s).*[\\p{InArabic}].*"),
        ENGLISH("en", "(?s).*[A-Za-z].*"),
        SPANISH("es", "(?s).*[A-Za-zÁÉÍÓÚÜÑáéíóúüñ].*"),
        FRENCH("fr", "(?s).*[A-Za-zÀÂÆÇÉÈÊËÏÎÔŒÙÛÜŸàâæçéèêëïîôœùûüÿ].*"),
        GERMAN("de", "(?s).*[A-Za-zÄÖÜẞäöüß].*");

        private final String languageCode;
        private final String detectionRegex;

        TextLanguage(String languageCode, String detectionRegex) {
            this.languageCode = languageCode;
            this.detectionRegex = detectionRegex;
        }
    }

    /** Identifies the visual comparison engine to use for image-based validations. */
    public enum VisualValidationEngine {
        EXACT_SHUTTERBUG,
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }

    /** Distinguishes hard assertions from soft assertions (verifications). */
    public enum ValidationCategory {
        HARD_ASSERT,
        SOFT_ASSERT
    }

    /** Defines relational operators for numeric comparisons. */
    @Getter
    public enum NumbersComparativeRelation {
        GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

        private final String value;

        NumbersComparativeRelation(String type) {
            this.value = type;
        }

    }

    /** Represents the outcome of a validation check: passed or failed. */
    public enum ValidationState {
        PASSED(true), FAILED(false);

        private final Boolean value;

        ValidationState(Boolean type) {
            this.value = type;
        }

        public boolean getValue() {
            return value;
        }
    }
}
