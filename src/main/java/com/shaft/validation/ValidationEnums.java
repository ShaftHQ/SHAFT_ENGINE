package com.shaft.validation;

import lombok.Getter;

public class ValidationEnums {

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

    @Getter
    public enum ValidationComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        ValidationComparisonType(int type) {
            this.value = type;
        }

    }

    public enum VisualValidationEngine {
        EXACT_SHUTTERBUG,
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }

    public enum ValidationCategory {
        HARD_ASSERT,
        SOFT_ASSERT
    }

    @Getter
    public enum NumbersComparativeRelation {
        GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

        private final String value;

        NumbersComparativeRelation(String type) {
            this.value = type;
        }

    }

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
