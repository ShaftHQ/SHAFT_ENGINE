package com.shaft.validation;

public class ValidationEnums {

    protected enum ValidationType {
        POSITIVE(true), NEGATIVE(false);

        private final Boolean value;

        ValidationType(Boolean type) {
            this.value = type;
        }

        protected boolean getValue() {
            return value;
        }
    }

    public enum ValidationComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        ValidationComparisonType(int type) {
            this.value = type;
        }

        protected int getValue() {
            return value;
        }
    }

    protected enum VisualValidationEngine {
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }

    protected enum ValidationCategory {
        HARD_ASSERT,
        SOFT_ASSERT
    }

    public enum NumbersComparativeRelation {
        GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

        private final String value;

        NumbersComparativeRelation(String type) {
            this.value = type;
        }

        protected String getValue() {
            return value;
        }
    }

    protected enum ValidationState {
        PASSED(true), FAILED(false);

        private final Boolean value;

        ValidationState(Boolean type) {
            this.value = type;
        }

        protected boolean getValue() {
            return value;
        }
    }

    public enum ElementAttribute {
        TEXT("text"), TAG_NAME("tagname"), SIZE("size"), SELECTED_TEXT("selectedtext");

        private final String value;

        ElementAttribute(String type) {
            this.value = type;
        }

        protected String getValue() {
            return value;
        }
    }

    public enum BrowserAttribute {
        CURRENT_URL("currenturl"), PAGE_SOURCE("pagesource"), TITLE("title"), WINDOW_HANDLE("windowhandle"), WINDOW_POSITION("windowposition"), WINDOW_SIZE("windowsize");

        private final String value;

        BrowserAttribute(String type) {
            this.value = type;
        }

        protected String getValue() {
            return value;
        }
    }
}
