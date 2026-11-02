package com.shaft.validation.internal;

/**
 * One failure string for hard asserts and soft-assert accumulation.
 * A non-blank custom report message is included with the expected/actual text.
 * A blank message leaves the generated wording unchanged. {@code forceFail} already
 * places the custom message in the actual value, so that path is not prefixed again.
 */
public final class ValidationFailureText {
    private ValidationFailureText() {
    }

    public static String compose(String categoryString, Object expected, Object actual, String customReportMessage) {
        String category = categoryString == null ? "Assert" : categoryString;
        String failure = category.replace("erify", "erificat")
                + "ion failed; expected " + expected + ", but found " + actual;
        if (customReportMessage == null || customReportMessage.isBlank()) {
            return failure;
        }
        String custom = customReportMessage.strip();
        String actualText = actual == null ? "" : String.valueOf(actual);
        if (actualText.equals(custom) || actualText.equals(customReportMessage) || failure.contains(custom)) {
            return failure;
        }
        return custom + " — " + failure;
    }
}
