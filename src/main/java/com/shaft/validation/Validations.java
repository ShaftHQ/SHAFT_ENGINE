package com.shaft.validation;

import com.shaft.validation.internal.builder.Standalone;

public class Validations {
    private Validations() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Start building your assertion (Note: if an assertion fails the test method execution will stop and fail)
     *
     * @return a ValidationsBuilder Object to start building your validation
     */
    public static Standalone assertThat() {
        return new Standalone(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    /**
     * Start building your verification (Note: if a verification fails the test method execution will continue normally and all failures will be reported at the end)
     *
     * @return a ValidationsBuilder Object to start building your validation
     */
    public static Standalone verifyThat() {
        return new Standalone(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }
}
