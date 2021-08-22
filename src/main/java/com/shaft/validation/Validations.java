package com.shaft.validation;

public class Validations {
    /**
     * Start building your assertion (Note: if an assertion fails the test method execution will stop and fail)
     * @return a ValidationsBuilder Object to start building your validation
     */
    public static ValidationsBuilder assertThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    /**
     * Start building your verification (Note: if a verification fails the test method execution will continue normally and all failures will be reported at the end)
     * @return a ValidationsBuilder Object to start building your validation
     */
    public static ValidationsBuilder verifyThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }
}
