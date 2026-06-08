package com.shaft.validation;

import com.shaft.validation.internal.ValidationsBuilder;

/**
 * Entry point for building assertions and verifications in SHAFT tests.
 *
 * <p>Use {@link #assertThat()} to create a <em>hard assertion</em> that stops
 * the test on failure, or {@link #verifyThat()} for a <em>soft assertion</em>
 * that collects failures and reports them at the end of the test.
 *
 * <p><b>Usage example:</b>
 * <pre>{@code
 * Validations.assertThat().object(actual).isEqualTo(expected).perform();
 * Validations.verifyThat().number(count).isGreaterThan(0).perform();
 * }</pre>
 *
 * @see com.shaft.driver.SHAFT.Validations
 * @see <a href="https://shafthq.github.io/">SHAFT User Guide &ndash; Validations</a>
 */
public class Validations {
    private Validations() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Start building your assertion (Note: if an assertion fails the test method execution will stop and fail)
     *
     * @return a ValidationsBuilder Object to start building your validation
     */
    public static ValidationsBuilder assertThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    /**
     * Start building your verification (Note: if a verification fails the test method execution will continue normally and all failures will be reported at the end)
     *
     * @return a ValidationsBuilder Object to start building your validation
     */
    public static ValidationsBuilder verifyThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }
}
