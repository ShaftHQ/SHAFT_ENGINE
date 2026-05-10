package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;

/**
 * Unit tests for {@link PrimitiveValidationsHelper}.
 *
 * <p>These tests verify the helper is reachable from shaft-core scope without dragging in
 * shaft-web (no Selenium), and that the public validation methods run to completion for
 * both matching and mismatching inputs. They do NOT verify the hard-assert AssertionError
 * throw path — that fires through {@code Allure.getLifecycle().updateStep(...)} which is a
 * no-op when there is no active Allure step context (the case in a plain unit test). The
 * end-to-end throw behaviour is exercised by integration tests that go through
 * {@code ValidationsExecutor.internalPerform()} which carries an {@code @Step} annotation —
 * notably {@code shaft-api/.../RestActionsCoverageUnitTest} and the shaft-web validation suite.
 */
public class PrimitiveValidationsHelperTest {

    @Test
    public void validateEqualsWithMatchingValuesShouldRunToCompletion() {
        new PrimitiveValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT)
                .validateEquals("expected", "expected",
                        ValidationEnums.ValidationComparisonType.EQUALS,
                        ValidationEnums.ValidationType.POSITIVE);
    }

    @Test
    public void validateEqualsWithMismatchedValuesShouldRunToCompletion() {
        // The mismatch routes into the failure-reporting path. In a plain unit test (no
        // active Allure step) the FailureReporter.fail invocation inside updateStep silently
        // no-ops. The test asserts that no NullPointerException / ClassNotFoundException /
        // class-loading error escapes the helper.
        new PrimitiveValidationsHelper(ValidationEnums.ValidationCategory.SOFT_ASSERT)
                .validateEquals("expected", "actual",
                        ValidationEnums.ValidationComparisonType.EQUALS,
                        ValidationEnums.ValidationType.POSITIVE);
    }

    @Test
    public void validateNumberWithSatisfiedRelationShouldRunToCompletion() {
        new PrimitiveValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT)
                .validateNumber(10, 5,
                        ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS,
                        ValidationEnums.ValidationType.POSITIVE);
    }

    @Test
    public void validateNumberWithUnsatisfiedRelationShouldRunToCompletion() {
        new PrimitiveValidationsHelper(ValidationEnums.ValidationCategory.SOFT_ASSERT)
                .validateNumber(5, 10,
                        ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS,
                        ValidationEnums.ValidationType.POSITIVE);
    }

    @Test
    public void primitiveValidationsHelperShouldNotPullInSelenium() {
        assertFalse(com.shaft.tools.internal.support.JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
                "WebDriver must NOT be on shaft-core test classpath — shaft-web is not a dependency");
    }
}
