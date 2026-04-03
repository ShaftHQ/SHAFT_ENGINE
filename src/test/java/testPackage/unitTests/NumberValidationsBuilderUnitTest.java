package testPackage.unitTests;

import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@code NumberValidationsBuilder} number comparison methods.
 *
 * <p>Each test exercises a distinct comparison method via the public
 * {@link Validations#assertThat()} API without requiring a browser or
 * external service.  Failure paths are verified by catching the expected
 * {@link AssertionError}.
 */
public class NumberValidationsBuilderUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── isEqualTo ─────────────────────────────────────────────────────────────

    @Test(description = "isEqualTo: equal integers should pass without throwing")
    public void isEqualToEqualIntegersShouldPass() {
        Validations.assertThat().number(42).isEqualTo(42).perform();
    }

    @Test(description = "isEqualTo: equal doubles should pass without throwing")
    public void isEqualToEqualDoublesShouldPass() {
        Validations.assertThat().number(3.14).isEqualTo(3.14).perform();
    }

    @Test(description = "isEqualTo: unequal numbers should throw AssertionError")
    public void isEqualToUnequalNumbersShouldThrowAssertionError() {
        try {
            Validations.assertThat().number(1).isEqualTo(2).perform();
            Assert.fail("Expected AssertionError for unequal number assertion");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage(), "AssertionError should carry a message");
        }
    }

    @Test(description = "isEqualTo: zero equals zero should pass")
    public void isEqualToZeroEqualsZeroShouldPass() {
        Validations.assertThat().number(0).isEqualTo(0).perform();
    }

    @Test(description = "isEqualTo: negative numbers that match should pass")
    public void isEqualToNegativeMatchShouldPass() {
        Validations.assertThat().number(-7).isEqualTo(-7).perform();
    }

    // ─── doesNotEqual ──────────────────────────────────────────────────────────

    @Test(description = "doesNotEqual: differing values should pass without throwing")
    public void doesNotEqualDifferingValuesShouldPass() {
        Validations.assertThat().number(5).doesNotEqual(99).perform();
    }

    @Test(description = "doesNotEqual: same values should throw AssertionError")
    public void doesNotEqualSameValueShouldThrowAssertionError() {
        try {
            Validations.assertThat().number(7).doesNotEqual(7).perform();
            Assert.fail("Expected AssertionError for doesNotEqual with matching numbers");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage(), "AssertionError should carry a message");
        }
    }

    @Test(description = "doesNotEqual: zero vs non-zero should pass")
    public void doesNotEqualZeroVsNonZeroShouldPass() {
        Validations.assertThat().number(0).doesNotEqual(1).perform();
    }

    // ─── isGreaterThan ─────────────────────────────────────────────────────────

    @Test(description = "isGreaterThan: larger actual value should pass")
    public void isGreaterThanLargerActualShouldPass() {
        Validations.assertThat().number(10).isGreaterThan(5).perform();
    }

    @Test(description = "isGreaterThan: equal values should throw AssertionError")
    public void isGreaterThanEqualValuesShouldThrow() {
        try {
            Validations.assertThat().number(5).isGreaterThan(5).perform();
            Assert.fail("Expected AssertionError for isGreaterThan with equal values");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isGreaterThan: smaller actual value should throw AssertionError")
    public void isGreaterThanSmallerActualShouldThrow() {
        try {
            Validations.assertThat().number(3).isGreaterThan(10).perform();
            Assert.fail("Expected AssertionError for isGreaterThan where actual < expected");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isGreaterThan: positive vs negative should pass")
    public void isGreaterThanPositiveVsNegativeShouldPass() {
        Validations.assertThat().number(1).isGreaterThan(-1).perform();
    }

    // ─── isGreaterThanOrEquals ─────────────────────────────────────────────────

    @Test(description = "isGreaterThanOrEquals: equal values should pass")
    public void isGreaterThanOrEqualsEqualValuesShouldPass() {
        Validations.assertThat().number(5).isGreaterThanOrEquals(5).perform();
    }

    @Test(description = "isGreaterThanOrEquals: larger actual should pass")
    public void isGreaterThanOrEqualsLargerActualShouldPass() {
        Validations.assertThat().number(10).isGreaterThanOrEquals(5).perform();
    }

    @Test(description = "isGreaterThanOrEquals: smaller actual should throw AssertionError")
    public void isGreaterThanOrEqualsSmallerActualShouldThrow() {
        try {
            Validations.assertThat().number(3).isGreaterThanOrEquals(10).perform();
            Assert.fail("Expected AssertionError for isGreaterThanOrEquals where actual < expected");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isGreaterThanOrEquals: zero vs zero should pass")
    public void isGreaterThanOrEqualsZeroVsZeroShouldPass() {
        Validations.assertThat().number(0).isGreaterThanOrEquals(0).perform();
    }

    // ─── isLessThan ────────────────────────────────────────────────────────────

    @Test(description = "isLessThan: smaller actual value should pass")
    public void isLessThanSmallerActualShouldPass() {
        Validations.assertThat().number(3).isLessThan(10).perform();
    }

    @Test(description = "isLessThan: equal values should throw AssertionError")
    public void isLessThanEqualValuesShouldThrow() {
        try {
            Validations.assertThat().number(5).isLessThan(5).perform();
            Assert.fail("Expected AssertionError for isLessThan with equal values");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isLessThan: larger actual value should throw AssertionError")
    public void isLessThanLargerActualShouldThrow() {
        try {
            Validations.assertThat().number(10).isLessThan(3).perform();
            Assert.fail("Expected AssertionError for isLessThan where actual > expected");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isLessThan: negative vs positive should pass")
    public void isLessThanNegativeVsPositiveShouldPass() {
        Validations.assertThat().number(-1).isLessThan(1).perform();
    }

    // ─── isLessThanOrEquals ────────────────────────────────────────────────────

    @Test(description = "isLessThanOrEquals: equal values should pass")
    public void isLessThanOrEqualsEqualValuesShouldPass() {
        Validations.assertThat().number(5).isLessThanOrEquals(5).perform();
    }

    @Test(description = "isLessThanOrEquals: smaller actual should pass")
    public void isLessThanOrEqualsSmallerActualShouldPass() {
        Validations.assertThat().number(2).isLessThanOrEquals(10).perform();
    }

    @Test(description = "isLessThanOrEquals: larger actual should throw AssertionError")
    public void isLessThanOrEqualsLargerActualShouldThrow() {
        try {
            Validations.assertThat().number(10).isLessThanOrEquals(3).perform();
            Assert.fail("Expected AssertionError for isLessThanOrEquals where actual > expected");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isLessThanOrEquals: zero vs zero should pass")
    public void isLessThanOrEqualsZeroVsZeroShouldPass() {
        Validations.assertThat().number(0).isLessThanOrEquals(0).perform();
    }

    // ─── soft assertions (verifyThat) ─────────────────────────────────────────

    @Test(description = "verifyThat number isEqualTo: equal numbers should not accumulate failure")
    public void verifyThatNumberIsEqualToEqualNumbersShouldNotFail() {
        Validations.verifyThat().number(100).isEqualTo(100).perform();
        AssertionError err = ValidationsHelper.getVerificationErrorToForceFail();
        Assert.assertNull(err, "Soft assertion on equal numbers should not store a failure");
    }

    @Test(description = "verifyThat number isGreaterThan: passing check should not accumulate failure")
    public void verifyThatNumberIsGreaterThanPassingShouldNotFail() {
        Validations.verifyThat().number(99).isGreaterThan(1).perform();
        AssertionError err = ValidationsHelper.getVerificationErrorToForceFail();
        Assert.assertNull(err, "Passing soft greater-than assertion should not store a failure");
    }

    @Test(description = "verifyThat number isEqualTo: failing check should accumulate failure")
    public void verifyThatNumberIsEqualToFailingShouldAccumulateFailure() {
        Validations.verifyThat().number(1).isEqualTo(2).perform();
        AssertionError err = ValidationsHelper.getVerificationErrorToForceFail();
        Assert.assertNotNull(err, "Failing soft assertion should store a verification error");
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── boundary / precision edge cases ──────────────────────────────────────

    @Test(description = "isEqualTo: long max value should equal itself")
    public void isEqualToLongMaxValueShouldPass() {
        Validations.assertThat().number(Long.MAX_VALUE).isEqualTo(Long.MAX_VALUE).perform();
    }

    @Test(description = "isGreaterThan: large difference should pass quickly")
    public void isGreaterThanLargeDifferenceShouldPass() {
        Validations.assertThat().number(1_000_000).isGreaterThan(1).perform();
    }
}
