package testPackage.unitTests;

import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link com.shaft.validation.internal.ValidationsBuilder}.
 * Tests the entry points for building different types of validations.
 */
public class ValidationsBuilderUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // --- object() ---

    @Test(description = "object: assertThat with string should pass for equal value")
    public void objectAssertThatStringShouldPass() {
        Validations.assertThat().object("test value").isEqualTo("test value").perform();
    }

    @Test(description = "object: assertThat with integer should pass for equal value")
    public void objectAssertThatIntegerShouldPass() {
        Validations.assertThat().object(100).isEqualTo(100).perform();
    }

    @Test(description = "object: assertThat with null should pass for isNull")
    public void objectAssertThatNullShouldPassForIsNull() {
        Validations.assertThat().object(null).isNull().perform();
    }

    @Test(description = "object: assertThat with boolean true should pass for isTrue")
    public void objectAssertThatBooleanShouldPassForIsTrue() {
        Validations.assertThat().object(true).isTrue().perform();
    }

    @Test(description = "object: assertThat with string should pass for contains")
    public void objectAssertThatStringShouldPassForContains() {
        Validations.assertThat().object("hello world").contains("hello").perform();
    }

    // --- number() ---

    @Test(description = "number: equal integers should pass isEqualTo")
    public void numberEqualIntegersShouldPass() {
        Validations.assertThat().number(42).isEqualTo(42).perform();
    }

    @Test(description = "number: greater than should pass")
    public void numberGreaterThanShouldPass() {
        Validations.assertThat().number(10).isGreaterThan(5).perform();
    }

    @Test(description = "number: less than should pass")
    public void numberLessThanShouldPass() {
        Validations.assertThat().number(5).isLessThan(10).perform();
    }

    @Test(description = "number: greater than or equals should pass for equal")
    public void numberGreaterThanOrEqualsShouldPassForEqual() {
        Validations.assertThat().number(10).isGreaterThanOrEquals(10).perform();
    }

    @Test(description = "number: less than or equals should pass for equal")
    public void numberLessThanOrEqualsShouldPassForEqual() {
        Validations.assertThat().number(10).isLessThanOrEquals(10).perform();
    }

    @Test(description = "number: unequal values should fail isEqualTo")
    public void numberUnequalShouldFail() {
        try {
            Validations.assertThat().number(10).isEqualTo(20).perform();
            Assert.fail("Expected AssertionError for unequal numbers");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "number: double values should pass isEqualTo")
    public void numberDoubleValuesShouldPass() {
        Validations.assertThat().number(3.14).isEqualTo(3.14).perform();
    }

    @Test(description = "number: greater than with smaller value should fail")
    public void numberGreaterThanWithSmallerShouldFail() {
        try {
            Validations.assertThat().number(5).isGreaterThan(10).perform();
            Assert.fail("Expected AssertionError");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- forceFail() ---

    @Test(description = "forceFail: should always throw AssertionError")
    public void forceFailShouldThrow() {
        try {
            Validations.assertThat().forceFail().perform();
            Assert.fail("Expected AssertionError from forceFail");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "forceFail with message: should throw with the provided message")
    public void forceFailWithMessageShouldThrow() {
        try {
            Validations.assertThat().forceFail("Custom failure reason").perform();
            Assert.fail("Expected AssertionError from forceFail with message");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
            Assert.assertTrue(e.getMessage().contains("Custom failure reason"),
                    "AssertionError message should contain the custom failure reason");
        }
    }

    // --- file() builder entry point ---

    @Test(description = "file: non-existent file path should pass doesNotExist validation")
    public void fileBuilderShouldWorkForDoesNotExist() {
        Validations.assertThat()
                .file("target/nonexistent-path/", "nonexistent-file.txt")
                .doesNotExist()
                .perform();
    }

    // --- Soft assertions (verifyThat) ---

    @Test(description = "verifyThat: object equal should pass (soft assert)")
    public void verifyThatObjectEqualShouldPass() {
        Validations.verifyThat().object("abc").isEqualTo("abc").perform();
    }

    @Test(description = "verifyThat: number equal should pass (soft assert)")
    public void verifyThatNumberEqualShouldPass() {
        Validations.verifyThat().number(42).isEqualTo(42).perform();
    }

    @Test(description = "verifyThat: object contains should pass (soft assert)")
    public void verifyThatObjectContainsShouldPass() {
        Validations.verifyThat().object("hello world").contains("world").perform();
    }
}
