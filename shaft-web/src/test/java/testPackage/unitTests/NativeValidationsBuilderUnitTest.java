package testPackage.unitTests;

import com.shaft.api.validation.ApiValidations;
import com.shaft.validation.Validations;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsHelper;
import com.shaft.validation.internal.ValidationsExecutor;
import com.shaft.validation.internal.WebDriverBrowserValidationsBuilder;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link com.shaft.validation.internal.NativeValidationsBuilder}.
 * Validates the fluent assertion API for object comparisons.
 */
public class NativeValidationsBuilderUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // --- isEqualTo ---

    @Test(description = "isEqualTo: equal strings should pass")
    public void isEqualToEqualStringsShouldPass() {
        Validations.assertThat().object("hello").isEqualTo("hello").perform();
    }

    @Test(description = "isEqualTo: equal integers should pass")
    public void isEqualToEqualIntegersShouldPass() {
        Validations.assertThat().object(42).isEqualTo(42).perform();
    }

    @Test(description = "isEqualTo: unequal strings should fail with AssertionError")
    public void isEqualToUnequalStringsShouldFail() {
        try {
            Validations.assertThat().object("hello").isEqualTo("world").perform();
            Assert.fail("Expected AssertionError for unequal string assertion");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage(), "AssertionError should carry a message");
        }
    }

    @Test(description = "isEqualTo: null actual with null expected should pass")
    public void isEqualToNullWithNullShouldPass() {
        Validations.assertThat().object(null).isEqualTo(null).perform();
    }

    // --- doesNotEqual ---

    @Test(description = "doesNotEqual: different strings should pass")
    public void doesNotEqualDifferentStringsShouldPass() {
        Validations.assertThat().object("hello").doesNotEqual("world").perform();
    }

    @Test(description = "doesNotEqual: same strings should fail with AssertionError")
    public void doesNotEqualSameStringsShouldFail() {
        try {
            Validations.assertThat().object("hello").doesNotEqual("hello").perform();
            Assert.fail("Expected AssertionError for equal values in doesNotEqual");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- contains ---

    @Test(description = "contains: string containing substring should pass")
    public void containsSubstringShouldPass() {
        Validations.assertThat().object("hello world").contains("world").perform();
    }

    @Test(description = "contains: string not containing substring should fail")
    public void containsMissingSubstringShouldFail() {
        try {
            Validations.assertThat().object("hello").contains("xyz").perform();
            Assert.fail("Expected AssertionError for missing substring");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- doesNotContain ---

    @Test(description = "doesNotContain: string missing substring should pass")
    public void doesNotContainMissingSubstringShouldPass() {
        Validations.assertThat().object("hello").doesNotContain("xyz").perform();
    }

    @Test(description = "doesNotContain: string containing substring should fail")
    public void doesNotContainPresentSubstringShouldFail() {
        try {
            Validations.assertThat().object("hello world").doesNotContain("world").perform();
            Assert.fail("Expected AssertionError when string contains the substring");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- matchesRegex ---

    @Test(description = "matchesRegex: matching pattern should pass")
    public void matchesRegexShouldPass() {
        Validations.assertThat().object("abc123").matchesRegex(".*\\d+.*").perform();
    }

    @Test(description = "matchesRegex: non-matching pattern should fail")
    public void matchesRegexNonMatchingShouldFail() {
        try {
            Validations.assertThat().object("abc").matchesRegex("^\\d+$").perform();
            Assert.fail("Expected AssertionError for non-matching regex");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- doesNotMatchRegex ---

    @Test(description = "doesNotMatchRegex: non-matching pattern should pass")
    public void doesNotMatchRegexNonMatchingShouldPass() {
        Validations.assertThat().object("abc").doesNotMatchRegex("^\\d+$").perform();
    }

    @Test(description = "doesNotMatchRegex: matching pattern should fail")
    public void doesNotMatchRegexMatchingShouldFail() {
        try {
            Validations.assertThat().object("123").doesNotMatchRegex("^\\d+$").perform();
            Assert.fail("Expected AssertionError for matching regex in doesNotMatchRegex");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- equalsIgnoringCaseSensitivity ---

    @Test(description = "equalsIgnoringCaseSensitivity: same text different case should pass")
    public void equalsIgnoringCaseShouldPass() {
        Validations.assertThat().object("Hello").equalsIgnoringCaseSensitivity("HELLO").perform();
    }

    @Test(description = "equalsIgnoringCaseSensitivity: different text should fail")
    public void equalsIgnoringCaseDifferentTextShouldFail() {
        try {
            Validations.assertThat().object("Hello").equalsIgnoringCaseSensitivity("World").perform();
            Assert.fail("Expected AssertionError");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- doesNotEqualIgnoringCaseSensitivity ---

    @Test(description = "doesNotEqualIgnoringCaseSensitivity: different text should pass")
    public void doesNotEqualIgnoringCaseDifferentTextShouldPass() {
        Validations.assertThat().object("Hello").doesNotEqualIgnoringCaseSensitivity("World").perform();
    }

    @Test(description = "doesNotEqualIgnoringCaseSensitivity: same text different case should fail")
    public void doesNotEqualIgnoringCaseSameTextShouldFail() {
        try {
            Validations.assertThat().object("Hello").doesNotEqualIgnoringCaseSensitivity("hello").perform();
            Assert.fail("Expected AssertionError");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- isNull / isNotNull ---

    @Test(description = "isNull: null object should pass")
    public void isNullWithNullShouldPass() {
        Validations.assertThat().object(null).isNull().perform();
    }

    @Test(description = "isNull: non-null object should fail")
    public void isNullWithNonNullShouldFail() {
        try {
            Validations.assertThat().object("notNull").isNull().perform();
            Assert.fail("Expected AssertionError for non-null in isNull");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isNotNull: non-null object should pass")
    public void isNotNullWithNonNullShouldPass() {
        Validations.assertThat().object("notNull").isNotNull().perform();
    }

    @Test(description = "isNotNull: null object should fail")
    public void isNotNullWithNullShouldFail() {
        try {
            Validations.assertThat().object(null).isNotNull().perform();
            Assert.fail("Expected AssertionError for null in isNotNull");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- isTrue / isFalse ---

    @Test(description = "isTrue: true value should pass")
    public void isTrueWithTrueShouldPass() {
        Validations.assertThat().object(true).isTrue().perform();
    }

    @Test(description = "isTrue: false value should fail")
    public void isTrueWithFalseShouldFail() {
        try {
            Validations.assertThat().object(false).isTrue().perform();
            Assert.fail("Expected AssertionError for false in isTrue");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    @Test(description = "isFalse: false value should pass")
    public void isFalseWithFalseShouldPass() {
        Validations.assertThat().object(false).isFalse().perform();
    }

    @Test(description = "isFalse: true value should fail")
    public void isFalseWithTrueShouldFail() {
        try {
            Validations.assertThat().object(true).isFalse().perform();
            Assert.fail("Expected AssertionError for true in isFalse");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }

    // --- Soft assertions (verifyThat) ---

    @Test(description = "verifyThat: isEqualTo with equal values should pass (soft assert)")
    public void verifyThatIsEqualToShouldPass() {
        Validations.verifyThat().object("test").isEqualTo("test").perform();
    }

    @Test(description = "verifyThat: contains with matching value should pass (soft assert)")
    public void verifyThatContainsShouldPass() {
        Validations.verifyThat().object("hello world").contains("hello").perform();
    }

    @Test(description = "language builder should fail outside text assertion context")
    public void languageBuilderOutsideTextContextShouldFail() {
        try {
            Validations.assertThat().object("hello").language().is("en").perform();
            Assert.fail("Expected IllegalStateException when using language() outside text() assertions");
        } catch (IllegalStateException e) {
            Assert.assertTrue(e.getMessage().contains("text()"));
        }
    }

    @Test(description = "direction builder should fail outside text assertion context")
    public void directionBuilderOutsideTextContextShouldFail() {
        try {
            Validations.assertThat().object("hello").direction().isLeftToRight().perform();
            Assert.fail("Expected IllegalStateException when using direction() outside text() assertions");
        } catch (IllegalStateException e) {
            Assert.assertTrue(e.getMessage().contains("text()"));
        }
    }

    @Test(description = "text language builder should be available for browser text assertions")
    public void languageBuilderInsideBrowserTextContextShouldPass() {
        WebDriver driver = Mockito.mock(WebDriver.class);

        NativeValidationsBuilder nativeValidationsBuilder = new WebDriverBrowserValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, driver, new StringBuilder("the browser "))
                .text();

        Assert.assertNotNull(nativeValidationsBuilder.language());
    }

    @Test(description = "text direction-style builders should be available for element text assertions")
    public void directionStyleBuildersInsideElementTextContextShouldPass() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        NativeValidationsBuilder nativeValidationsBuilder = new WebDriverElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, driver, By.id("sample"), new StringBuilder("the element "))
                .text();

        Assert.assertNotNull(nativeValidationsBuilder.direction());
        Assert.assertNotNull(nativeValidationsBuilder.alignment());
        Assert.assertNotNull(nativeValidationsBuilder.orientation());
        Assert.assertNotNull(nativeValidationsBuilder.displayStyle());
    }

    @Test(description = "rest and file fluent entry points should create native builder instances")
    public void restAndFileEntryPointsShouldCreateNativeBuilderInstances() {
        Assert.assertNotNull(ApiValidations.assertThat(new Object()).extractedJsonValue("$.key"));
        Assert.assertNotNull(Validations.assertThat().file("target/", "sample.txt").content());
    }

    @Test(description = "equals override should return true for matching values")
    public void equalsOverrideShouldReturnTrueForMatchingValues() {
        Assert.assertTrue(Validations.assertThat().object("value").equals("value"));
    }

    @Test(description = "equals override should throw AssertionError for mismatched values")
    public void equalsOverrideShouldThrowForMismatchedValues() {
        Assert.expectThrows(AssertionError.class, () -> Validations.assertThat().object("value").equals("different"));
    }

    @Test(description = "isArabic should chain language and direction assertions for text context")
    public void isArabicShouldChainLanguageAndDirectionAssertions() {
        NativeValidationsBuilder nativeValidationsBuilder = new WebDriverBrowserValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, Mockito.mock(WebDriver.class), new StringBuilder("the browser "))
                .text();
        NativeValidationsBuilder nativeValidationsBuilderSpy = Mockito.spy(nativeValidationsBuilder);
        var languageBuilderMock = Mockito.mock(com.shaft.validation.internal.TextLanguageValidationsBuilder.class);
        var directionBuilderMock = Mockito.mock(com.shaft.validation.internal.TextDirectionValidationsBuilder.class);
        var validationsExecutorMock = Mockito.mock(ValidationsExecutor.class);

        Mockito.doReturn(languageBuilderMock).when(nativeValidationsBuilderSpy).language();
        Mockito.doReturn(directionBuilderMock).when(nativeValidationsBuilderSpy).direction();
        Mockito.when(languageBuilderMock.is(ValidationEnums.TextLanguage.ARABIC)).thenReturn(validationsExecutorMock);
        Mockito.when(directionBuilderMock.is(ValidationEnums.TextDirection.RTL)).thenReturn(validationsExecutorMock);

        Assert.assertSame(nativeValidationsBuilderSpy.isArabic(), validationsExecutorMock);
        Mockito.verify(languageBuilderMock).is(ValidationEnums.TextLanguage.ARABIC);
        Mockito.verify(directionBuilderMock).is(ValidationEnums.TextDirection.RTL);
    }

}
