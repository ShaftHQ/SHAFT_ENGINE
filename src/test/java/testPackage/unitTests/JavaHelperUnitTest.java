package testPackage.unitTests;

import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * Unit tests for {@link JavaHelper} static utility methods.
 * Covers string manipulation, object comparison, encoding, and locator formatting.
 */
public class JavaHelperUnitTest {

    // ─── replaceRegex(String[], String) ────────────────────────────────────────

    @Test(description = "replaceRegex with array: single special char should be escaped")
    public void replaceRegexArraySingleCharShouldBeEscaped() {
        String[] chars = {"@"};
        String result = JavaHelper.replaceRegex(chars, "user@example.com");
        Assert.assertEquals(result, "user\\@example.com",
                "Special character '@' should be prefixed with backslash");
    }

    @Test(description = "replaceRegex with array: multiple special chars should all be escaped")
    public void replaceRegexArrayMultipleCharsShouldBeEscaped() {
        String[] chars = {"#", "$"};
        String result = JavaHelper.replaceRegex(chars, "price#$99");
        Assert.assertEquals(result, "price\\#\\$99",
                "Both '#' and '$' should be escaped");
    }

    @Test(description = "replaceRegex with array: text without special chars should be unchanged")
    public void replaceRegexArrayNoSpecialCharsShouldBeUnchanged() {
        String[] chars = {"@"};
        String original = "normalText";
        String result = JavaHelper.replaceRegex(chars, original);
        Assert.assertEquals(result, original,
                "Text without the special char should remain unchanged");
    }

    // ─── replaceRegex(String) ──────────────────────────────────────────────────

    @Test(description = "replaceRegex(String): brackets should be escaped")
    public void replaceRegexStringShouldEscapeBrackets() {
        String result = JavaHelper.replaceRegex("a[0]");
        Assert.assertTrue(result.contains("\\[") && result.contains("\\]"),
                "Square brackets should be escaped");
    }

    @Test(description = "replaceRegex(String): dot should be escaped")
    public void replaceRegexStringShouldEscapeDot() {
        String result = JavaHelper.replaceRegex("a.b");
        Assert.assertTrue(result.contains("\\."),
                "Dot should be escaped");
    }

    @Test(description = "replaceRegex(String): text without regex chars should be unchanged")
    public void replaceRegexStringNormalTextShouldBeUnchanged() {
        String result = JavaHelper.replaceRegex("abc123");
        Assert.assertEquals(result, "abc123",
                "Plain alphanumeric text should not be changed");
    }

    // ─── removeSpecialCharacters ───────────────────────────────────────────────

    @Test(description = "removeSpecialCharacters: special chars replaced with underscore")
    public void removeSpecialCharactersShouldReplaceWithUnderscore() {
        String result = JavaHelper.removeSpecialCharacters("hello@world!");
        Assert.assertEquals(result, "hello_world_",
                "Non-alphanumeric characters should be replaced with '_'");
    }

    @Test(description = "removeSpecialCharacters: plain alphanumeric text should be unchanged")
    public void removeSpecialCharactersAlphanumericShouldBeUnchanged() {
        String result = JavaHelper.removeSpecialCharacters("Test123");
        Assert.assertEquals(result, "Test123",
                "Alphanumeric text should pass through unchanged");
    }

    @Test(description = "removeSpecialCharacters: null input should return empty string")
    public void removeSpecialCharactersNullShouldReturnEmptyString() {
        String result = JavaHelper.removeSpecialCharacters(null);
        Assert.assertEquals(result, "",
                "Null input should produce empty string");
    }

    @Test(description = "removeSpecialCharacters: empty string should return empty string")
    public void removeSpecialCharactersEmptyShouldReturnEmpty() {
        String result = JavaHelper.removeSpecialCharacters("");
        Assert.assertEquals(result, "",
                "Empty input should produce empty string");
    }

    @Test(description = "removeSpecialCharacters: spaces replaced with underscore")
    public void removeSpecialCharactersSpaceShouldBecomeUnderscore() {
        String result = JavaHelper.removeSpecialCharacters("hello world");
        Assert.assertEquals(result, "hello_world",
                "Space should be replaced with '_'");
    }

    // ─── encodeToBase64String ──────────────────────────────────────────────────

    @Test(description = "encodeToBase64String: result should be decodable to original")
    public void encodeToBase64StringShouldBeDecodable() {
        String original = "hello SHAFT";
        String encoded = JavaHelper.encodeToBase64String(original);
        String decoded = new String(Base64.getUrlDecoder().decode(encoded), StandardCharsets.UTF_8);
        Assert.assertEquals(decoded, original,
                "Decoded value should equal the original string");
    }

    @Test(description = "encodeToBase64String: empty string should encode without error")
    public void encodeToBase64StringEmptyShouldSucceed() {
        String encoded = JavaHelper.encodeToBase64String("");
        Assert.assertNotNull(encoded, "Encoding empty string should not return null");
    }

    @Test(description = "encodeToBase64String: different inputs should produce different encodings")
    public void encodeToBase64StringDifferentInputsShouldDiffer() {
        String enc1 = JavaHelper.encodeToBase64String("abc");
        String enc2 = JavaHelper.encodeToBase64String("xyz");
        Assert.assertNotEquals(enc1, enc2,
                "Different inputs must produce different base64 outputs");
    }

    // ─── compareTwoObjects – integer comparison types ─────────────────────────

    @Test(description = "compareTwoObjects: literal equal strings positive → 1")
    public void compareTwoObjectsLiteralEqualPositiveShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects("foo", "foo", 1, true);
        Assert.assertEquals(result, 1, "Matching literal comparison should return 1");
    }

    @Test(description = "compareTwoObjects: literal unequal strings positive → 0")
    public void compareTwoObjectsLiteralUnequalPositiveShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects("foo", "bar", 1, true);
        Assert.assertEquals(result, 0, "Non-matching literal comparison should return 0");
    }

    @Test(description = "compareTwoObjects: literal unequal strings negative → 1")
    public void compareTwoObjectsLiteralUnequalNegativeShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects("foo", "bar", 1, false);
        Assert.assertEquals(result, 1, "Negative assertion on differing strings should return 1");
    }

    @Test(description = "compareTwoObjects: literal equal strings negative → 0")
    public void compareTwoObjectsLiteralEqualNegativeShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects("foo", "foo", 1, false);
        Assert.assertEquals(result, 0, "Negative assertion on equal strings should return 0");
    }

    @Test(description = "compareTwoObjects: regex match positive → 1")
    public void compareTwoObjectsRegexMatchPositiveShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects("[0-9]+", "123", 2, true);
        Assert.assertEquals(result, 1, "Matching regex should return 1");
    }

    @Test(description = "compareTwoObjects: regex non-match positive → 0")
    public void compareTwoObjectsRegexNonMatchPositiveShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects("[0-9]+", "abc", 2, true);
        Assert.assertEquals(result, 0, "Non-matching regex should return 0");
    }

    @Test(description = "compareTwoObjects: contains match positive → 1")
    public void compareTwoObjectsContainsMatchPositiveShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects("SHAFT", "Hello SHAFT World", 3, true);
        Assert.assertEquals(result, 1, "Contains check should return 1 when found");
    }

    @Test(description = "compareTwoObjects: contains non-match positive → 0")
    public void compareTwoObjectsContainsNonMatchPositiveShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects("missing", "Hello World", 3, true);
        Assert.assertEquals(result, 0, "Contains check should return 0 when not found");
    }

    @Test(description = "compareTwoObjects: case-insensitive equal positive → 1")
    public void compareTwoObjectsCaseInsensitiveEqualPositiveShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects("shaft", "SHAFT", 4, true);
        Assert.assertEquals(result, 1, "Case-insensitive equal strings should return 1");
    }

    @Test(description = "compareTwoObjects: case-insensitive unequal positive → 0")
    public void compareTwoObjectsCaseInsensitiveUnequalPositiveShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects("shaft", "selenium", 4, true);
        Assert.assertEquals(result, 0, "Case-insensitive unequal strings should return 0");
    }

    @Test(description = "compareTwoObjects: invalid comparison type → -1")
    public void compareTwoObjectsInvalidComparisonTypeShouldReturnNegativeOne() {
        int result = JavaHelper.compareTwoObjects("foo", "foo", 99, true);
        Assert.assertEquals(result, -1, "Unknown comparison type should return -1");
    }

    @Test(description = "compareTwoObjects: null equals null literal positive → 1")
    public void compareTwoObjectsNullEqualsNullPositiveShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects("null", "null", 1, true);
        Assert.assertEquals(result, 1, "Both null literal strings should match positively");
    }

    @Test(description = "compareTwoObjects: null compared to non-null literal positive → 0")
    public void compareTwoObjectsNullVsNonNullPositiveShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects("null", "value", 1, true);
        Assert.assertEquals(result, 0, "Null vs non-null should fail positive assertion");
    }

    @Test(description = "compareTwoObjects: equal numbers literal positive → 1")
    public void compareTwoObjectsEqualNumbersPositiveShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(42, 42, 1, true);
        Assert.assertEquals(result, 1, "Equal numbers literal comparison should return 1");
    }

    // ─── compareTwoObjects – NumbersComparativeRelation ──────────────────────

    @Test(description = "compareTwoObjects: NumbersComparativeRelation EQUALS match → 1")
    public void compareTwoObjectsNumberEqualsMatchShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(5.0, 5.0,
                ValidationEnums.NumbersComparativeRelation.EQUALS, true);
        Assert.assertEquals(result, 1, "Equal number comparison should return 1");
    }

    @Test(description = "compareTwoObjects: NumbersComparativeRelation GREATER_THAN match → 1")
    public void compareTwoObjectsNumberGreaterThanMatchShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(3.0, 10.0,
                ValidationEnums.NumbersComparativeRelation.GREATER_THAN, true);
        Assert.assertEquals(result, 1, "actual > expected should return 1");
    }

    @Test(description = "compareTwoObjects: NumbersComparativeRelation GREATER_THAN_OR_EQUALS match → 1")
    public void compareTwoObjectsNumberGTEMatchShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(10.0, 10.0,
                ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS, true);
        Assert.assertEquals(result, 1, "actual >= expected should return 1");
    }

    @Test(description = "compareTwoObjects: NumbersComparativeRelation LESS_THAN match → 1")
    public void compareTwoObjectsNumberLessThanMatchShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(10.0, 3.0,
                ValidationEnums.NumbersComparativeRelation.LESS_THAN, true);
        Assert.assertEquals(result, 1, "actual < expected should return 1");
    }

    @Test(description = "compareTwoObjects: NumbersComparativeRelation LESS_THAN_OR_EQUALS match → 1")
    public void compareTwoObjectsNumberLTEMatchShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(5.0, 5.0,
                ValidationEnums.NumbersComparativeRelation.LESS_THAN_OR_EQUALS, true);
        Assert.assertEquals(result, 1, "actual <= expected should return 1");
    }

    @Test(description = "compareTwoObjects: NumbersComparativeRelation EQUALS no-match negative → 1")
    public void compareTwoObjectsNumberEqualsNoMatchNegativeShouldReturnOne() {
        int result = JavaHelper.compareTwoObjects(5.0, 9.0,
                ValidationEnums.NumbersComparativeRelation.EQUALS, false);
        Assert.assertEquals(result, 1, "actual != expected negative assertion should return 1");
    }

    @Test(description = "compareTwoObjects: NumbersComparativeRelation GREATER_THAN no-match → 0")
    public void compareTwoObjectsNumberGreaterThanNoMatchShouldReturnZero() {
        int result = JavaHelper.compareTwoObjects(10.0, 3.0,
                ValidationEnums.NumbersComparativeRelation.GREATER_THAN, true);
        Assert.assertEquals(result, 0, "actual not > expected should return 0");
    }

    // ─── convertToSentenceCase ─────────────────────────────────────────────────

    @Test(description = "convertToSentenceCase: camelCase input produces sentence")
    public void convertToSentenceCaseCamelCaseShouldProduceSentence() {
        String result = JavaHelper.convertToSentenceCase("camelCaseInput");
        Assert.assertFalse(result.isEmpty(), "Result should not be empty");
        // First char should be upper-cased
        Assert.assertTrue(Character.isUpperCase(result.charAt(0)),
                "First character of sentence should be upper-case");
    }

    @Test(description = "convertToSentenceCase: PascalCase input produces sentence")
    public void convertToSentenceCasePascalCaseShouldProduceSentence() {
        String result = JavaHelper.convertToSentenceCase("PascalCaseText");
        Assert.assertFalse(result.isEmpty(), "Result should not be empty");
        Assert.assertTrue(Character.isUpperCase(result.charAt(0)),
                "First character should be upper-case");
    }

    @Test(description = "convertToSentenceCase: single word produces single word sentence")
    public void convertToSentenceCaseSingleWordShouldProduceSingleWord() {
        String result = JavaHelper.convertToSentenceCase("word");
        Assert.assertEquals(result, "Word", "Single lowercase word should be capitalized");
    }

    // ─── formatLocatorToString ─────────────────────────────────────────────────

    @Test(description = "formatLocatorToString: By.id should produce non-null description")
    public void formatLocatorToStringByIdShouldProduceNonNull() {
        String result = JavaHelper.formatLocatorToString(By.id("myId"));
        Assert.assertNotNull(result, "Formatted locator should not be null");
        Assert.assertTrue(result.contains("myId"), "Result should mention the id value");
    }

    @Test(description = "formatLocatorToString: By.xpath should contain xpath in result")
    public void formatLocatorToStringByXpathShouldContainXpath() {
        String result = JavaHelper.formatLocatorToString(By.xpath("//div[@id='x']"));
        Assert.assertNotNull(result, "Formatted locator should not be null");
        Assert.assertTrue(result.contains("xpath") || result.contains("div"),
                "Result should reference the xpath expression");
    }

    @Test(description = "formatLocatorToString: By.cssSelector should be formatted correctly")
    public void formatLocatorToStringByCssShouldBeFormatted() {
        String result = JavaHelper.formatLocatorToString(By.cssSelector(".myClass"));
        Assert.assertNotNull(result, "Formatted locator should not be null");
        Assert.assertTrue(result.contains("myClass"), "Result should contain the CSS class name");
    }
}
