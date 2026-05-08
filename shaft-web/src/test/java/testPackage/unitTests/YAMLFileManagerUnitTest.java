package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

/**
 * Unit tests for {@link com.shaft.tools.io.YAMLFileManager} via the {@link SHAFT.TestData.YAML} facade.
 * Exercises all typed accessor methods, nested key paths, list indexing, and error paths
 * using the pre-existing test data file at
 * {@code src/test/resources/testDataFiles/yaml/yaml_test_data.yaml}.
 */
public class YAMLFileManagerUnitTest {

    private SHAFT.TestData.YAML yaml;

    @BeforeClass
    public void setUp() {
        yaml = new SHAFT.TestData.YAML("src/test/resources/testDataFiles/yaml/yaml_test_data.yaml");
    }

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── getTestData (returns String for any type) ────────────────────────────

    @Test(description = "getTestData: integer key returns string representation")
    public void getTestDataIntegerReturnsStringRepresentation() {
        Validations.assertThat().object(yaml.getTestData("integer")).isEqualTo("10").perform();
    }

    @Test(description = "getTestData: string key returns the plain string value")
    public void getTestDataStringKeyReturnsPlainStringValue() {
        Validations.assertThat().object(yaml.getTestData("string")).isEqualTo("text").perform();
    }

    @Test(description = "getTestData: boolean key returns 'true'")
    public void getTestDataBooleanKeyReturnsTrueString() {
        Validations.assertThat().object(yaml.getTestData("boolean")).isEqualTo("true").perform();
    }

    @Test(description = "getTestData: null key (null0) returns null string")
    public void getTestDataNullKeyReturnsNull() {
        Assert.assertNull(yaml.getTestData("null0"), "getTestData for null value should return null");
    }

    // ─── get(key) ─────────────────────────────────────────────────────────────

    @Test(description = "get: object key returns the raw value")
    public void getObjectKeyReturnsRawValue() {
        Validations.assertThat().object(yaml.get("object")).isEqualTo("object").perform();
    }

    @Test(description = "get: null1 key returns null")
    public void getNullKeyReturnsNull() {
        Validations.assertThat().object(yaml.get("null1")).isNull().perform();
    }

    // ─── getString ────────────────────────────────────────────────────────────

    @Test(description = "getString: string key returns the correct string")
    public void getStringReturnsCorrectValue() {
        Validations.assertThat().object(yaml.getString("string")).isEqualTo("text").perform();
    }

    @Test(description = "getString: long-text key returns multi-line folded string")
    public void getStringLongTextReturnsFoldedContent() {
        String expected = "this is not a normal string it spans more than one line see?\n";
        Validations.assertThat().object(yaml.getString("long-text")).isEqualTo(expected).perform();
    }

    @Test(description = "getString: long-text2 returns literal block string")
    public void getStringLongText2ReturnsLiteralContent() {
        String expected = "this is not a normal string it\nspans more than\none line\nsee?\n";
        Validations.assertThat().object(yaml.getString("long-text2")).isEqualTo(expected).perform();
    }

    // ─── getInteger ───────────────────────────────────────────────────────────

    @Test(description = "getInteger: integer key returns Integer value")
    public void getIntegerReturnsCorrectValue() {
        Validations.assertThat().object(yaml.getInteger("integer")).isEqualTo(10).perform();
    }

    @Test(description = "getInteger: hex-decimal key is parsed correctly")
    public void getIntegerHexDecimalIsParsed() {
        Validations.assertThat().object(yaml.getInteger("hex-decimal")).isEqualTo(4820).perform();
    }

    @Test(description = "getInteger: octal key is parsed correctly")
    public void getIntegerOctalIsParsed() {
        Validations.assertThat().object(yaml.getInteger("octal")).isEqualTo(9946).perform();
    }

    // ─── getDouble ────────────────────────────────────────────────────────────

    @Test(description = "getDouble: double key returns correct value")
    public void getDoubleReturnsCorrectValue() {
        Validations.assertThat().object(yaml.getDouble("double")).isEqualTo(5.3).perform();
    }

    @Test(description = "getDouble: exponential notation is parsed")
    public void getDoubleExponentialIsParsed() {
        Validations.assertThat().object(yaml.getDouble("exponential")).isEqualTo(1230150.0).perform();
    }

    @Test(description = "getDouble: .inf returns positive infinity")
    public void getDoubleInfinityReturnsPositiveInfinity() {
        Validations.assertThat().object(yaml.getDouble("infinity")).isEqualTo(Double.POSITIVE_INFINITY).perform();
    }

    @Test(description = "getDouble: -.Inf returns negative infinity")
    public void getDoubleNegativeInfinityReturnsNegativeInfinity() {
        Validations.assertThat().object(yaml.getDouble("negative-infinity")).isEqualTo(Double.NEGATIVE_INFINITY).perform();
    }

    @Test(description = "getDouble: .NAN returns NaN")
    public void getDoubleNaNReturnsNaN() {
        Double result = yaml.getDouble("not-a-number");
        Assert.assertTrue(result.isNaN(), "Expected NaN");
    }

    // ─── getLong ─────────────────────────────────────────────────────────────

    @Test(description = "getLong: long key with trailing L returns correct Long")
    public void getLongReturnsCorrectValue() {
        Validations.assertThat().object(yaml.getLong("long")).isEqualTo(10L).perform();
    }

    @Test(description = "getLong: non-long value throws RuntimeException")
    public void getLongWithNonLongValueThrowsException() {
        Assert.assertThrows(RuntimeException.class, () -> yaml.getLong("invalid-long"));
    }

    // ─── getBoolean ──────────────────────────────────────────────────────────

    @Test(description = "getBoolean: true value returns true")
    public void getBooleanTrueValueReturnsTrue() {
        Validations.assertThat().object(yaml.getBoolean("boolean")).isTrue().perform();
    }

    @Test(description = "getBoolean: false (off) value returns false")
    public void getBooleanFalseValueReturnsFalse() {
        Validations.assertThat().object(yaml.getBoolean("boolean1")).isFalse().perform();
    }

    // ─── getAs ────────────────────────────────────────────────────────────────

    @Test(description = "getAs: String class returns the correct string")
    public void getAsStringClassReturnsCorrectValue() {
        Validations.assertThat().object(yaml.getAs("string", String.class)).isEqualTo("text").perform();
    }

    @Test(description = "getAs: mismatched class throws RuntimeException")
    public void getAsMismatchedClassThrowsException() {
        Assert.assertThrows(RuntimeException.class, () -> yaml.getAs("integer", Boolean.class));
    }

    // ─── getListAs ────────────────────────────────────────────────────────────

    @Test(description = "getListAs: list key returns list of strings")
    public void getListAsReturnsCorrectList() {
        List<String> result = yaml.getListAs("list", String.class);
        Validations.assertThat().object(result).isEqualTo(List.of("l1", "l2")).perform();
    }

    // ─── getMapAs ─────────────────────────────────────────────────────────────

    @Test(description = "getMapAs: map key returns correct map")
    public void getMapAsReturnsCorrectMap() {
        Map<String, String> result = yaml.getMapAs("map", String.class);
        Validations.assertThat().object(result).isEqualTo(Map.of("m1", "m1", "m2", "m2")).perform();
    }

    // ─── get() (full map) ─────────────────────────────────────────────────────

    @Test(description = "get(): returns non-null map containing all top-level keys")
    public void getAllDataReturnsNonNullMap() {
        var allData = yaml.get();
        Assert.assertNotNull(allData, "Full data map must not be null");
        Assert.assertTrue(allData.containsKey("string"), "Data map should contain 'string' key");
        Assert.assertTrue(allData.containsKey("integer"), "Data map should contain 'integer' key");
    }

    // ─── nested key paths ────────────────────────────────────────────────────

    @Test(description = "get: nested map accessed with dot notation")
    public void getNestedMapWithDotNotation() {
        Validations.assertThat().object(yaml.getString("nested-map.a.b.c.d"))
                .isEqualTo("I covered by maps:D").perform();
    }

    @Test(description = "get: nested list accessed with index notation")
    public void getNestedListWithIndexNotation() {
        Validations.assertThat().object(yaml.getString("nested-list[0][0][0][0][0][0]"))
                .isEqualTo("You caught me").perform();
    }

    @Test(description = "get: mixed map and list path")
    public void getMixedMapAndListPath() {
        Validations.assertThat().object(yaml.getString("mix-map-list.m1[1].l1.m2[1].l3"))
                .isEqualTo("HOW DID YOU FIND ME!!!").perform();
    }

    // ─── error paths ─────────────────────────────────────────────────────────

    @Test(description = "get: non-existent key throws RuntimeException")
    public void getNonExistentKeyThrowsException() {
        Assert.assertThrows(RuntimeException.class, () -> yaml.get("non-existent-key-xyz"));
    }
}
