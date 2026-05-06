package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.JSONFileManager;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

/**
 * Unit tests for {@link JSONFileManager} via the {@link SHAFT.TestData.JSON} facade.
 * Covers {@code getTestData}, {@code get}, {@code getTestDataAsJson},
 * {@code getTestDataAsList}, and {@code getTestDataAsMap} using the bundled
 * {@code jsonFileManagerTestData.json} file.
 */
public class JSONFileManagerUnitTest {

    private SHAFT.TestData.JSON jsonData;

    @BeforeClass
    public void setUp() {
        jsonData = new SHAFT.TestData.JSON("jsonFileManagerTestData.json");
    }

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── getTestData (string) ─────────────────────────────────────────────────

    @Test(description = "getTestData: top-level string key returns correct value")
    public void getTestDataTopLevelStringKeyReturnsCorrectValue() {
        String title = jsonData.getTestData("title");
        Validations.assertThat().object(title).isEqualTo("JSONFileManager Unit Test Data").perform();
    }

    @Test(description = "getTestData: integer value returns its string representation")
    public void getTestDataIntegerValueReturnsStringRepresentation() {
        String version = jsonData.getTestData("version");
        Validations.assertThat().object(version).isEqualTo("1").perform();
    }

    @Test(description = "getTestData: boolean value returns its string representation")
    public void getTestDataBooleanValueReturnsStringRepresentation() {
        String active = jsonData.getTestData("active");
        Validations.assertThat().object(active).isEqualTo("true").perform();
    }

    @Test(description = "getTestData: nested key accessed via dot notation")
    public void getTestDataNestedKeyViaDotNotation() {
        String timeout = jsonData.getTestData("config.timeout");
        Validations.assertThat().object(timeout).isEqualTo("30").perform();
    }

    @Test(description = "getTestData: nested string key under config object")
    public void getTestDataNestedStringKeyUnderConfigObject() {
        String baseUrl = jsonData.getTestData("config.baseUrl");
        Validations.assertThat().object(baseUrl).isEqualTo("https://example.com").perform();
    }

    @Test(description = "getTestData: JSONPath with x. prefix is cleaned correctly")
    public void getTestDataWithXDotPrefixIsCleanedCorrectly() {
        // x.title should be normalized to title
        String title = jsonData.getTestData("x.title");
        Validations.assertThat().object(title).isEqualTo("JSONFileManager Unit Test Data").perform();
    }

    // ─── get() alias ──────────────────────────────────────────────────────────

    @Test(description = "get: alias returns the same value as getTestData")
    public void getAliasReturnsSameValueAsGetTestData() {
        String via_getTestData = jsonData.getTestData("title");
        String via_get = jsonData.get("title");
        Validations.assertThat().object(via_get).isEqualTo(via_getTestData).perform();
    }

    @Test(description = "get: works for nested key")
    public void getAliasWorksForNestedKey() {
        String retries = jsonData.get("config.retries");
        Validations.assertThat().object(retries).isEqualTo("3").perform();
    }

    // ─── getTestDataAsList ────────────────────────────────────────────────────

    @Test(description = "getTestDataAsList: list key returns non-null list")
    public void getTestDataAsListReturnsNonNullList() {
        List<?> tags = jsonData.getTestDataAsList("tags");
        Assert.assertNotNull(tags, "getTestDataAsList must not return null for a list key");
        Assert.assertFalse(tags.isEmpty(), "tags list must not be empty");
    }

    @Test(description = "getTestDataAsList: list contains expected values")
    public void getTestDataAsListContainsExpectedValues() {
        List<?> tags = jsonData.getTestDataAsList("tags");
        Assert.assertTrue(tags.contains("java"), "Tags list should contain 'java'");
        Assert.assertTrue(tags.contains("testing"), "Tags list should contain 'testing'");
        Assert.assertTrue(tags.contains("shaft"), "Tags list should contain 'shaft'");
    }

    @Test(description = "getTestDataAsList: array of objects returns correct count")
    public void getTestDataAsListObjectArrayReturnsCorrectCount() {
        List<?> users = jsonData.getTestDataAsList("users");
        Assert.assertNotNull(users, "users list must not be null");
        Assert.assertEquals(users.size(), 2, "users list should have 2 entries");
    }

    @Test(description = "getTestDataAsList: numeric list returns non-empty list")
    public void getTestDataAsListNumericListReturnsNonEmptyList() {
        List<?> scores = jsonData.getTestDataAsList("scores");
        Assert.assertNotNull(scores, "scores list must not be null");
        Assert.assertEquals(scores.size(), 3, "scores list should have 3 entries");
    }

    // ─── getTestDataAsMap ─────────────────────────────────────────────────────

    @Test(description = "getTestDataAsMap: object key returns non-null map")
    public void getTestDataAsMapReturnsNonNullMap() {
        Map<?, ?> config = jsonData.getTestDataAsMap("config");
        Assert.assertNotNull(config, "getTestDataAsMap must not return null for an object key");
        Assert.assertFalse(config.isEmpty(), "config map must not be empty");
    }

    @Test(description = "getTestDataAsMap: map contains expected keys")
    public void getTestDataAsMapContainsExpectedKeys() {
        Map<?, ?> config = jsonData.getTestDataAsMap("config");
        Assert.assertTrue(config.containsKey("timeout"), "config map should contain 'timeout'");
        Assert.assertTrue(config.containsKey("retries"), "config map should contain 'retries'");
        Assert.assertTrue(config.containsKey("baseUrl"), "config map should contain 'baseUrl'");
    }

    @Test(description = "getTestDataAsMap: map values are accessible")
    public void getTestDataAsMapValuesAreAccessible() {
        Map<?, ?> config = jsonData.getTestDataAsMap("config");
        Object timeout = config.get("timeout");
        Assert.assertNotNull(timeout, "timeout value in config map must not be null");
    }

    // ─── getTestDataAsJson ────────────────────────────────────────────────────

    @Test(description = "getTestDataAsJson: object key returns non-null JSON object")
    public void getTestDataAsJsonObjectKeyReturnsNonNull() {
        Object jsonObject = jsonData.getTestDataAsJson("config");
        Assert.assertNotNull(jsonObject, "getTestDataAsJson must not return null for an object key");
    }

    @Test(description = "getTestDataAsJson: primitive key returns a value")
    public void getTestDataAsJsonPrimitiveKeyReturnsValue() {
        Object jsonValue = jsonData.getTestDataAsJson("version");
        Assert.assertNotNull(jsonValue, "getTestDataAsJson must not return null for a primitive key");
    }
}
