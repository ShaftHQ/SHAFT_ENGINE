package testPackage.unitTests;

import com.shaft.api.RestActions;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Unit tests for static utility methods of {@link RestActions} that do
 * NOT require live HTTP requests.  Specifically covers:
 * <ul>
 *   <li>{@link RestActions#formatXML(String)}</li>
 *   <li>{@link RestActions#parseBodyToJson(Object)} with various body types</li>
 *   <li>{@link RestActions.RequestType} enum</li>
 *   <li>{@link RestActions.ComparisonType} enum</li>
 *   <li>{@link RestActions.ParametersType} enum</li>
 * </ul>
 */
public class RestActionsUtilsUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── formatXML ────────────────────────────────────────────────────────────

    @Test(description = "formatXML: well-formed XML returns non-null formatted string")
    public void formatXmlWellFormedInputReturnsNonNullString() {
        String input = "<root><child>value</child></root>";
        String result = RestActions.formatXML(input);
        Assert.assertNotNull(result, "formatXML must not return null for valid XML");
    }

    @Test(description = "formatXML: result contains the original element names")
    public void formatXmlResultContainsOriginalElementNames() {
        String input = "<root><child>hello</child></root>";
        String result = RestActions.formatXML(input);
        Assert.assertTrue(result.contains("root") && result.contains("child"),
                "Formatted XML should still contain the original element names");
    }

    @Test(description = "formatXML: result contains the text content")
    public void formatXmlResultContainsTextContent() {
        String input = "<message>hello world</message>";
        String result = RestActions.formatXML(input);
        Assert.assertTrue(result.contains("hello world"),
                "Formatted XML should preserve the original text content");
    }

    @Test(description = "formatXML: multi-element document is formatted without error")
    public void formatXmlMultiElementDocumentIsFormattedWithoutError() {
        String input = "<catalog><item id='1'><name>Widget</name><price>9.99</price></item></catalog>";
        String result = RestActions.formatXML(input);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.contains("Widget"), "Formatted XML should contain nested element value");
    }

    // ─── parseBodyToJson(Object) ──────────────────────────────────────────────

    @Test(description = "parseBodyToJson: String body returns non-null InputStream")
    public void parseBodyToJsonStringReturnsNonNullInputStream() {
        InputStream result = RestActions.parseBodyToJson("plain text body");
        Assert.assertNotNull(result, "parseBodyToJson must not return null for String input");
    }

    @Test(description = "parseBodyToJson: Map body is serialized to JSON InputStream")
    public void parseBodyToJsonMapReturnsInputStream() {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("key", "value");
        body.put("count", 42);
        InputStream result = RestActions.parseBodyToJson(body);
        Assert.assertNotNull(result, "parseBodyToJson must not return null for Map input");
    }

    @Test(description = "parseBodyToJson: JSON string body returns InputStream with JSON content")
    public void parseBodyToJsonJsonStringReturnsParsableInputStream() throws Exception {
        String json = "{\"name\":\"SHAFT\",\"version\":1}";
        InputStream result = RestActions.parseBodyToJson(json);
        Assert.assertNotNull(result);
        byte[] bytes = result.readAllBytes();
        Assert.assertTrue(bytes.length > 0, "Resulting InputStream must not be empty");
    }

    @Test(description = "parseBodyToJson: byte array body returns InputStream")
    public void parseBodyToJsonByteArrayReturnsInputStream() {
        byte[] body = "binary content".getBytes(StandardCharsets.UTF_8);
        InputStream result = RestActions.parseBodyToJson(body);
        Assert.assertNotNull(result, "parseBodyToJson must not return null for byte[] input");
    }

    // ─── RequestType enum ─────────────────────────────────────────────────────

    @Test(description = "RequestType enum has all 5 expected constants")
    public void requestTypeEnumHasFiveConstants() {
        Assert.assertEquals(RestActions.RequestType.values().length, 5,
                "RequestType must have GET, POST, PATCH, DELETE, PUT");
    }

    @Test(description = "RequestType.GET can be retrieved by name")
    public void requestTypeGetCanBeRetrievedByName() {
        RestActions.RequestType type = RestActions.RequestType.valueOf("GET");
        Assert.assertEquals(type, RestActions.RequestType.GET);
    }

    @Test(description = "RequestType.POST can be retrieved by name")
    public void requestTypePostCanBeRetrievedByName() {
        RestActions.RequestType type = RestActions.RequestType.valueOf("POST");
        Assert.assertEquals(type, RestActions.RequestType.POST);
    }

    @Test(description = "RequestType.DELETE can be retrieved by name")
    public void requestTypeDeleteCanBeRetrievedByName() {
        RestActions.RequestType type = RestActions.RequestType.valueOf("DELETE");
        Assert.assertEquals(type, RestActions.RequestType.DELETE);
    }

    // ─── ComparisonType enum ──────────────────────────────────────────────────

    @Test(description = "ComparisonType.EQUALS can be retrieved by name")
    public void comparisonTypeEqualsCanBeRetrievedByName() {
        RestActions.ComparisonType type = RestActions.ComparisonType.valueOf("EQUALS");
        Assert.assertEquals(type, RestActions.ComparisonType.EQUALS);
    }

    @Test(description = "ComparisonType.CONTAINS can be retrieved by name")
    public void comparisonTypeContainsCanBeRetrievedByName() {
        RestActions.ComparisonType type = RestActions.ComparisonType.valueOf("CONTAINS");
        Assert.assertEquals(type, RestActions.ComparisonType.CONTAINS);
    }

    @Test(description = "ComparisonType enum has at least 3 constants")
    public void comparisonTypeEnumHasAtLeastThreeConstants() {
        Assert.assertTrue(RestActions.ComparisonType.values().length >= 3,
                "ComparisonType must have at least EQUALS, CONTAINS, and EQUALS_IGNORING_ORDER");
    }

    // ─── ParametersType enum ──────────────────────────────────────────────────

    @Test(description = "ParametersType enum has FORM and QUERY constants")
    public void parametersTypeEnumHasFormAndQueryConstants() {
        RestActions.ParametersType form = RestActions.ParametersType.valueOf("FORM");
        RestActions.ParametersType query = RestActions.ParametersType.valueOf("QUERY");
        Assert.assertNotEquals(form, query, "FORM and QUERY must be distinct enum constants");
    }
}
