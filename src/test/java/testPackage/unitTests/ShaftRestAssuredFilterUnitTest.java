package testPackage.unitTests;

import com.shaft.api.ShaftRestAssuredFilter;
import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link ShaftRestAssuredFilter} — the SHAFT-native REST Assured filter
 * that replaces the default {@code AllureRestAssured} HTML-template-based filter.
 *
 * <p>Tests cover the helper methods responsible for MIME-type detection, JSON
 * pretty-printing, and file-extension mapping.  Live HTTP requests are NOT made;
 * these are pure unit tests driven by string inputs.
 */
public class ShaftRestAssuredFilterUnitTest {

    private ShaftRestAssuredFilter filter;

    @BeforeMethod
    public void setUp() {
        filter = new ShaftRestAssuredFilter();
    }

    // ─── looksLikeJson ────────────────────────────────────────────────────────

    @Test(description = "looksLikeJson returns true for JSON object")
    public void looksLikeJsonShouldReturnTrueForJsonObject() {
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("{\"key\":\"value\"}"))
                .isEqualTo(true).perform();
    }

    @Test(description = "looksLikeJson returns true for JSON array")
    public void looksLikeJsonShouldReturnTrueForJsonArray() {
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("[1,2,3]"))
                .isEqualTo(true).perform();
    }

    @Test(description = "looksLikeJson returns true for JSON with leading/trailing whitespace")
    public void looksLikeJsonShouldHandleWhitespace() {
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("  {\"key\":\"value\"}  "))
                .isEqualTo(true).perform();
    }

    @Test(description = "looksLikeJson returns false for plain text")
    public void looksLikeJsonShouldReturnFalseForPlainText() {
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("plain text response"))
                .isEqualTo(false).perform();
    }

    @Test(description = "looksLikeJson returns false for XML content")
    public void looksLikeJsonShouldReturnFalseForXml() {
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("<root><item>test</item></root>"))
                .isEqualTo(false).perform();
    }

    @Test(description = "looksLikeJson returns false for empty string")
    public void looksLikeJsonShouldReturnFalseForEmptyString() {
        SHAFT.Validations.assertThat().object(filter.looksLikeJson(""))
                .isEqualTo(false).perform();
    }

    // ─── looksLikeXml ────────────────────────────────────────────────────────

    @Test(description = "looksLikeXml returns true for XML document")
    public void looksLikeXmlShouldReturnTrueForXml() {
        SHAFT.Validations.assertThat().object(filter.looksLikeXml("<root><item>test</item></root>"))
                .isEqualTo(true).perform();
    }

    @Test(description = "looksLikeXml returns false for JSON")
    public void looksLikeXmlShouldReturnFalseForJson() {
        SHAFT.Validations.assertThat().object(filter.looksLikeXml("{\"key\":\"value\"}"))
                .isEqualTo(false).perform();
    }

    @Test(description = "looksLikeXml returns false for plain text")
    public void looksLikeXmlShouldReturnFalseForPlainText() {
        SHAFT.Validations.assertThat().object(filter.looksLikeXml("plain text"))
                .isEqualTo(false).perform();
    }

    // ─── detectContentType ───────────────────────────────────────────────────

    @Test(description = "detectContentType returns application/json when declared content-type contains json")
    public void detectContentTypeShouldReturnApplicationJsonForJsonHeader() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("{\"key\":\"value\"}", "application/json; charset=utf-8"))
                .isEqualTo("application/json").perform();
    }

    @Test(description = "detectContentType returns text/xml when declared content-type contains xml")
    public void detectContentTypeShouldReturnTextXmlForXmlHeader() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("<root/>", "text/xml"))
                .isEqualTo("text/xml").perform();
    }

    @Test(description = "detectContentType returns text/csv when declared content-type contains csv")
    public void detectContentTypeShouldReturnTextCsvForCsvHeader() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("a,b,c", "text/csv"))
                .isEqualTo("text/csv").perform();
    }

    @Test(description = "detectContentType sniffs JSON body even when content-type declares text/html")
    public void detectContentTypeShouldSniffJsonBodyWhenDeclaredAsHtml() {
        // Many APIs return application/json content with text/html content-type header
        String jsonBody = "{\"responseCode\":200,\"products\":[]}";
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType(jsonBody, "text/html; charset=utf-8"))
                .isEqualTo("application/json").perform();
    }

    @Test(description = "detectContentType returns text/html for genuine HTML when declared as html")
    public void detectContentTypeShouldReturnTextHtmlForGenuineHtml() {
        String htmlBody = "<html><body><p>Hello</p></body></html>";
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType(htmlBody, "text/html"))
                .isEqualTo("text/html").perform();
    }

    @Test(description = "detectContentType sniffs JSON from body when content-type is absent")
    public void detectContentTypeShouldSniffJsonBodyWhenNoHeader() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("{\"id\":1}", ""))
                .isEqualTo("application/json").perform();
    }

    @Test(description = "detectContentType sniffs XML from body when content-type is absent")
    public void detectContentTypeShouldSniffXmlBodyWhenNoHeader() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("<root><item>1</item></root>", ""))
                .isEqualTo("text/xml").perform();
    }

    @Test(description = "detectContentType falls back to text/plain for unknown content")
    public void detectContentTypeShouldFallBackToTextPlain() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("some random text", ""))
                .isEqualTo("text/plain").perform();
    }

    @Test(description = "detectContentType returns text/plain for null content-type")
    public void detectContentTypeShouldHandleNullDeclaredType() {
        SHAFT.Validations.assertThat()
                .object(filter.detectContentType("some text", null))
                .isEqualTo("text/plain").perform();
    }

    // ─── formatBody ──────────────────────────────────────────────────────────

    @Test(description = "formatBody pretty-prints compact JSON")
    public void formatBodyShouldPrettyPrintJson() {
        String compact = "{\"key\":\"value\",\"num\":42}";
        String formatted = filter.formatBody(compact, "application/json");
        // Pretty-printed JSON should contain newlines
        SHAFT.Validations.assertThat().object(formatted).contains("\n").perform();
        // And indentation
        SHAFT.Validations.assertThat().object(formatted).contains("  ").perform();
    }

    @Test(description = "formatBody returns original content for non-JSON content types")
    public void formatBodyShouldReturnOriginalForNonJson() {
        String xml = "<root><item>test</item></root>";
        String result = filter.formatBody(xml, "text/xml");
        SHAFT.Validations.assertThat().object(result).isEqualTo(xml).perform();
    }

    @Test(description = "formatBody returns original string when JSON parsing fails")
    public void formatBodyShouldReturnOriginalWhenJsonParsingFails() {
        String invalidJson = "not valid json { broken";
        String result = filter.formatBody(invalidJson, "application/json");
        SHAFT.Validations.assertThat().object(result).isEqualTo(invalidJson).perform();
    }

    // ─── getFileExtension ────────────────────────────────────────────────────

    @Test(description = "getFileExtension returns .json for application/json")
    public void getFileExtensionShouldReturnDotJsonForApplicationJson() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("application/json")).isEqualTo(".json").perform();
    }

    @Test(description = "getFileExtension returns .xml for text/xml")
    public void getFileExtensionShouldReturnDotXmlForTextXml() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("text/xml")).isEqualTo(".xml").perform();
    }

    @Test(description = "getFileExtension returns .xml for application/xml")
    public void getFileExtensionShouldReturnDotXmlForApplicationXml() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("application/xml")).isEqualTo(".xml").perform();
    }

    @Test(description = "getFileExtension returns .csv for text/csv")
    public void getFileExtensionShouldReturnDotCsvForTextCsv() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("text/csv")).isEqualTo(".csv").perform();
    }

    @Test(description = "getFileExtension returns .html for text/html")
    public void getFileExtensionShouldReturnDotHtmlForTextHtml() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("text/html")).isEqualTo(".html").perform();
    }

    @Test(description = "getFileExtension returns .txt for text/plain and unknown types")
    public void getFileExtensionShouldReturnDotTxtForTextPlainAndUnknown() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("text/plain")).isEqualTo(".txt").perform();
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("application/octet-stream")).isEqualTo(".txt").perform();
    }
}
