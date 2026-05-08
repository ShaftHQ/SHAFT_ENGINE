package testPackage.unitTests;

import com.shaft.api.ShaftRestAssuredFilter;
import com.shaft.driver.SHAFT;
import io.restassured.filter.FilterContext;
import io.restassured.http.Cookie;
import io.restassured.http.Cookies;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import io.restassured.specification.FilterableRequestSpecification;
import io.restassured.specification.FilterableResponseSpecification;
import org.mockito.Mockito;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.LinkedHashMap;
import java.util.Map;

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

    @Test(description = "filter attaches textual request/response metadata and returns the downstream response")
    public void filterShouldHandleTextualRequestAndResponse() {
        FilterableRequestSpecification requestSpec = Mockito.mock(FilterableRequestSpecification.class);
        FilterableResponseSpecification responseSpec = Mockito.mock(FilterableResponseSpecification.class);
        FilterContext filterContext = Mockito.mock(FilterContext.class);
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> responseBody = Mockito.mock(ResponseBody.class);

        Map<String, String> formParams = new LinkedHashMap<>();
        formParams.put("k", "v");

        Mockito.when(requestSpec.getMethod()).thenReturn("POST");
        Mockito.when(requestSpec.getURI()).thenReturn("https://example.test/api");
        Mockito.when(requestSpec.getHeaders())
                .thenReturn(new Headers(new Header("Content-Type", "application/json")));
        Mockito.when(requestSpec.getCookies())
                .thenReturn(new Cookies(new Cookie.Builder("session", "abc").build()));
        Mockito.when(requestSpec.getFormParams()).thenReturn(formParams);
        Mockito.when(requestSpec.getBody()).thenReturn("{\"hello\":\"world\"}");
        Mockito.when(requestSpec.getContentType()).thenReturn("application/json; charset=utf-8");

        Mockito.when(filterContext.next(requestSpec, responseSpec)).thenReturn(response);
        Mockito.when(response.getStatusLine()).thenReturn("HTTP/1.1 200 OK");
        Mockito.when(response.getTime()).thenReturn(25L);
        Mockito.when(response.getHeaders()).thenReturn(new Headers(new Header("Content-Type", "application/json")));
        Mockito.when(response.getContentType()).thenReturn("application/json");
        Mockito.when(response.getBody()).thenReturn(responseBody);
        Mockito.when(responseBody.asString()).thenReturn("{\"ok\":true}");

        Response actual = filter.filter(requestSpec, responseSpec, filterContext);

        SHAFT.Validations.assertThat().object(actual).isEqualTo(response).perform();
        Mockito.verify(filterContext).next(requestSpec, responseSpec);
    }

    @Test(description = "filter handles binary request and binary response bodies with normalized MIME types")
    public void filterShouldHandleBinaryRequestAndResponse() {
        FilterableRequestSpecification requestSpec = Mockito.mock(FilterableRequestSpecification.class);
        FilterableResponseSpecification responseSpec = Mockito.mock(FilterableResponseSpecification.class);
        FilterContext filterContext = Mockito.mock(FilterContext.class);
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> responseBody = Mockito.mock(ResponseBody.class);

        Mockito.when(requestSpec.getMethod()).thenReturn("PUT");
        Mockito.when(requestSpec.getURI()).thenReturn("https://example.test/binary");
        Mockito.when(requestSpec.getHeaders()).thenReturn(new Headers());
        Mockito.when(requestSpec.getCookies()).thenReturn(new Cookies());
        Mockito.when(requestSpec.getFormParams()).thenReturn(new LinkedHashMap<>());
        Mockito.when(requestSpec.getBody()).thenReturn(new byte[]{1, 2, 3});
        Mockito.when(requestSpec.getContentType()).thenReturn(null);

        Mockito.when(filterContext.next(requestSpec, responseSpec)).thenReturn(response);
        Mockito.when(response.getStatusLine()).thenReturn("HTTP/1.1 201 Created");
        Mockito.when(response.getTime()).thenReturn(7L);
        Mockito.when(response.getHeaders()).thenReturn(new Headers());
        Mockito.when(response.getContentType()).thenReturn("application/pdf; charset=binary");
        Mockito.when(response.getBody()).thenReturn(responseBody);
        Mockito.when(responseBody.asByteArray()).thenReturn(new byte[]{9, 8, 7});

        Response actual = filter.filter(requestSpec, responseSpec, filterContext);

        SHAFT.Validations.assertThat().object(actual).isEqualTo(response).perform();
        Mockito.verify(filterContext).next(requestSpec, responseSpec);
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

    @Test(description = "looksLikeJson returns false for invalid JSON that only looks like JSON")
    public void looksLikeJsonShouldReturnFalseForInvalidJson() {
        // Strings that start/end with brackets but are NOT valid JSON — the improved
        // implementation uses actual Gson parsing to eliminate these false positives.
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("{not valid json at all}"))
                .isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(filter.looksLikeJson("[plain text in brackets]"))
                .isEqualTo(false).perform();
    }

    // ─── looksLikeXml ────────────────────────────────────────────────────────

    @Test(description = "looksLikeXml returns true for XML document")
    public void looksLikeXmlShouldReturnTrueForXml() {
        SHAFT.Validations.assertThat().object(filter.looksLikeXml("<root><item>test</item></root>"))
                .isEqualTo(true).perform();
    }

    @Test(description = "looksLikeXml returns true for self-closing XML element")
    public void looksLikeXmlShouldReturnTrueForSelfClosingElement() {
        SHAFT.Validations.assertThat().object(filter.looksLikeXml("<result status=\"ok\"/>"))
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

    @Test(description = "looksLikeXml returns false for text that only looks like XML superficially")
    public void looksLikeXmlShouldReturnFalseForFalsePositive() {
        // "<not xml at all>" starts with '<' and ends with '>' but has no closing tag or self-close
        SHAFT.Validations.assertThat().object(filter.looksLikeXml("<not xml at all>"))
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

    @Test(description = "getFileExtension returns .png for image/png")
    public void getFileExtensionShouldReturnDotPngForImagePng() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("image/png")).isEqualTo(".png").perform();
    }

    @Test(description = "getFileExtension returns .gif for image/gif")
    public void getFileExtensionShouldReturnDotGifForImageGif() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("image/gif")).isEqualTo(".gif").perform();
    }

    @Test(description = "getFileExtension returns .mp4 for video/mp4")
    public void getFileExtensionShouldReturnDotMp4ForVideoMp4() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("video/mp4")).isEqualTo(".mp4").perform();
    }

    @Test(description = "getFileExtension returns .pdf for application/pdf")
    public void getFileExtensionShouldReturnDotPdfForApplicationPdf() {
        SHAFT.Validations.assertThat()
                .object(filter.getFileExtension("application/pdf")).isEqualTo(".pdf").perform();
    }

    // ─── isBinaryContentType ─────────────────────────────────────────────────

    @Test(description = "isBinaryContentType returns true for image types")
    public void isBinaryContentTypeShouldReturnTrueForImageTypes() {
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("image/png"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("image/gif"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("image/jpeg"))
                .isEqualTo(true).perform();
    }

    @Test(description = "isBinaryContentType returns true for video and audio types")
    public void isBinaryContentTypeShouldReturnTrueForMediaTypes() {
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("video/mp4"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("audio/mpeg"))
                .isEqualTo(true).perform();
    }

    @Test(description = "isBinaryContentType returns true for application/pdf and octet-stream")
    public void isBinaryContentTypeShouldReturnTrueForBinaryApplicationTypes() {
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("application/pdf"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("application/octet-stream"))
                .isEqualTo(true).perform();
    }

    @Test(description = "isBinaryContentType returns true for zip and OOXML types")
    public void isBinaryContentTypeShouldReturnTrueForZipAndOoxml() {
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("application/zip"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(
                filter.isBinaryContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                .isEqualTo(true).perform();
    }

    @Test(description = "isBinaryContentType returns false for text types")
    public void isBinaryContentTypeShouldReturnFalseForTextTypes() {
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("application/json"))
                .isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("text/plain"))
                .isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("text/html"))
                .isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType("text/xml"))
                .isEqualTo(false).perform();
    }

    @Test(description = "isBinaryContentType returns false for null or empty content type")
    public void isBinaryContentTypeShouldReturnFalseForNullOrEmpty() {
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType(null))
                .isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(filter.isBinaryContentType(""))
                .isEqualTo(false).perform();
    }

    // ─── normalizeMimeType ────────────────────────────────────────────────────

    @Test(description = "normalizeMimeType strips charset and parameters from content-type")
    public void normalizeMimeTypeShouldStripParameters() {
        SHAFT.Validations.assertThat()
                .object(filter.normalizeMimeType("application/json; charset=utf-8"))
                .isEqualTo("application/json").perform();
        SHAFT.Validations.assertThat()
                .object(filter.normalizeMimeType("text/html; charset=UTF-8"))
                .isEqualTo("text/html").perform();
    }

    @Test(description = "normalizeMimeType returns the MIME type unchanged when no parameters")
    public void normalizeMimeTypeShouldReturnUnchangedWhenNoParameters() {
        SHAFT.Validations.assertThat()
                .object(filter.normalizeMimeType("image/png"))
                .isEqualTo("image/png").perform();
        SHAFT.Validations.assertThat()
                .object(filter.normalizeMimeType("application/json"))
                .isEqualTo("application/json").perform();
    }

    @Test(description = "normalizeMimeType returns application/octet-stream for null or empty")
    public void normalizeMimeTypeShouldReturnOctetStreamForNullOrEmpty() {
        SHAFT.Validations.assertThat()
                .object(filter.normalizeMimeType(null))
                .isEqualTo("application/octet-stream").perform();
        SHAFT.Validations.assertThat()
                .object(filter.normalizeMimeType(""))
                .isEqualTo("application/octet-stream").perform();
    }
}
