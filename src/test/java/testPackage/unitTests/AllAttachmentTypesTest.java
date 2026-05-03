package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.AttachmentReporter;
import io.qameta.allure.Allure;
import io.qameta.allure.Epic;
import io.qameta.allure.Feature;
import io.qameta.allure.Severity;
import io.qameta.allure.SeverityLevel;
import io.qameta.allure.Story;
import io.qameta.allure.model.Attachment;
import org.testng.annotations.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Comprehensive test that exercises every attachment type supported by SHAFT,
 * verifies that each produces the correct MIME type in the Allure 3 lifecycle,
 * and generates a rich Allure report that can be visually inspected.
 *
 * <h2>How to run this test for visual validation</h2>
 * <ol>
 *   <li>Run {@code mvn test -Dtest=AllAttachmentTypesTest}</li>
 *   <li>Generate the Allure HTML report:
 *       {@code allure serve allure-results} (or via the Allure 3 npm package)</li>
 *   <li>Navigate to the test in the report and click on each attachment to
 *       verify it renders with the expected format (JSON syntax highlighting,
 *       XML/CSV as text, images displayed inline, etc.).</li>
 * </ol>
 *
 * <p><strong>Implementation note:</strong> Attachment calls are made directly at
 * the {@code @Test} method scope (not inside {@code Allure.step()} lambdas). This
 * is because {@code Allure.addAttachment()} registers on the currently active
 * lifecycle stage; if it is called inside a step-lambda the attachment belongs to
 * the step, not the test case, so {@code updateTestCase} cannot capture it.
 *
 * <p>This class validates the two bug fixes made for the
 * "API attachments not styled properly in .JSON format" issue:
 * <ol>
 *   <li>{@link AttachmentReporter} now uses {@code application/json} (RFC 8259)
 *       instead of the non-standard {@code text/json} for JSON attachments.</li>
 *   <li>{@code ShaftRestAssuredFilter} replaces {@code AllureRestAssured},
 *       eliminating the {@code text/html} wrapping of API request/response bodies.</li>
 * </ol>
 */
@Epic("Allure 3 Migration")
@Feature("Attachment Types")
public class AllAttachmentTypesTest {

    // ─── sample content ───────────────────────────────────────────────────────

    private static final String SAMPLE_JSON =
            "{\n  \"id\": 1,\n  \"name\": \"test\",\n  \"status\": \"passed\",\n"
                    + "  \"tags\": [\"allure3\", \"attachment\"]\n}";
    private static final String SAMPLE_XML =
            "<result><id>1</id><name>test</name><status>passed</status></result>";
    private static final String SAMPLE_CSV =
            "id,name,status\n1,test,passed\n2,demo,skipped\n";
    private static final String SAMPLE_HTML =
            "<html><body><h1>Test Report</h1><p>Attachment test page</p></body></html>";
    private static final String SAMPLE_TEXT =
            "Engine log line 1: Test started\nEngine log line 2: Assertion passed\n";
    private static final String SAMPLE_PROPERTIES =
            "targetBrowserName=chrome\nbrowser.headless=false\nbaseUrl=https://example.com\n";
    private static final String SAMPLE_URI =
            "https://shafthq.github.io/SHAFT_ENGINE/\n";
    private static final String SAMPLE_MHTML =
            "MIME-Version: 1.0\nContent-Type: multipart/related;\n\n--boundary\n"
                    + "Content-Type: text/html\n\n<html><body>Snapshot</body></html>\n";

    /** Minimal valid 8-byte PNG signature used for the screenshot attachment test. */
    private static final byte[] PNG_SIGNATURE = {
            (byte) 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A
    };

    // ─── helper ───────────────────────────────────────────────────────────────

    private static ByteArrayOutputStream bytesOf(String content) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            baos.write(content.getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new RuntimeException("Failed to write sample content to ByteArrayOutputStream", e);
        }
        return baos;
    }

    private static ByteArrayOutputStream bytesOf(byte[] bytes) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            baos.write(bytes);
        } catch (IOException e) {
            throw new RuntimeException("Failed to write sample bytes to ByteArrayOutputStream", e);
        }
        return baos;
    }

    /**
     * Reads the MIME type of the last attachment registered on the current Allure
     * test case.
     *
     * <p><strong>Important:</strong> This method must be called at the test-case scope
     * (not inside a step lambda) so that the attachment and the read both occur at
     * the same lifecycle level.
     *
     * @return the {@code type} field of the most-recently added {@link Attachment}
     * @throws AssertionError if no attachment was found (attachment creation failed silently)
     */
    private String captureLastAttachmentMimeType() {
        AtomicReference<String> capturedType = new AtomicReference<>(null);
        Allure.getLifecycle().updateTestCase(result -> {
            List<Attachment> attachments = result.getAttachments();
            if (!attachments.isEmpty()) {
                capturedType.set(attachments.getLast().getType());
            }
        });
        if (capturedType.get() == null) {
            throw new AssertionError(
                    "No attachment found in the Allure test case. "
                            + "Ensure attachBasedOnFileType() is called at the @Test method scope "
                            + "(not inside an Allure.step() lambda) and that it did not fail silently.");
        }
        return capturedType.get();
    }

    // ─── one test per attachment type ─────────────────────────────────────────

    /**
     * Verifies that a JSON attachment is created with {@code application/json} MIME type.
     *
     * <p>This is the primary regression test for the core bug fix: previously
     * {@code AttachmentReporter.handleJson()} used {@code "text/json"} which is not
     * a registered MIME type, causing Allure 3 to render the attachment without JSON
     * syntax highlighting.
     */
    @Test(description = "JSON attachment uses application/json MIME type in Allure 3")
    @Story("JSON attachment")
    @Severity(SeverityLevel.CRITICAL)
    public void jsonAttachmentCreatedWithApplicationJsonMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "json", "api-response.json",
                bytesOf(SAMPLE_JSON),
                "API Response Body (JSON)");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("application/json").perform();
    }

    /**
     * Verifies that when the attachment NAME contains "json" (the pattern used by
     * {@code RestActions.compareJSON}), the handler also produces {@code application/json}.
     */
    @Test(description = "Attachment whose name contains 'json' routes to application/json MIME type")
    @Story("JSON attachment")
    @Severity(SeverityLevel.CRITICAL)
    public void jsonAttachmentViaNameRoutingUsesApplicationJsonMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "File Content", "Expected JSON",
                bytesOf(SAMPLE_JSON),
                "File Content - Expected JSON");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("application/json").perform();
    }

    /**
     * Verifies that an XML attachment uses the {@code text/xml} MIME type.
     */
    @Test(description = "XML attachment uses text/xml MIME type in Allure 3")
    @Story("XML attachment")
    @Severity(SeverityLevel.NORMAL)
    public void xmlAttachmentCreatedWithTextXmlMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "xml", "api-response.xml",
                bytesOf(SAMPLE_XML),
                "API Response Body (XML)");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/xml").perform();
    }

    /**
     * Verifies that a CSV attachment uses the {@code text/csv} MIME type.
     */
    @Test(description = "CSV attachment uses text/csv MIME type in Allure 3")
    @Story("CSV attachment")
    @Severity(SeverityLevel.NORMAL)
    public void csvAttachmentCreatedWithTextCsvMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "csv", "test-data.csv",
                bytesOf(SAMPLE_CSV),
                "Test Data (CSV)");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/csv").perform();
    }

    /**
     * Verifies that an HTML attachment uses the {@code text/html} MIME type.
     */
    @Test(description = "HTML attachment uses text/html MIME type in Allure 3")
    @Story("HTML attachment")
    @Severity(SeverityLevel.NORMAL)
    public void htmlAttachmentCreatedWithTextHtmlMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "html", "page.html",
                bytesOf(SAMPLE_HTML),
                "Page HTML");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/html").perform();
    }

    /**
     * Verifies that an engine-logs attachment uses the {@code text/plain} MIME type.
     */
    @Test(description = "Engine logs attachment uses text/plain MIME type in Allure 3")
    @Story("Engine logs attachment")
    @Severity(SeverityLevel.NORMAL)
    public void engineLogsAttachmentCreatedWithTextPlainMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "engine logs", "execution.log",
                bytesOf(SAMPLE_TEXT),
                "SHAFT Engine Logs - execution");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/plain").perform();
    }

    /**
     * Verifies that a properties attachment uses the {@code text/plain} MIME type.
     */
    @Test(description = "Properties attachment uses text/plain MIME type in Allure 3")
    @Story("Properties attachment")
    @Severity(SeverityLevel.MINOR)
    public void propertiesAttachmentCreatedWithTextPlainMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "properties", "custom.properties",
                bytesOf(SAMPLE_PROPERTIES),
                "SHAFT Properties");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/plain").perform();
    }

    /**
     * Verifies that a link (URI) attachment uses the {@code text/uri-list} MIME type.
     */
    @Test(description = "Link attachment uses text/uri-list MIME type in Allure 3")
    @Story("Link attachment")
    @Severity(SeverityLevel.MINOR)
    public void linkAttachmentCreatedWithTextUriListMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "link", "report-url.uri",
                bytesOf(SAMPLE_URI),
                "SHAFT User Guide Link");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/uri-list").perform();
    }

    /**
     * Verifies that a page-snapshot (MHTML) attachment uses the
     * {@code multipart/related} MIME type.
     */
    @Test(description = "Page snapshot attachment uses multipart/related MIME type in Allure 3")
    @Story("Page snapshot attachment")
    @Severity(SeverityLevel.NORMAL)
    public void pageSnapshotAttachmentCreatedWithMultipartRelatedMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "page snapshot", "snapshot.mhtml",
                bytesOf(SAMPLE_MHTML),
                "Page Snapshot");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("multipart/related").perform();
    }

    /**
     * Verifies that a screenshot attachment uses the {@code image/png} MIME type.
     * Uses a minimal 8-byte PNG signature to keep the test lightweight.
     */
    @Test(description = "Screenshot attachment uses image/png MIME type in Allure 3")
    @Story("Screenshot attachment")
    @Severity(SeverityLevel.CRITICAL)
    public void screenshotAttachmentCreatedWithImagePngMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "screenshot", "homepage.png",
                bytesOf(PNG_SIGNATURE),
                "Homepage Screenshot");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("image/png").perform();
    }

    /**
     * Verifies that an Excel attachment uses the correct OOXML MIME type.
     */
    @Test(description = "Excel attachment uses correct OOXML MIME type in Allure 3")
    @Story("Excel attachment")
    @Severity(SeverityLevel.MINOR)
    public void excelAttachmentCreatedWithOoxmlMimeType() {
        AttachmentReporter.attachBasedOnFileType(
                "excel", "report.xlsx",
                bytesOf("fake-excel-content"),
                "Test Results Excel");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType())
                .isEqualTo("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
                .perform();
    }

    /**
     * Verifies that an unknown/unrecognised attachment type falls back to {@code text/plain}.
     */
    @Test(description = "Unknown attachment type falls back to text/plain MIME type")
    @Story("Default (unknown) attachment")
    @Severity(SeverityLevel.MINOR)
    public void unknownAttachmentTypeFallsBackToTextPlain() {
        AttachmentReporter.attachBasedOnFileType(
                "unknownBinaryType", "data.bin",
                bytesOf("binary content"),
                "Unknown Binary Data");

        SHAFT.Validations.assertThat()
                .object(captureLastAttachmentMimeType()).isEqualTo("text/plain").perform();
    }
}
