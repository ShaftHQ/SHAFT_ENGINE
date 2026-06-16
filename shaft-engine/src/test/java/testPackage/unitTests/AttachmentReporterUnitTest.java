package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.AttachmentReporter;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
import org.testng.annotations.Test;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for {@link AttachmentReporter} verifying that every attachment type
 * uses the correct, standards-compliant MIME type when adding attachments to the
 * Allure 3 report.
 *
 * <p>Each test:
 * <ol>
 *   <li>Calls {@link AttachmentReporter#attachBasedOnFileType} (or the matching
 *       internal handler via {@code getAttachmentCase} reflection) with a specific
 *       attachment category.</li>
 *   <li>Reads back the {@link io.qameta.allure.model.Attachment#getType()} from the
 *       live Allure lifecycle (the current {@code @Test} stage).</li>
 *   <li>Asserts the expected MIME type.</li>
 * </ol>
 *
 * <p>The most important fix this class validates is the JSON content type:
 * {@code "application/json"} (RFC 8259) instead of the non-standard {@code "text/json"}
 * that was previously used.
 */
public class AttachmentReporterUnitTest {

    // ─── helpers ────────────────────────────────────────────────────────────────

    /**
     * Calls {@link AttachmentReporter#attachBasedOnFileType} with minimal byte content
     * and returns the MIME type that was registered on the Allure lifecycle for the
     * most-recently-added attachment.
     *
     * @throws AssertionError if no attachment was found (attachment creation failed silently)
     */
    private String captureAttachmentMimeType(String attachmentType, String attachmentName) {
        ByteArrayOutputStream content = new ByteArrayOutputStream();
        try {
            content.write("sample content".getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            throw new RuntimeException(
                    "Failed to write sample content to ByteArrayOutputStream for '"
                            + attachmentType + "/" + attachmentName + "'", e);
        }

        AttachmentReporter.attachBasedOnFileType(attachmentType, attachmentName, content,
                attachmentType + " - " + attachmentName);

        AtomicReference<String> capturedType = new AtomicReference<>(null);
        Allure.getLifecycle().updateTestCase(result -> {
            List<Attachment> attachments = result.getAttachments();
            if (!attachments.isEmpty()) {
                capturedType.set(attachments.getLast().getType());
            }
        });
        if (capturedType.get() == null) {
            throw new AssertionError(
                    "No attachment found in Allure test-case for type='" + attachmentType
                            + "', name='" + attachmentName + "'. "
                            + "AttachmentReporter.attachBasedOnFileType() may have failed silently.");
        }
        return capturedType.get();
    }

    private String captureFileBackedAttachmentMimeType(String attachmentType, String attachmentName, Path contentPath) {
        AttachmentReporter.attachBasedOnFileType(attachmentType, attachmentName, contentPath,
                attachmentType + " - " + attachmentName);

        AtomicReference<String> capturedType = new AtomicReference<>(null);
        Allure.getLifecycle().updateTestCase(result -> {
            List<Attachment> attachments = result.getAttachments();
            if (!attachments.isEmpty()) {
                capturedType.set(attachments.getLast().getType());
            }
        });
        if (capturedType.get() == null) {
            throw new AssertionError(
                    "No file-backed attachment found in Allure test-case for type='" + attachmentType
                            + "', name='" + attachmentName + "'.");
        }
        return capturedType.get();
    }

    /**
     * Uses reflection to verify that {@code getAttachmentCase} routes a given
     * (type, name) pair to the expected handler key.
     */
    private String invokeGetAttachmentCase(String attachmentType, String attachmentName) throws Exception {
        Method m = AttachmentReporter.class.getDeclaredMethod("getAttachmentCase", String.class, String.class);
        m.setAccessible(true);
        return (String) m.invoke(null, attachmentType, attachmentName);
    }

    // ─── MIME type tests ─────────────────────────────────────────────────────────

    /**
     * JSON attachments MUST use {@code application/json} (RFC 8259), not the
     * non-standard {@code text/json}.
     *
     * <p>This is the primary regression test for the bug fix: previously the handler
     * used {@code "text/json"} which Allure 3 does not recognise as JSON, causing the
     * attachment to render without syntax-highlighting or to display as plain text.
     */
    @Test(description = "JSON attachment should use application/json MIME type (RFC 8259)")
    public void jsonAttachmentShouldUseApplicationJsonMimeType() {
        String mimeType = captureAttachmentMimeType("json", "response.json");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("application/json").perform();
    }

    /**
     * When the attachment NAME contains "json" (e.g. "Expected JSON"), the handler
     * should also route to the JSON handler and use {@code application/json}.
     */
    @Test(description = "Attachment whose name contains 'json' should also use application/json")
    public void attachmentNameContainingJsonShouldUseApplicationJsonMimeType() {
        // The name "Expected JSON" is exactly what compareJSON() passes
        String mimeType = captureAttachmentMimeType("File Content", "Expected JSON");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("application/json").perform();
    }

    /** Screenshot attachments must use {@code image/png}. */
    @Test(description = "Screenshot attachment should use image/png MIME type")
    public void screenshotAttachmentShouldUseImagePngMimeType() {
        String mimeType = captureAttachmentMimeType("screenshot", "homepage.png");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("image/png").perform();
    }

    /** Video recording attachments must use {@code video/mp4}. */
    @Test(description = "Recording attachment should use video/mp4 MIME type")
    public void recordingAttachmentShouldUseVideoMp4MimeType() {
        String mimeType = captureAttachmentMimeType("recording", "test_recording.mp4");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("video/mp4").perform();
    }

    /** Animated GIF attachments must use {@code image/gif}. */
    @Test(description = "GIF attachment should use image/gif MIME type")
    public void gifAttachmentShouldUseImageGifMimeType() {
        String mimeType = captureAttachmentMimeType("gif", "test_actions.gif");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("image/gif").perform();
    }

    /** CSV attachments must use {@code text/csv}. */
    @Test(description = "CSV attachment should use text/csv MIME type")
    public void csvAttachmentShouldUseTextCsvMimeType() {
        String mimeType = captureAttachmentMimeType("csv", "data.csv");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/csv").perform();
    }

    /** XML attachments must use {@code text/xml}. */
    @Test(description = "XML attachment should use text/xml MIME type")
    public void xmlAttachmentShouldUseTextXmlMimeType() {
        String mimeType = captureAttachmentMimeType("xml", "response.xml");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/xml").perform();
    }

    /** Excel attachments must use the correct OOXML MIME type. */
    @Test(description = "Excel attachment should use application/vnd.openxmlformats-officedocument.spreadsheetml.sheet MIME type")
    public void excelAttachmentShouldUseOoxmlMimeType() {
        String mimeType = captureAttachmentMimeType("excel", "report.xlsx");
        SHAFT.Validations.assertThat().object(mimeType)
                .isEqualTo("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet").perform();
    }

    /** HTML attachments must use {@code text/html}. */
    @Test(description = "HTML attachment should use text/html MIME type")
    public void htmlAttachmentShouldUseTextHtmlMimeType() {
        String mimeType = captureAttachmentMimeType("html", "report.html");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/html").perform();
    }

    /** Engine log attachments must use {@code text/plain}. */
    @Test(description = "Engine logs attachment should use text/plain MIME type")
    public void engineLogsAttachmentShouldUseTextPlainMimeType() {
        String mimeType = captureAttachmentMimeType("engine logs", "execution.log");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/plain").perform();
    }

    /** File-backed engine log attachments must use {@code text/plain}. */
    @Test(description = "File-backed engine logs attachment should use text/plain MIME type")
    public void fileBackedEngineLogsAttachmentShouldUseTextPlainMimeType() throws Exception {
        Path engineLog = Files.createTempFile("shaft-engine-log-attachment-", ".txt");
        try {
            Files.writeString(engineLog, "sample engine log", StandardCharsets.UTF_8);
            String mimeType = captureFileBackedAttachmentMimeType("engine logs", "execution.log", engineLog);
            SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/plain").perform();
        } finally {
            Files.deleteIfExists(engineLog);
        }
    }

    /** Page snapshot / MHTML attachments must use {@code multipart/related}. */
    @Test(description = "Page snapshot attachment should use multipart/related MIME type")
    public void pageSnapshotAttachmentShouldUseMultipartRelatedMimeType() {
        String mimeType = captureAttachmentMimeType("page snapshot", "snapshot.mhtml");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("multipart/related").perform();
    }

    /** Properties file attachments must use {@code text/plain}. */
    @Test(description = "Properties attachment should use text/plain MIME type")
    public void propertiesAttachmentShouldUseTextPlainMimeType() {
        String mimeType = captureAttachmentMimeType("properties", "config.properties");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/plain").perform();
    }

    /** URI list link attachments must use {@code text/uri-list}. */
    @Test(description = "Link attachment should use text/uri-list MIME type")
    public void linkAttachmentShouldUseTextUriListMimeType() {
        String mimeType = captureAttachmentMimeType("link", "test_link.uri");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/uri-list").perform();
    }

    /** Unknown / unrecognised attachment types must fall back to {@code text/plain}. */
    @Test(description = "Unknown attachment type should fall back to text/plain MIME type")
    public void unknownAttachmentTypeShouldFallBackToTextPlain() {
        String mimeType = captureAttachmentMimeType("unknownType", "someFile.bin");
        SHAFT.Validations.assertThat().object(mimeType).isEqualTo("text/plain").perform();
    }

    // ─── routing tests ───────────────────────────────────────────────────────────

    /**
     * Verifies that {@code getAttachmentCase} routes attachment-type strings to the
     * correct handler keys. Uses reflection because {@code getAttachmentCase} is private.
     */
    @Test(description = "getAttachmentCase should route type strings to correct handler keys")
    public void getAttachmentCaseShouldRouteCorrectly() throws Exception {
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("screenshot", "img"))
                .isEqualTo("screenshot").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("json", "response.json"))
                .isEqualTo("json").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("File Content", "Expected JSON"))
                .isEqualTo("json").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("csv", "data.csv"))
                .isEqualTo("csv").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("xml", "data.xml"))
                .isEqualTo("xml").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("html", "page.html"))
                .isEqualTo("html").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("engine logs", "exec.log"))
                .isEqualTo("engine logs").perform();
        SHAFT.Validations.assertThat().object(invokeGetAttachmentCase("unknown_type", "file.bin"))
                .isEqualTo("default").perform();
    }
}
