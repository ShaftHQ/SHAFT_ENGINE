package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Covers #7185: a swapped attachment call (content passed as the type, or the MIME type passed as the
 * content) is named at call time and before the report is generated, instead of silently producing a
 * report whose attachment type label is the content.
 */
public class AttachmentTypeGuardTest {
    private static final String MARKDOWN = "# J04 Services tab — locator gaps\n\n- missing id on Save";

    @Test
    public void mimeTypesAreRecognised() {
        for (String type : List.of("text/markdown", "image/png", "application/vnd.allure.image.diff",
                "text/plain; charset=UTF-8", "image/svg+xml")) {
            Assert.assertTrue(AttachmentTypeGuard.isMimeType(type), type);
        }
        for (String type : List.of("# J04 Services tab", "Markdown", "", "text/", "/plain", "a/b\nc",
                "text/" + "x".repeat(130))) {
            Assert.assertFalse(AttachmentTypeGuard.isMimeType(type), type);
        }
        Assert.assertFalse(AttachmentTypeGuard.isMimeType(null));
    }

    @Test
    public void swappedCallsAreNamedWithoutTheWholeContent() {
        String contentAsType = AttachmentTypeGuard.callTimeProblem("gap report", MARKDOWN, "text/markdown").orElseThrow();
        Assert.assertTrue(contentAsType.contains("gap report"), contentAsType);
        Assert.assertTrue(contentAsType.contains("# J04 Services tab — locator gaps"), contentAsType);
        Assert.assertFalse(contentAsType.contains("missing id on Save"), "only the first line is quoted");
        Assert.assertTrue(AttachmentTypeGuard.callTimeProblem("long", "y".repeat(200), null).isPresent(), "over 127 chars");
        String mimeAsContent = AttachmentTypeGuard.callTimeProblem("r", "Report", " text/markdown ").orElseThrow();
        Assert.assertTrue(mimeAsContent.contains("text/markdown"), mimeAsContent);
    }

    @Test
    public void legitimateCallsAreNotFlagged() {
        Assert.assertTrue(AttachmentTypeGuard.callTimeProblem("r", "text/markdown", MARKDOWN).isEmpty());
        Assert.assertTrue(AttachmentTypeGuard.callTimeProblem("r", "Markdown", MARKDOWN).isEmpty(), "SHAFT keywords stay quiet");
        Assert.assertTrue(AttachmentTypeGuard.callTimeProblem("r", "JSON", null).isEmpty());
        Assert.assertTrue(AttachmentTypeGuard.callTimeProblem("r", null, "text").isEmpty());
        Assert.assertTrue(AttachmentTypeGuard.callTimeProblem("r", "text/plain", "text/markdown").isEmpty(),
                "a MIME type may describe a MIME string");
    }

    @Test
    public void addKeepsWorkingForASwappedCall() {
        AllureAttachments.add("swapped", MARKDOWN, "text/markdown");
        AllureAttachments.add("swapped bytes", MARKDOWN, new byte[]{1}, ".md");
    }

    @Test
    public void resultsWithNonMimeTypesAreListedBeforeTheReport() throws IOException {
        Path results = Files.createTempDirectory("shaft-7185");
        Files.writeString(results.resolve("a-result.json"), """
                {"uuid":"a","name":"ServicesTabTest","attachments":[{"name":"ok","source":"1-attachment.png","type":"image/png"}],
                 "steps":[{"name":"s","attachments":[{"name":"untyped","source":"3-attachment"}]}]}
                """, StandardCharsets.UTF_8);
        Files.writeString(results.resolve("b-container.json"), """
                {"uuid":"b","afters":[{"name":"tearDown","attachments":[{"name":"Locator gap report — J04","source":"2-attachment.md","type":"# J04 Services tab — locator gaps\\n\\n- missing id"}]}]}
                """, StandardCharsets.UTF_8);
        Files.writeString(results.resolve("broken-result.json"), "{not json", StandardCharsets.UTF_8);
        List<String> invalid = AllureManager.findInvalidAttachmentTypes(results);
        Assert.assertEquals(invalid,
                List.of("b-container.json: 'Locator gap report — J04' -> type '# J04 Services tab — locator gaps'"));
        Assert.assertEquals(AllureManager.findInvalidAttachmentTypes(results.resolve("a-result.json")), List.of());
    }
}
