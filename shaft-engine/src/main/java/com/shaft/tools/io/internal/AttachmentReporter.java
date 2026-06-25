package com.shaft.tools.io.internal;

import com.epam.reportportal.service.ReportPortal;
import com.google.common.io.Files;
import com.shaft.listeners.TestNGListener;
import io.qameta.allure.Allure;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Calendar;
import java.util.LinkedHashMap;
import java.util.function.BiConsumer;

/**
 * Internal utility class responsible for attaching test artifacts (screenshots, recordings,
 * GIFs, data files, page snapshots, etc.) to the Allure report and optionally to ReportPortal.
 *
 * <p>Attachment routing is driven by the {@code attachmentType} / {@code attachmentName} strings
 * using a {@link java.util.LinkedHashMap} of named handlers registered in the static initialiser.
 * New content types can be supported by adding entries to {@code attachmentHandlers}.
 *
 * <p>This class is not intended for direct use in test code; it is invoked by
 * {@link com.shaft.tools.io.internal.ReportManagerHelper}.
 */
public class AttachmentReporter {
    private static final LinkedHashMap<String, BiConsumer<String, ByteArrayOutputStream>> attachmentHandlers = new LinkedHashMap<>();
    private static final LinkedHashMap<String, AttachmentFormat> attachmentFormats = new LinkedHashMap<>();

    /**
     * This is a utility class and cannot be instantiated.
     *
     * @throws IllegalStateException always
     */
    private AttachmentReporter() {
        throw new IllegalStateException("Utility class");
    }

    static {
        attachmentHandlers.put("screenshot", AttachmentReporter::handleScreenshot);
        attachmentHandlers.put("jpeg", AttachmentReporter::handleJpeg);
        attachmentHandlers.put("recording", AttachmentReporter::handleRecording);
        attachmentHandlers.put("gif", AttachmentReporter::handleGif);
        attachmentHandlers.put("csv", AttachmentReporter::handleCsv);
        attachmentHandlers.put("xml", AttachmentReporter::handleXml);
        attachmentHandlers.put("excel", AttachmentReporter::handleExcel);
        attachmentHandlers.put("docx", AttachmentReporter::handleDocx);
        attachmentHandlers.put("pptx", AttachmentReporter::handlePptx);
        attachmentHandlers.put("json", AttachmentReporter::handleJson);
        attachmentHandlers.put("yaml", AttachmentReporter::handleYaml);
        attachmentHandlers.put("markdown", AttachmentReporter::handleMarkdown);
        attachmentHandlers.put("har", AttachmentReporter::handleHar);
        attachmentHandlers.put("pdf", AttachmentReporter::handlePdf);
        attachmentHandlers.put("properties", AttachmentReporter::handleProperties);
        attachmentHandlers.put("link", AttachmentReporter::handleLink);
        attachmentHandlers.put("engine logs", AttachmentReporter::handleEngineLogs);
        attachmentHandlers.put("page snapshot", AttachmentReporter::handlePageSnapshot);
        attachmentHandlers.put("html", AttachmentReporter::handleHtml);
        attachmentHandlers.put("zip", AttachmentReporter::handleZip);
        attachmentHandlers.put("binary", AttachmentReporter::handleBinary);
        attachmentHandlers.put("default", AttachmentReporter::handleDefault);

        attachmentFormats.put("screenshot", new AttachmentFormat("image/png", ".png"));
        attachmentFormats.put("jpeg", new AttachmentFormat("image/jpeg", ".jpg"));
        attachmentFormats.put("recording", new AttachmentFormat("video/mp4", ".mp4"));
        attachmentFormats.put("gif", new AttachmentFormat("image/gif", ".gif"));
        attachmentFormats.put("csv", new AttachmentFormat("text/csv", ".csv"));
        attachmentFormats.put("xml", new AttachmentFormat("text/xml", ".xml"));
        attachmentFormats.put("excel", new AttachmentFormat("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".xlsx"));
        attachmentFormats.put("docx", new AttachmentFormat("application/vnd.openxmlformats-officedocument.wordprocessingml.document", ".docx"));
        attachmentFormats.put("pptx", new AttachmentFormat("application/vnd.openxmlformats-officedocument.presentationml.presentation", ".pptx"));
        attachmentFormats.put("json", new AttachmentFormat("application/json", ".json"));
        attachmentFormats.put("yaml", new AttachmentFormat("application/yaml", ".yaml"));
        attachmentFormats.put("markdown", new AttachmentFormat("text/markdown", ".md"));
        attachmentFormats.put("har", new AttachmentFormat("application/json", ".har"));
        attachmentFormats.put("pdf", new AttachmentFormat("application/pdf", ".pdf"));
        attachmentFormats.put("properties", new AttachmentFormat("text/plain", ".properties"));
        attachmentFormats.put("link", new AttachmentFormat("text/uri-list", ".uri"));
        attachmentFormats.put("engine logs", new AttachmentFormat("text/plain", ".txt"));
        attachmentFormats.put("page snapshot", new AttachmentFormat("multipart/related", ".mhtml"));
        attachmentFormats.put("html", new AttachmentFormat("text/html", ".html"));
        attachmentFormats.put("zip", new AttachmentFormat("application/zip", ".zip"));
        attachmentFormats.put("binary", new AttachmentFormat("application/octet-stream", ".bin"));
        attachmentFormats.put("default", new AttachmentFormat("text/plain", ".txt"));
    }

    private record AttachmentFormat(String contentType, String fileExtension) {
    }

    private static void handleScreenshot(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "image/png", content, ".png");
    }

    private static void handleJpeg(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "image/jpeg", content, ".jpg");
    }

    private static void handleRecording(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "video/mp4", content, ".mp4");
    }

    private static void handleGif(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "image/gif", content, ".gif");
    }

    private static void handleCsv(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/csv", content, ".csv");
    }

    private static void handleXml(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/xml", content, ".xml");
    }

    @SuppressWarnings("SpellCheckingInspection")
    private static void handleExcel(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", content, ".xlsx");
    }

    private static void handleDocx(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/vnd.openxmlformats-officedocument.wordprocessingml.document", content, ".docx");
    }

    private static void handlePptx(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/vnd.openxmlformats-officedocument.presentationml.presentation", content, ".pptx");
    }

    private static void handleJson(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/json", content, ".json");
    }

    private static void handleYaml(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/yaml", content, ".yaml");
    }

    private static void handleMarkdown(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/markdown", content, ".md");
    }

    private static void handleHar(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/json", content, ".har");
    }

    private static void handlePdf(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/pdf", content, ".pdf");
    }

    private static void handleProperties(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/plain", content, ".properties");
    }

    private static void handleLink(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/uri-list", content, ".uri");
    }

    private static void handleEngineLogs(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/plain", content, ".txt");
    }

    private static void handlePageSnapshot(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "multipart/related", content, ".mhtml");
    }

    private static void handleHtml(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/html", content, ".html");
    }

    private static void handleZip(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/zip", content, ".zip");
    }

    private static void handleBinary(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/octet-stream", content, ".bin");
    }

    private static void handleDefault(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/plain", content, ".txt");
    }

    private static void attachFileBased(String attachmentDescription, String contentType, ByteArrayOutputStream content, String fileExtension) {
        Allure.addAttachment(attachmentDescription, contentType, new ByteArrayInputStream(content.toByteArray()), fileExtension);
        ReportContext.recordAttachment(attachmentDescription, contentType, fileExtension, "", content.size());
        if (TestNGListener.isReportPortalEnabled()) {
            File file = null;
            try {
                file = File.createTempFile("rp-test", fileExtension);
                Files.write(content.toByteArray(), file);
                ReportPortal.emitLog(attachmentDescription, "INFO", Calendar.getInstance().getTime(), file);
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            } finally {
                if (file != null) {
                    if (!file.delete()) {
                        file.deleteOnExit();
                    }
                }
            }
        }
    }

    private static void attachFileBased(String attachmentDescription, AttachmentFormat attachmentFormat, Path contentPath) {
        try (InputStream content = java.nio.file.Files.newInputStream(contentPath)) {
            Allure.addAttachment(attachmentDescription, attachmentFormat.contentType(), content, attachmentFormat.fileExtension());
            ReportContext.recordAttachment(attachmentDescription, attachmentFormat.contentType(),
                    attachmentFormat.fileExtension(), "", java.nio.file.Files.size(contentPath));
            if (TestNGListener.isReportPortalEnabled()) {
                ReportPortal.emitLog(attachmentDescription, "INFO", Calendar.getInstance().getTime(), contentPath.toFile());
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    /**
     * Attaches the given content to the Allure report (and ReportPortal if enabled),
     * automatically selecting the correct MIME type and file extension based on the
     * attachment type and name strings.
     *
     * <p>The {@code attachmentType} string is matched (case-insensitively) against known keywords
     * such as {@code "screenshot"}, {@code "gif"}, {@code "json"}, {@code "xml"}, etc.
     * For unrecognised types the {@code "default"} handler is used, which attaches the raw bytes
     * without a declared MIME type.
     *
     * <p>Example:
     * <pre>{@code
     * AttachmentReporter.attachBasedOnFileType(
     *     "screenshot",
     *     "Home page screenshot",
     *     screenshotOutputStream,
     *     "Screenshot after login"
     * );
     * }</pre>
     *
     * @param attachmentType        a string describing the content category (e.g. {@code "screenshot"},
     *                              {@code "json"}, {@code "page snapshot"})
     * @param attachmentName        the file or data name, also inspected for type inference
     *                              (e.g. {@code "response.xml"})
     * @param attachmentContent     the raw content to attach as a {@link java.io.ByteArrayOutputStream}
     * @param attachmentDescription the human-readable label shown in the Allure report attachment panel
     */
    public static void attachBasedOnFileType(String attachmentType, String attachmentName,
                                             ByteArrayOutputStream attachmentContent, String attachmentDescription) {
        // Get the appropriate handler based on the attachment type, or use the default handler(resilient in case any changes were to be made to getAttachmentCase)
        BiConsumer<String, ByteArrayOutputStream> handler = attachmentHandlers.getOrDefault(getAttachmentCase(attachmentType, attachmentName), AttachmentReporter::handleDefault);
        // Call the handler with the provided parameters
        handler.accept(attachmentDescription, attachmentContent);
    }

    /**
     * Attaches an already materialized file to Allure without copying the whole file into memory first.
     *
     * @param attachmentType        a string describing the content category
     * @param attachmentName        the file or data name, also inspected for type inference
     * @param attachmentContentPath the file to attach
     * @param attachmentDescription the human-readable label shown in the Allure report attachment panel
     */
    public static void attachBasedOnFileType(String attachmentType, String attachmentName,
                                             Path attachmentContentPath, String attachmentDescription) {
        String attachmentCase = getAttachmentCase(attachmentType, attachmentName);
        AttachmentFormat attachmentFormat = attachmentFormats.getOrDefault(attachmentCase, attachmentFormats.get("default"));
        attachFileBased(attachmentDescription, attachmentFormat, attachmentContentPath);
    }

    private static String getAttachmentCase(String attachmentType, String attachmentName) {
        String type = value(attachmentType).toLowerCase();
        String name = value(attachmentName).toLowerCase();
        String extensionCase = caseFromExtension(name);
        if (!extensionCase.isBlank()) {
            return extensionCase;
        }
        String combined = type + " " + name;
        if (containsAny(combined, "screenshot", "png", "image/png", "playwright screenshot")) return "screenshot";
        if (containsAny(combined, "jpeg", "jpg", "image/jpeg")) return "jpeg";
        if (containsAny(combined, "gif", "animated gif", "image/gif")) return "gif";
        if (containsAny(combined, "recording", "video", "mp4", "video/mp4")) return "recording";
        if (containsAny(combined, "har", "network.har")) return "har";
        if (containsAny(combined, "json", "application/json")) return "json";
        if (containsAny(combined, "xml", "text/xml", "application/xml")) return "xml";
        if (containsAny(combined, "csv", "text/csv")) return "csv";
        if (containsAny(combined, "yaml", "yml", "application/yaml")) return "yaml";
        if (containsAny(combined, "markdown", "text/markdown")) return "markdown";
        if (containsAny(combined, "pdf", "application/pdf")) return "pdf";
        if (containsAny(combined, "excel", "xlsx")) return "excel";
        if (containsAny(combined, "docx")) return "docx";
        if (containsAny(combined, "pptx")) return "pptx";
        if (containsAny(combined, "html", "text/html")) return "html";
        if (containsAny(combined, "page snapshot", "mhtml")) return "page snapshot";
        if (containsAny(combined, "link", "uri", "text/uri-list")) return "link";
        if (containsAny(combined, "engine logs")) return "engine logs";
        if (containsAny(combined, "zip", "trace", "diagnostics", "application/zip")) return "zip";
        if (containsAny(combined, "octet-stream", "binary")) return "binary";
        if (containsAny(combined, "properties")) return "properties";
        return "default";
    }

    private static String caseFromExtension(String attachmentName) {
        if (attachmentName.endsWith(".png")) return "screenshot";
        if (attachmentName.endsWith(".jpeg") || attachmentName.endsWith(".jpg")) return "jpeg";
        if (attachmentName.endsWith(".gif")) return "gif";
        if (attachmentName.endsWith(".mp4")) return "recording";
        if (attachmentName.endsWith(".har")) return "har";
        if (attachmentName.endsWith(".json")) return "json";
        if (attachmentName.endsWith(".xml")) return "xml";
        if (attachmentName.endsWith(".csv")) return "csv";
        if (attachmentName.endsWith(".yaml") || attachmentName.endsWith(".yml")) return "yaml";
        if (attachmentName.endsWith(".md")) return "markdown";
        if (attachmentName.endsWith(".pdf")) return "pdf";
        if (attachmentName.endsWith(".xlsx")) return "excel";
        if (attachmentName.endsWith(".docx")) return "docx";
        if (attachmentName.endsWith(".pptx")) return "pptx";
        if (attachmentName.endsWith(".html") || attachmentName.endsWith(".htm")) return "html";
        if (attachmentName.endsWith(".mhtml")) return "page snapshot";
        if (attachmentName.endsWith(".uri")) return "link";
        if (attachmentName.endsWith(".zip")) return "zip";
        if (attachmentName.endsWith(".properties")) return "properties";
        return "";
    }

    private static boolean containsAny(String value, String... tokens) {
        for (String token : tokens) {
            if (value.contains(token)) {
                return true;
            }
        }
        return false;
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

}
