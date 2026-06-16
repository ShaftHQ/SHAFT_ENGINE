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
        attachmentHandlers.put("recording", AttachmentReporter::handleRecording);
        attachmentHandlers.put("gif", AttachmentReporter::handleGif);
        attachmentHandlers.put("csv", AttachmentReporter::handleCsv);
        attachmentHandlers.put("xml", AttachmentReporter::handleXml);
        attachmentHandlers.put("excel", AttachmentReporter::handleExcel);
        attachmentHandlers.put("json", AttachmentReporter::handleJson);
        attachmentHandlers.put("properties", AttachmentReporter::handleProperties);
        attachmentHandlers.put("link", AttachmentReporter::handleLink);
        attachmentHandlers.put("engine logs", AttachmentReporter::handleEngineLogs);
        attachmentHandlers.put("page snapshot", AttachmentReporter::handlePageSnapshot);
        attachmentHandlers.put("html", AttachmentReporter::handleHtml);
        attachmentHandlers.put("default", AttachmentReporter::handleDefault);

        attachmentFormats.put("screenshot", new AttachmentFormat("image/png", ".png"));
        attachmentFormats.put("recording", new AttachmentFormat("video/mp4", ".mp4"));
        attachmentFormats.put("gif", new AttachmentFormat("image/gif", ".gif"));
        attachmentFormats.put("csv", new AttachmentFormat("text/csv", ".csv"));
        attachmentFormats.put("xml", new AttachmentFormat("text/xml", ".xml"));
        attachmentFormats.put("excel", new AttachmentFormat("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".xlsx"));
        attachmentFormats.put("json", new AttachmentFormat("application/json", ".json"));
        attachmentFormats.put("properties", new AttachmentFormat("text/plain", ".properties"));
        attachmentFormats.put("link", new AttachmentFormat("text/uri-list", ".uri"));
        attachmentFormats.put("engine logs", new AttachmentFormat("text/plain", ".txt"));
        attachmentFormats.put("page snapshot", new AttachmentFormat("multipart/related", ".mhtml"));
        attachmentFormats.put("html", new AttachmentFormat("text/html", ".html"));
        attachmentFormats.put("default", new AttachmentFormat("text/plain", ".txt"));
    }

    private record AttachmentFormat(String contentType, String fileExtension) {
    }

    private static void handleScreenshot(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "image/png", content, ".png");
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

    private static void handleJson(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "application/json", content, ".json");
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

    private static void handleDefault(String attachmentDescription, ByteArrayOutputStream content) {
        attachFileBased(attachmentDescription, "text/plain", content, ".txt");
    }

    private static void attachFileBased(String attachmentDescription, String contentType, ByteArrayOutputStream content, String fileExtension) {
        Allure.addAttachment(attachmentDescription, contentType, new ByteArrayInputStream(content.toByteArray()), fileExtension);
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
        for (String key : attachmentHandlers.keySet()) {
            switch (key) {
                case "screenshot", "properties", "link", "recording", "gif", "page snapshot", "engine logs", "html" -> {
                    if (attachmentType.toLowerCase().contains(key)) {
                        return key;
                    }
                }
                case "csv", "json", "xml", "excel" -> {
                    if (attachmentType.toLowerCase().contains(key) || attachmentName.toLowerCase().contains(key)) {
                        return key;
                    }
                }
                default -> {
                    return "default";
                }
            }
        }
        return "";
    }

}
