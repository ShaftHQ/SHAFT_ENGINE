package com.shaft.tools.io.internal;

import io.qameta.allure.Allure;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.LinkedHashMap;
import java.util.function.BiConsumer;

public class AttachmentReporter {
    private static final LinkedHashMap<String, BiConsumer<String, ByteArrayOutputStream>> attachmentHandlers = new LinkedHashMap<>();

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
        attachFileBased(attachmentDescription, "text/json", content, ".json");
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
        attachFileBased(attachmentDescription, null, content, null);
    }

    private static void attachFileBased(String attachmentDescription, String contentType, ByteArrayOutputStream content, String fileExtension) {
        Allure.addAttachment(attachmentDescription, contentType, new ByteArrayInputStream(content.toByteArray()), fileExtension);
    }

    public static void attachBasedOnFileType(String attachmentType, String attachmentName,
                                             ByteArrayOutputStream attachmentContent, String attachmentDescription) {
        // Get the appropriate handler based on the attachment type, or use the default handler(resilient in case any changes were to be made to getAttachmentCase)
        BiConsumer<String, ByteArrayOutputStream> handler = attachmentHandlers.getOrDefault(getAttachmentCase(attachmentType, attachmentName), AttachmentReporter::handleDefault);
        // Call the handler with the provided parameters
        handler.accept(attachmentDescription, attachmentContent);
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