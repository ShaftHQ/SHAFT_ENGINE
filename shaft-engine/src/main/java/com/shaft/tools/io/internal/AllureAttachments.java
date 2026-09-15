package com.shaft.tools.io.internal;

import io.qameta.allure.Allure;
import io.qameta.allure.AttachmentOptions;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * Compatibility helpers for Allure Java 3.x attachment APIs.
 *
 * <p>Allure 3 replaced {@code Allure.addAttachment(...)} with {@code Allure.attachment(...)} /
 * {@link io.qameta.allure.AllureLifecycle#addAttachment} plus {@link AttachmentOptions}.
 * SHAFT call sites use this helper so attachment behaviour stays on the current executable
 * (not wrapped as an attachment-step) while matching the pre-3 signatures.
 */
public final class AllureAttachments {

    private AllureAttachments() {
        throw new IllegalStateException("Utility class");
    }

    public static void add(String name, String type, String body) {
        byte[] bytes = body == null ? new byte[0] : body.getBytes(StandardCharsets.UTF_8);
        add(name, type, new ByteArrayInputStream(bytes), null);
    }

    public static void add(String name, String type, InputStream content, String fileExtension) {
        Allure.getLifecycle().addAttachment(name, type, content, options(fileExtension));
    }

    public static void add(String name, String type, byte[] content, String fileExtension) {
        add(name, type, new ByteArrayInputStream(content == null ? new byte[0] : content), fileExtension);
    }

    static AttachmentOptions options(String fileExtension) {
        if (fileExtension == null) {
            return AttachmentOptions.empty();
        }
        return AttachmentOptions.withFileExtension(fileExtension);
    }
}
