package com.shaft.tools.io.internal;

import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureResultsWriteException;
import io.qameta.allure.AttachmentOptions;
import org.apache.logging.log4j.Level;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.channels.ClosedByInterruptException;
import java.nio.charset.StandardCharsets;

/**
 * Compatibility helpers for Allure Java 3.x attachment APIs.
 *
 * <p>Allure 3 replaced {@code Allure.addAttachment(...)} with {@code Allure.attachment(...)} /
 * {@link io.qameta.allure.AllureLifecycle#addAttachment} plus {@link AttachmentOptions}.
 * SHAFT call sites use this helper so attachment behaviour stays on the current executable
 * (not wrapped as an attachment-step) while matching the pre-3 signatures.
 *
 * <p>Allure 3's {@code FileSystemResultsWriter} uses NIO channels that throw
 * {@link ClosedByInterruptException} when the caller thread is interrupted. Attachments are
 * best-effort evidence, so interrupt-driven write failures are logged and swallowed instead of
 * failing the calling assertion/action.
 */
public final class AllureAttachments {

    private AllureAttachments() {
        throw new IllegalStateException("Utility class");
    }

    public static void add(String name, String type, String body) {
        warnIfSwapped(name, type, body);
        byte[] bytes = body == null ? new byte[0] : body.getBytes(StandardCharsets.UTF_8);
        write(name, type, new ByteArrayInputStream(bytes), null);
    }

    public static void add(String name, String type, InputStream content, String fileExtension) {
        warnIfSwapped(name, type, null);
        write(name, type, content, fileExtension);
    }

    private static void write(String name, String type, InputStream content, String fileExtension) {
        try {
            Allure.getLifecycle().addAttachment(name, type, content, options(fileExtension));
        } catch (AllureResultsWriteException exception) {
            handleAttachmentWriteFailure(exception);
        }
    }

    public static void add(String name, String type, byte[] content, String fileExtension) {
        add(name, type, new ByteArrayInputStream(content == null ? new byte[0] : content), fileExtension);
    }

    private static void warnIfSwapped(String name, String type, String textContent) {
        String problem = AttachmentTypeGuard.callTimeProblem(name, type, textContent);
        if (problem != null) {
            ReportManager.logDiscrete(problem, Level.WARN);
        }
    }

    static AttachmentOptions options(String fileExtension) {
        if (fileExtension == null) {
            return AttachmentOptions.empty();
        }
        return AttachmentOptions.withFileExtension(fileExtension);
    }

    private static void handleAttachmentWriteFailure(AllureResultsWriteException exception) {
        if (isInterruptDrivenWriteFailure(exception)) {
            Thread.currentThread().interrupt();
        }
        ReportManagerHelper.logDiscrete(exception);
    }

    private static boolean isInterruptDrivenWriteFailure(Throwable throwable) {
        if (Thread.currentThread().isInterrupted()) {
            return true;
        }
        for (Throwable current = throwable; current != null; current = current.getCause()) {
            if (current instanceof ClosedByInterruptException
                    || current instanceof InterruptedException) {
                return true;
            }
        }
        return false;
    }
}
