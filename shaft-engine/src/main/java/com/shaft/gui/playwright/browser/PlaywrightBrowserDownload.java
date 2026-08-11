package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.Download;
import com.shaft.gui.driver.BrowserDownload;
import com.shaft.gui.driver.DownloadActionsContract;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.tools.io.internal.TraceArchiveWriter;
import com.shaft.tools.io.internal.TraceEventRecorder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

final class PlaywrightBrowserDownload implements BrowserDownload {
    private final Download download;
    private final DownloadActions owner;
    private final PlaywrightSession session;

    PlaywrightBrowserDownload(Download download, DownloadActions owner, PlaywrightSession session) {
        this.download = download;
        this.owner = owner;
        this.session = session;
    }

    @Override
    public String suggestedFilename() {
        return download.suggestedFilename();
    }

    @Override
    public Optional<String> url() {
        return Optional.ofNullable(download.url()).filter(value -> !value.isBlank());
    }

    @Override
    public OptionalLong size() {
        try {
            return OptionalLong.of(Files.size(download.path()));
        } catch (IOException | RuntimeException ignored) {
            return OptionalLong.empty();
        }
    }

    @Override
    public Optional<Instant> creationTime() {
        return attributes().map(attributes -> attributes.creationTime().toInstant());
    }

    @Override
    public Optional<Instant> lastModifiedTime() {
        return attributes().map(attributes -> attributes.lastModifiedTime().toInstant());
    }

    @Override
    public Optional<String> failure() {
        return Optional.ofNullable(download.failure()).filter(value -> !value.isBlank());
    }

    @Override
    public BrowserDownload saveAs(Path target) {
        Path exactTarget = java.util.Objects.requireNonNull(target, "target").toAbsolutePath().normalize();
        return query("save-as", exactTarget.getFileName().toString(), () -> save(exactTarget));
    }

    private BrowserDownload save(Path exactTarget) {
        Path staging = null;
        try {
            Files.createDirectories(exactTarget.getParent());
            staging = Files.createTempFile(exactTarget.getParent(), exactTarget.getFileName() + ".shaft-download-", ".tmp");
            download.saveAs(staging);
            TraceArchiveWriter.copy(staging, exactTarget);
        } catch (IOException exception) {
            var failure = new java.io.UncheckedIOException("Failed to publish download target " + exactTarget,
                    exception);
            cleanup(staging, failure);
            throw failure;
        } catch (RuntimeException exception) {
            cleanup(staging, exception);
            throw exception;
        }
        try {
            Files.deleteIfExists(staging);
        } catch (IOException cleanupFailure) {
            throw new java.io.UncheckedIOException("Failed to remove download staging file " + staging,
                    cleanupFailure);
        }
        return this;
    }

    private static void cleanup(Path staging, RuntimeException original) {
        if (staging == null) {
            return;
        }
        try {
            Files.deleteIfExists(staging);
        } catch (IOException cleanupFailure) {
            original.addSuppressed(cleanupFailure);
        }
    }

    @Override
    public BrowserDownload cancel() {
        return query("cancel", suggestedFilename(), () -> {
            download.cancel();
            return this;
        });
    }

    @Override
    public DownloadActionsContract delete() {
        return query("delete", suggestedFilename(), () -> {
            download.delete();
            session.forgetDownload(download);
            return owner;
        });
    }

    @Override
    public DownloadActionsContract and() {
        return owner;
    }

    private Optional<BasicFileAttributes> attributes() {
        try {
            return Optional.of(Files.readAttributes(download.path(), BasicFileAttributes.class));
        } catch (IOException | RuntimeException ignored) {
            return Optional.empty();
        }
    }

    private <T> T query(String operation, String locator, Supplier<T> action) {
        var event = TraceEventRecorder.startForBackend("downloads", operation, locator,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "download " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "download " + operation + " failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }
}
