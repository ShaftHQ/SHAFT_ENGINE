package com.shaft.gui.browser;

import com.shaft.gui.driver.BrowserDownload;
import com.shaft.gui.driver.DownloadActionsContract;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.WebDriver;
import com.shaft.tools.io.internal.TraceArchiveWriter;
import com.shaft.tools.io.internal.TraceEventRecorder;

import java.nio.file.Path;
import java.nio.file.Files;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.time.Instant;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

final class SeleniumBrowserDownload implements BrowserDownload {
    private final HasDownloads.DownloadedFile file;
    private final DownloadActions owner;
    private final HasDownloads downloads;

    SeleniumBrowserDownload(HasDownloads.DownloadedFile file, DownloadActions owner, HasDownloads downloads) {
        this.file = file;
        this.owner = owner;
        this.downloads = downloads;
    }

    @Override
    public String suggestedFilename() {
        return file.getName();
    }

    @Override
    public Optional<String> url() {
        return Optional.empty();
    }

    @Override
    public OptionalLong size() {
        return OptionalLong.of(file.getSize());
    }

    @Override
    public Optional<Instant> creationTime() {
        return Optional.of(Instant.ofEpochMilli(file.getCreationTime()));
    }

    @Override
    public Optional<Instant> lastModifiedTime() {
        return Optional.of(Instant.ofEpochMilli(file.getLastModifiedTime()));
    }

    @Override
    public Optional<String> failure() {
        return Optional.empty();
    }

    @Override
    public BrowserDownload saveAs(Path target) {
        Path exactTarget = java.util.Objects.requireNonNull(target, "target").toAbsolutePath().normalize();
        return query("save-as", exactTarget.getFileName().toString(), () -> save(exactTarget));
    }

    private BrowserDownload save(Path exactTarget) {
        Path filename = safeFilename();
        Path parent = exactTarget.getParent();
        Path staging = null;
        RuntimeException primaryFailure = null;
        try {
            Files.createDirectories(parent);
            staging = Files.createTempDirectory(parent, ".shaft-download-");
            downloads.downloadFile(file.getName(), staging);
            Path source = staging.resolve(filename).normalize();
            if (!source.startsWith(staging) || !Files.isRegularFile(source)) {
                throw new IOException("Selenium did not provide the requested download: " + file.getName());
            }
            TraceArchiveWriter.copy(source, exactTarget);
            return this;
        } catch (IOException exception) {
            primaryFailure = new UncheckedIOException("Failed to save browser download to " + exactTarget, exception);
            throw primaryFailure;
        } catch (RuntimeException exception) {
            primaryFailure = exception;
            throw exception;
        } finally {
            if (staging != null) {
                try {
                    deleteStaging(staging);
                } catch (IOException cleanupFailure) {
                    if (primaryFailure != null) {
                        primaryFailure.addSuppressed(cleanupFailure);
                    } else {
                        throw new UncheckedIOException("Failed to clean temporary download staging " + staging,
                                cleanupFailure);
                    }
                }
            }
        }
    }

    @Override
    public BrowserDownload cancel() {
        return query("cancel", file.getName(), () -> {
            throw new UnsupportedOperationException("Selenium HasDownloads cannot cancel an individual download.");
        });
    }

    @Override
    public DownloadActionsContract delete() {
        return query("delete", file.getName(), () -> {
            throw new UnsupportedOperationException("Selenium HasDownloads can only clear all managed downloads; use downloads().clear().");
        });
    }

    @Override
    public DownloadActionsContract and() {
        return owner;
    }

    private Path safeFilename() {
        String name = file.getName();
        if (name == null || name.isEmpty() || name.contains("/") || name.contains("\\")
                || name.matches("^[A-Za-z]:.*")) {
            throw new IllegalArgumentException("Downloaded filename must be a single safe path segment.");
        }
        Path candidate = Path.of(name);
        if (candidate.isAbsolute() || candidate.getNameCount() != 1
                || candidate.toString().equals(".") || candidate.toString().equals("..")) {
            throw new IllegalArgumentException("Downloaded filename must be a single safe path segment.");
        }
        return candidate;
    }

    private static void deleteStaging(Path staging) throws IOException {
        IOException failure = null;
        try (var paths = Files.walk(staging)) {
            for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException exception) {
                    if (failure == null) {
                        failure = exception;
                    } else {
                        failure.addSuppressed(exception);
                    }
                }
            }
        }
        if (failure != null) {
            throw failure;
        }
    }

    private <T> T query(String operation, String locator, Supplier<T> action) {
        WebDriver driver = downloads instanceof WebDriver webDriver ? webDriver : null;
        var event = TraceEventRecorder.start("downloads", operation, locator, driver);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "download " + operation + " completed.", null,
                    Map.of("backend", "SELENIUM_WEBDRIVER"), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "download " + operation + " failed.", exception,
                    Map.of("backend", "SELENIUM_WEBDRIVER"), List.of());
            throw exception;
        }
    }
}
