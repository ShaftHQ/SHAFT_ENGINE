package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileFileActionsContract;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.TraceArchiveWriter;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.PullsFiles;
import io.appium.java_client.PushesFiles;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Appium file-transfer implementation for the categorized mobile facade. */
final class FileActions implements MobileFileActionsContract {
    private final MobileActions mobile;

    FileActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public byte[] pull(String devicePath) {
        return query("pull", () -> {
            registerSourcePath(devicePath);
            PullsFiles provider = pulls();
            String path = requireDevicePath(devicePath);
            try {
                return provider.pullFile(path);
            } catch (RuntimeException exception) {
                registerPathFailure(path);
                throw exception;
            }
        });
    }

    @Override
    public String pullText(String devicePath) {
        return query("pull-text", () -> {
            registerSourcePath(devicePath);
            PullsFiles provider = pulls();
            String path = requireDevicePath(devicePath);
            byte[] content;
            try {
                content = provider.pullFile(path);
            } catch (RuntimeException exception) {
                registerPathFailure(path);
                throw exception;
            }
            String result = new String(content, StandardCharsets.UTF_8);
            FailureTraceReporter.registerSensitiveSourceValue(result);
            FailureTraceReporter.registerSensitiveValue(result);
            return result;
        });
    }

    @Override
    public Path pullTo(String devicePath, Path localTarget) {
        return query("pull-to", () -> {
            registerSourcePath(devicePath);
            registerSourcePath(localTarget == null ? null : localTarget.toString());
            PullsFiles provider = pulls();
            String path = requireDevicePath(devicePath);
            Path target = Objects.requireNonNull(localTarget, "local target").toAbsolutePath().normalize();
            byte[] content;
            try {
                content = provider.pullFile(path);
            } catch (RuntimeException exception) {
                registerPathFailure(path, target.toString());
                throw exception;
            }
            try {
                TraceArchiveWriter.writeBytes(target, content);
                return target;
            } catch (IOException exception) {
                registerLocalPathFailure(path, target);
                throw new UncheckedIOException("Could not publish the pulled file to its local target.", exception);
            }
        });
    }

    @Override
    public byte[] pullFolder(String devicePath) {
        return query("pull-folder", () -> {
            registerSourcePath(devicePath);
            PullsFiles provider = pulls();
            String path = requireDevicePath(devicePath);
            try {
                return provider.pullFolder(path);
            } catch (RuntimeException exception) {
                registerPathFailure(path);
                throw exception;
            }
        });
    }

    @Override
    public MobileFileActionsContract push(String devicePath, byte[] content) {
        query("push", () -> {
            registerSourcePath(devicePath);
            PushesFiles provider = pushes();
            String path = requireDevicePath(devicePath);
            byte[] submitted = Objects.requireNonNull(content, "file content").clone();
            sensitiveProviderCall(() -> provider.pushFile(path, submitted));
            return null;
        });
        return this;
    }

    @Override
    public MobileFileActionsContract pushText(String devicePath, String content) {
        query("push-text", () -> {
            registerSourcePath(devicePath);
            if (content != null) {
                FailureTraceReporter.registerSensitiveSourceValue(content);
            }
            PushesFiles provider = pushes();
            String path = requireDevicePath(devicePath);
            String submitted = Objects.requireNonNull(content, "file text");
            byte[] encoded = submitted.getBytes(StandardCharsets.UTF_8);
            FailureTraceReporter.registerSensitiveSourceValue(submitted);
            try {
                sensitiveProviderCall(() -> provider.pushFile(path, encoded));
                return null;
            } catch (RuntimeException exception) {
                FailureTraceReporter.registerSensitiveValue(submitted);
                throw exception;
            }
        });
        return this;
    }

    @Override
    public MobileFileActionsContract pushFrom(String devicePath, Path localSource) {
        return query("push-from", () -> {
            registerSourcePath(devicePath);
            registerSourcePath(localSource == null ? null : localSource.toString());
            PushesFiles provider = pushes();
            String path = requireDevicePath(devicePath);
            Path source = Objects.requireNonNull(localSource, "local source").toAbsolutePath().normalize();
            if (!Files.isRegularFile(source)) {
                throw new IllegalArgumentException("The local source must be a regular file.");
            }
            try {
                byte[] content = Files.readAllBytes(source);
                sensitiveProviderCall(() -> provider.pushFile(path, content));
                return this;
            } catch (IOException exception) {
                registerLocalPathFailure(path, source);
                throw new UncheckedIOException("Could not read the local source file.", exception);
            }
        });
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private PullsFiles pulls() {
        if (mobile.driver() instanceof PullsFiles provider) {
            return provider;
        }
        throw unsupported("pulling files");
    }

    private PushesFiles pushes() {
        if (mobile.driver() instanceof PushesFiles provider) {
            return provider;
        }
        throw unsupported("pushing files");
    }

    private <T> T query(String operation, Supplier<T> action) {
        TraceEventRecorder.Event event = TraceEventRecorder.start(
                "mobile/files", operation, "<device-file>", mobile.traceDriver());
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile file action completed.", null, Map.of(), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "Mobile file action failed.", exception, Map.of(), List.of());
            throw exception;
        }
    }

    private static void sensitiveProviderCall(Runnable action) {
        try {
            action.run();
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveThrowable(exception);
            throw exception;
        }
    }

    private static void registerLocalPathFailure(String devicePath, Path localPath) {
        registerPathFailure(devicePath, localPath.toString());
        Path parent = localPath.getParent();
        if (parent != null && parent.getNameCount() > 1) {
            registerPathFailure(parent.toString());
        }
    }

    private static void registerPathFailure(String... paths) {
        for (String path : paths) {
            FailureTraceReporter.registerSensitiveSourceValue(path);
            FailureTraceReporter.registerSensitiveValue(path);
        }
    }

    private static void registerSourcePath(String path) {
        if (path != null && !path.isBlank()) {
            FailureTraceReporter.registerSensitiveSourceValue(path);
        }
    }

    private static String requireDevicePath(String path) {
        if (path == null || path.isBlank()) {
            throw new IllegalArgumentException("The device file path must not be blank.");
        }
        return path;
    }

    private static UnsupportedOperationException unsupported(String operation) {
        return new UnsupportedOperationException("The live Appium session does not support " + operation + ".");
    }
}
