package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileEvidenceActionsContract;
import com.shaft.gui.driver.MobileEvidenceBundle;
import com.shaft.gui.driver.MobileLogError;
import com.shaft.gui.driver.MobileLogMessage;
import com.shaft.gui.driver.MobilePerformanceSample;
import com.shaft.gui.mobile.internal.MobileLogSource;
import com.shaft.gui.mobile.internal.MobileEvidenceState;
import com.shaft.gui.mobile.internal.MobilePerformanceState;
import com.shaft.gui.mobile.internal.MobileRecordingState;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.trace.TraceArtifactReference;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.HasSupportedPerformanceDataType;
import io.appium.java_client.android.ListensToLogcatMessages;
import io.appium.java_client.ios.ListensToSyslogMessages;
import org.openqa.selenium.remote.SessionId;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

final class EvidenceActions implements MobileEvidenceActionsContract {
    private static final long BYTES_PER_MEBIBYTE = 1024L * 1024L;
    private static final int MAX_METADATA_VALUE_BYTES = 4096;
    private static final byte[] OMITTED = "Omitted by SHAFT mobile evidence policy."
            .getBytes(StandardCharsets.UTF_8);
    private final MobileActions mobile;

    EvidenceActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public MobileEvidenceBundle capture(Path exactTarget) {
        registerSensitivePath(exactTarget);
        Path target = validateTarget(exactTarget);
        registerSensitivePath(target);
        AppiumDriver driver = mobile.driver();
        MobileEvidenceState.begin(driver);
        SessionId initialSessionId = driver.getSessionId();
        long maxBytes = Math.max(1L, SHAFT.Properties.reporting.traceMaxArtifactMb()) * BYTES_PER_MEBIBYTE;
        Instant capturedAt = Instant.now();
        MobileEvidenceCollector.Capture capture = MobileEvidenceCollector.collect(driver, maxBytes);

        Map<String, String> omissions = new LinkedHashMap<>(capture.omissions());
        String safeContext = redact(capture.context());
        Map<String, String> applicationMetadata = sanitizeMetadata(capture.applicationMetadata());
        Map<String, String> deviceMetadata = sanitizeMetadata(capture.deviceMetadata());
        Optional<MobileLogSource.Snapshot> logSnapshot = MobileLogSource.snapshotIfPresent(driver);
        List<MobileLogMessage> logMessages = logMessages(driver, logSnapshot, omissions);
        List<MobileLogError> logErrors = logErrors(driver, logSnapshot, omissions);
        List<MobilePerformanceSample> performance = performance(driver, omissions);
        byte[] screenshot = capture.screenshot();
        byte[] source = capture.source();

        PreparedRecording preparedRecording = prepareRecording(driver, maxBytes, omissions);
        try {
            ArchiveContent content = fit(maxBytes, capturedAt, safeContext, applicationMetadata,
                    deviceMetadata, logMessages, logErrors, performance, screenshot, source,
                    capture.sourceKind(), preparedRecording, omissions);
            AppiumDriver finalDriver = mobile.driver();
            if (finalDriver != driver || !initialSessionId.equals(finalDriver.getSessionId())) {
                throw new UnsupportedOperationException("Mobile evidence capture requires one live Appium session.");
            }
            try (MobileEvidenceArchiveWriter.StagedArchive staged = MobileEvidenceArchiveWriter.stage(
                    content.manifest(), content.entries(), maxBytes)) {
                MobileEvidenceArchiveWriter.publish(driver, staged, target, () -> {
                    AppiumDriver publishingDriver = mobile.driver();
                    if (publishingDriver != driver || !initialSessionId.equals(publishingDriver.getSessionId())) {
                        throw new UnsupportedOperationException(
                                "Mobile evidence capture requires one live Appium session.");
                    }
                });
            }
            return new MobileEvidenceBundle(capturedAt, target, safeContext, applicationMetadata,
                    deviceMetadata, content.logMessages(), content.logErrors(), content.performanceSamples(),
                    content.references(), content.omissions());
        } finally {
            preparedRecording.close();
        }
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private static ArchiveContent fit(long maxBytes, Instant capturedAt, String context,
                                      Map<String, String> applicationMetadata,
                                      Map<String, String> deviceMetadata,
                                      List<MobileLogMessage> originalMessages,
                                      List<MobileLogError> originalErrors,
                                      List<MobilePerformanceSample> originalPerformance,
                                      byte[] originalScreenshot, byte[] originalSource, String sourceKind,
                                      PreparedRecording originalRecording, Map<String, String> originalOmissions) {
        List<MobileLogMessage> messages = originalMessages;
        List<MobileLogError> errors = originalErrors;
        List<MobilePerformanceSample> performance = originalPerformance;
        byte[] screenshot = originalScreenshot;
        byte[] source = originalSource;
        PreparedRecording recording = originalRecording;
        Map<String, String> omissions = new LinkedHashMap<>(originalOmissions);

        while (true) {
            List<TraceArtifactReference> references = references(screenshot, source, sourceKind, recording);
            List<MobileEvidenceArchiveWriter.Entry> entries = entries(screenshot, source, recording);
            long entryBytes = entrySize(entries);
            Optional<byte[]> manifest = entryBytes > maxBytes ? Optional.empty()
                    : MobileEvidenceArchiveWriter.serializeBounded(manifest(capturedAt, context,
                    applicationMetadata, deviceMetadata, messages, errors, performance, references, omissions),
                    maxBytes - entryBytes);
            if (manifest.isPresent()) {
                return new ArchiveContent(manifest.orElseThrow(), entries, messages, errors,
                        performance, references, omissions);
            }
            if (source != null) {
                source = null;
                omissions.put("source", "oversized");
            } else if (screenshot != null) {
                screenshot = null;
                omissions.put("screenshot", "oversized");
            } else if (recording.available()) {
                recording = PreparedRecording.omitted();
                omissions.put("recording", "oversized");
            } else if (!performance.isEmpty()) {
                performance = List.of();
                omissions.put("performance", "oversized");
            } else if (!errors.isEmpty()) {
                errors = List.of();
                omissions.put("logErrors", "oversized");
            } else if (!messages.isEmpty()) {
                messages = List.of();
                omissions.put("logs", "oversized");
            } else {
                throw new IllegalStateException("Mobile evidence manifest exceeds its aggregate byte limit.");
            }
        }
    }

    private static List<MobileLogMessage> logMessages(AppiumDriver driver,
                                                       Optional<MobileLogSource.Snapshot> snapshot,
                                                       Map<String, String> omissions) {
        if (snapshot.isEmpty()) {
            omissions.put("logs", supportsLogs(driver) ? "not-started" : "unsupported");
            return List.of();
        }
        List<MobileLogMessage> messages = snapshot.get().messages().stream()
                .map(message -> new MobileLogMessage(message.capturedAt(), redact(message.source()),
                        redact(message.text())))
                .toList();
        if (messages.isEmpty()) {
            omissions.put("logs", "empty");
        }
        return messages;
    }

    private static List<MobileLogError> logErrors(AppiumDriver driver,
                                                   Optional<MobileLogSource.Snapshot> snapshot,
                                                   Map<String, String> omissions) {
        if (snapshot.isEmpty()) {
            omissions.put("logErrors", supportsLogs(driver) ? "not-started" : "unsupported");
            return List.of();
        }
        List<MobileLogError> errors = snapshot.get().errors().stream()
                .map(error -> new MobileLogError(error.capturedAt(), redact(error.source()),
                        redact(error.type()), redact(error.message())))
                .toList();
        if (errors.isEmpty()) {
            omissions.put("logErrors", "empty");
        }
        return errors;
    }

    private static List<MobilePerformanceSample> performance(AppiumDriver driver, Map<String, String> omissions) {
        Optional<List<MobilePerformanceSample>> snapshot = MobilePerformanceState.historyIfPresent(driver);
        if (snapshot.isEmpty()) {
            omissions.put("performance", driver instanceof HasSupportedPerformanceDataType ? "empty" : "unsupported");
            return List.of();
        }
        List<MobilePerformanceSample> samples = snapshot.get().stream()
                .map(EvidenceActions::sanitizePerformance)
                .toList();
        if (samples.isEmpty()) {
            omissions.put("performance", "empty");
        }
        return samples;
    }

    private static MobilePerformanceSample sanitizePerformance(MobilePerformanceSample sample) {
        List<String> columns = sample.columns().stream().map(EvidenceActions::redact).toList();
        if (new LinkedHashSet<>(columns).size() != columns.size()) {
            List<String> safeColumns = new ArrayList<>(columns.size());
            for (int index = 0; index < columns.size(); index++) {
                safeColumns.add("column-" + (index + 1));
            }
            columns = List.copyOf(safeColumns);
        }
        List<List<Object>> rows = sample.rows().stream()
                .map(row -> row.stream().map(EvidenceActions::sanitizeScalar).toList())
                .toList();
        return new MobilePerformanceSample(sample.capturedAt(), redact(sample.applicationId()),
                redact(sample.dataType()), columns, rows);
    }

    private static Object sanitizeScalar(Object value) {
        if (value == null) {
            return null;
        }
        String original = String.valueOf(value);
        String redacted = redact(original);
        return redacted.equals(original) ? value : redacted;
    }

    private static PreparedRecording prepareRecording(AppiumDriver driver, long maxBytes,
                                                       Map<String, String> omissions) {
        Optional<MobileRecordingState.Snapshot> snapshot = MobileRecordingState.snapshotIfPresent(driver);
        if (snapshot.isEmpty() || snapshot.get().savedRecording().isEmpty()) {
            omissions.put("recording", snapshot.isPresent() && snapshot.get().recordingInProgress()
                    ? "active" : "no-retained-recording");
            return PreparedRecording.omitted();
        }
        if (snapshot.get().recordingInProgress()) {
            omissions.put("recording", "active");
            return PreparedRecording.omitted();
        }
        MobileRecordingState.SavedRecording saved = snapshot.get().savedRecording().orElseThrow();
        if (saved.sizeBytes() > maxBytes) {
            omissions.put("recording", "oversized");
            return PreparedRecording.omitted();
        }
        Path source = saved.path();
        if (!Files.isRegularFile(source, LinkOption.NOFOLLOW_LINKS)) {
            omissions.put("recording", Files.exists(source, LinkOption.NOFOLLOW_LINKS) ? "changed" : "missing");
            return PreparedRecording.omitted();
        }
        Path staged = null;
        try {
            if (Files.size(source) != saved.sizeBytes()) {
                omissions.put("recording", "changed");
                return PreparedRecording.omitted();
            }
            staged = Files.createTempFile("shaft-mobile-evidence-recording-", ".tmp");
            registerSensitivePath(staged);
            MessageDigest digest = sha256();
            long copied = 0;
            try (InputStream input = Files.newInputStream(source);
                 OutputStream output = Files.newOutputStream(staged, StandardOpenOption.TRUNCATE_EXISTING)) {
                byte[] buffer = new byte[16 * 1024];
                int count;
                while ((count = input.read(buffer)) != -1) {
                    copied = Math.addExact(copied, count);
                    if (copied > maxBytes) {
                        omissions.put("recording", "oversized");
                        delete(staged);
                        return PreparedRecording.omitted();
                    }
                    digest.update(buffer, 0, count);
                    output.write(buffer, 0, count);
                }
            }
            if (copied != saved.sizeBytes()
                    || !HexFormat.of().formatHex(digest.digest()).equals(saved.sha256())) {
                omissions.put("recording", "changed");
                delete(staged);
                return PreparedRecording.omitted();
            }
            return new PreparedRecording(staged, copied);
        } catch (IOException | ArithmeticException exception) {
            delete(staged);
            omissions.put("recording", "missing");
            return PreparedRecording.omitted();
        }
    }

    private static List<TraceArtifactReference> references(byte[] screenshot, byte[] source,
                                                            String sourceKind, PreparedRecording recording) {
        return List.of(
                reference("screenshot", "screenshot", "artifacts/screenshot.png", "image/png", screenshot == null),
                reference("source", sourceKind, "artifacts/source.txt", "text/plain; charset=utf-8", source == null),
                reference("recording", "recording", "artifacts/recording.mp4", "video/mp4", !recording.available()));
    }

    private static TraceArtifactReference reference(String id, String kind, String path,
                                                     String mimeType, boolean omitted) {
        return new TraceArtifactReference(id, kind, path, mimeType, omitted, Map.of());
    }

    private static List<MobileEvidenceArchiveWriter.Entry> entries(byte[] screenshot, byte[] source,
                                                                    PreparedRecording recording) {
        return List.of(
                MobileEvidenceArchiveWriter.Entry.bytes("artifacts/screenshot.png",
                        screenshot == null ? OMITTED : screenshot),
                MobileEvidenceArchiveWriter.Entry.bytes("artifacts/source.txt", source == null ? OMITTED : source),
                recording.available()
                        ? MobileEvidenceArchiveWriter.Entry.file("artifacts/recording.mp4", recording.path(),
                        recording.size())
                        : MobileEvidenceArchiveWriter.Entry.bytes("artifacts/recording.mp4", OMITTED));
    }

    private static Map<String, Object> manifest(Instant capturedAt, String context,
                                   Map<String, String> applicationMetadata,
                                   Map<String, String> deviceMetadata,
                                   List<MobileLogMessage> messages, List<MobileLogError> errors,
                                   List<MobilePerformanceSample> performance,
                                   List<TraceArtifactReference> references,
                                   Map<String, String> omissions) {
        Map<String, Object> json = new LinkedHashMap<>();
        json.put("schemaVersion", "1.0");
        json.put("capturedAt", capturedAt.toString());
        json.put("context", context);
        json.put("applicationMetadata", applicationMetadata);
        json.put("deviceMetadata", deviceMetadata);
        json.put("logMessages", messages.stream().map(EvidenceActions::logMessageMap).toList());
        json.put("logErrors", errors.stream().map(EvidenceActions::logErrorMap).toList());
        json.put("performanceSamples", performance.stream().map(EvidenceActions::performanceMap).toList());
        json.put("artifacts", references.stream().map(EvidenceActions::artifactMap).toList());
        json.put("omissions", omissions);
        return json;
    }

    private static Map<String, Object> logMessageMap(MobileLogMessage message) {
        return Map.of("capturedAt", message.capturedAt().toString(), "source", message.source(),
                "text", message.text());
    }

    private static Map<String, Object> logErrorMap(MobileLogError error) {
        return Map.of("capturedAt", error.capturedAt().toString(), "source", error.source(),
                "type", error.type(), "message", error.message());
    }

    private static Map<String, Object> performanceMap(MobilePerformanceSample sample) {
        Map<String, Object> value = new LinkedHashMap<>();
        value.put("capturedAt", sample.capturedAt().toString());
        value.put("applicationId", sample.applicationId());
        value.put("dataType", sample.dataType());
        value.put("columns", sample.columns());
        value.put("rows", sample.rows());
        return value;
    }

    private static Map<String, Object> artifactMap(TraceArtifactReference reference) {
        Map<String, Object> value = new LinkedHashMap<>();
        value.put("id", reference.id());
        value.put("kind", reference.kind());
        value.put("path", reference.path());
        value.put("mimeType", reference.mimeType());
        value.put("omitted", reference.omitted());
        value.put("metadata", reference.metadata());
        return value;
    }

    private static Map<String, String> sanitizeMetadata(Map<String, String> metadata) {
        Map<String, String> sanitized = new LinkedHashMap<>();
        metadata.forEach((key, value) -> {
            if (value.length() > MAX_METADATA_VALUE_BYTES) {
                return;
            }
            String safe = redact(value);
            if (safe.length() <= MAX_METADATA_VALUE_BYTES && fitsUtf8(safe, MAX_METADATA_VALUE_BYTES)) {
                sanitized.put(key, safe);
            }
        });
        return Map.copyOf(sanitized);
    }

    private static boolean fitsUtf8(String value, int maxBytes) {
        var encoder = StandardCharsets.UTF_8.newEncoder();
        ByteBuffer output = ByteBuffer.allocate(maxBytes + 1);
        try {
            var result = encoder.encode(CharBuffer.wrap(value), output, true);
            if (result.isOverflow()) {
                return false;
            }
            if (result.isError()) {
                result.throwException();
            }
            result = encoder.flush(output);
            if (result.isOverflow()) {
                return false;
            }
            if (result.isError()) {
                result.throwException();
            }
            return output.position() <= maxBytes;
        } catch (CharacterCodingException exception) {
            return false;
        }
    }

    private static long entrySize(List<MobileEvidenceArchiveWriter.Entry> entries) {
        long total = 0;
        for (MobileEvidenceArchiveWriter.Entry entry : entries) {
            total = Math.addExact(total, entry.size());
        }
        return total;
    }

    private static void registerSensitivePath(Path path) {
        if (path == null) {
            return;
        }
        String value = path.toString();
        FailureTraceReporter.registerSensitiveValue(value);
        FailureTraceReporter.registerSensitiveSourceValue(value);
    }

    private static String redact(String value) {
        return FailureTraceReporter.redactInvocationText(value == null ? "" : value);
    }

    private static boolean supportsLogs(AppiumDriver driver) {
        return driver instanceof ListensToLogcatMessages || driver instanceof ListensToSyslogMessages;
    }

    private static Path validateTarget(Path exactTarget) {
        Path target = Objects.requireNonNull(exactTarget, "exactTarget").toAbsolutePath().normalize();
        Path parent = target.getParent();
        if (parent == null || target.getFileName() == null) {
            throw new IllegalArgumentException("Mobile evidence target must name a file with a parent directory.");
        }
        if (Files.isDirectory(target, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalArgumentException("Mobile evidence target must not be a directory.");
        }
        if (Files.exists(parent, LinkOption.NOFOLLOW_LINKS) && !Files.isDirectory(parent)) {
            throw new IllegalArgumentException("Mobile evidence target parent must be a directory.");
        }
        return target;
    }

    private static MessageDigest sha256() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    private static void delete(Path path) {
        if (path == null) {
            return;
        }
        try {
            Files.deleteIfExists(path);
        } catch (IOException ignored) {
            path.toFile().deleteOnExit();
        }
    }

    private record ArchiveContent(byte[] manifest, List<MobileEvidenceArchiveWriter.Entry> entries,
                                  List<MobileLogMessage> logMessages, List<MobileLogError> logErrors,
                                  List<MobilePerformanceSample> performanceSamples,
                                  List<TraceArtifactReference> references, Map<String, String> omissions) {
        private ArchiveContent {
            manifest = Arrays.copyOf(manifest, manifest.length);
            entries = List.copyOf(entries);
            logMessages = List.copyOf(logMessages);
            logErrors = List.copyOf(logErrors);
            performanceSamples = List.copyOf(performanceSamples);
            references = List.copyOf(references);
            omissions = Map.copyOf(omissions);
        }

        @Override
        public byte[] manifest() {
            return Arrays.copyOf(manifest, manifest.length);
        }
    }

    private record PreparedRecording(Path path, long size) implements AutoCloseable {
        private static PreparedRecording omitted() {
            return new PreparedRecording(null, 0);
        }

        private boolean available() {
            return path != null;
        }

        @Override
        public void close() {
            delete(path);
        }
    }
}
