package com.shaft.tools.io.internal;

import com.shaft.tools.io.trace.TraceArtifactReference;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.logging.log4j.Level;

/** Finalized evidence-entry plan used by both schema serialization and ZIP publication. */
final class TraceArtifactManifest implements AutoCloseable {
    private static final int BUFFER_SIZE = 16 * 1024;
    private List<TraceArtifactReference> references;
    private final TraceArchiveWriter.Entry nativeEntry;
    private final Path stagedNativeTrace;
    private static final NativeTraceSource FILE_SOURCE = new NativeTraceSource() {
        @Override
        public boolean isRegularFile(Path path) {
            return Files.isRegularFile(path);
        }

        @Override
        public long size(Path path) throws IOException {
            return Files.size(path);
        }

        @Override
        public InputStream open(Path path) throws IOException {
            return Files.newInputStream(path);
        }
    };

    private TraceArtifactManifest(List<TraceArtifactReference> references, TraceArchiveWriter.Entry nativeEntry,
                                  Path stagedNativeTrace) {
        this.references = List.copyOf(references);
        this.nativeEntry = nativeEntry;
        this.stagedNativeTrace = stagedNativeTrace;
    }

    static TraceArtifactManifest create(String networkJson, Map<String, byte[]> screenshots, Path nativeTrace,
                                        long maxBytes, String omissionMarker) {
        return create(networkJson, screenshots, nativeTrace, maxBytes, omissionMarker, FILE_SOURCE);
    }

    static TraceArtifactManifest create(String networkJson, Map<String, byte[]> screenshots, Path nativeTrace,
                                        long maxBytes, String omissionMarker, NativeTraceSource nativeSource) {
        List<TraceArtifactReference> references = new ArrayList<>();
        byte[] networkHar = BrowserObservabilityRecorder.networkHarJson(networkJson)
                .getBytes(StandardCharsets.UTF_8);
        boolean networkOmitted = networkHar.length > maxBytes;
        references.add(new TraceArtifactReference("network", "network", "shaft-network.har",
                "application/json", networkOmitted, omissionMetadata(networkOmitted, omissionMarker)));
        screenshots.forEach((id, bytes) -> {
            boolean omitted = bytes.length > maxBytes;
            references.add(new TraceArtifactReference("screenshot-" + id, "screenshot",
                    "screenshots/" + id + ".png", "image/png", omitted,
                    omissionMetadata(omitted, omissionMarker)));
        });

        NativeArtifact nativeArtifact = stageNative(nativeTrace, maxBytes, omissionMarker, nativeSource);
        if (nativeArtifact.entry() != null) {
            references.add(new TraceArtifactReference("native-trace", "native-trace",
                    nativeArtifact.entry().name(), "application/zip", nativeArtifact.omitted(),
                    omissionMetadata(nativeArtifact.omitted(), nativeArtifact.omissionReason())));
        }
        return new TraceArtifactManifest(references, nativeArtifact.entry(), nativeArtifact.stagedPath());
    }

    List<TraceArtifactReference> references() {
        return references;
    }

    TraceArchiveWriter.Entry nativeEntry() {
        return nativeEntry;
    }

    List<String> omittedPaths() {
        return references.stream().filter(TraceArtifactReference::omitted).map(TraceArtifactReference::path).toList();
    }

    void markOmitted(List<String> paths, String reason) {
        Set<String> omitted = Set.copyOf(paths);
        references = references.stream().map(reference -> omitted.contains(reference.path()) && !reference.omitted()
                ? new TraceArtifactReference(reference.id(), reference.kind(), reference.path(), reference.mimeType(),
                true, Map.of("omissionReason", reason)) : reference).toList();
    }

    private static NativeArtifact stageNative(Path source, long maxBytes, String omissionMarker,
                                               NativeTraceSource nativeSource) {
        if (source == null) {
            return new NativeArtifact(null, null, false, "");
        }
        String name = source.getFileName() == null ? "playwright-trace.zip" : source.getFileName().toString();
        if (!nativeSource.isRegularFile(source)) {
            return omittedNative(name, "Omitted because the native Playwright trace was unavailable before staging.");
        }
        Path staged = null;
        try {
            if (nativeSource.size(source) > maxBytes) {
                return omittedNative(name, omissionMarker);
            }
            staged = Files.createTempFile("shaft-native-trace-", ".zip");
            boolean oversized = false;
            try (InputStream input = nativeSource.open(source); var output = Files.newOutputStream(staged)) {
                byte[] buffer = new byte[BUFFER_SIZE];
                long written = 0;
                int count;
                while ((count = input.read(buffer)) != -1) {
                    written += count;
                    if (written > maxBytes) {
                        oversized = true;
                        break;
                    }
                    output.write(buffer, 0, count);
                }
            }
            if (oversized) {
                Files.deleteIfExists(staged);
                return omittedNative(name, omissionMarker);
            }
            return new NativeArtifact(TraceArchiveWriter.Entry.optionalFile(name, staged), staged, false, "");
        } catch (IOException ignored) {
            deleteStaged(staged);
            return omittedNative(name, "Omitted because SHAFT could not read the native Playwright trace.");
        }
    }

    private static NativeArtifact omittedNative(String name, String reason) {
        ReportManagerHelper.logDiscrete(reason, Level.WARN);
        return new NativeArtifact(TraceArchiveWriter.Entry.text(name, reason), null, true, reason);
    }

    private static Map<String, String> omissionMetadata(boolean omitted, String reason) {
        return omitted ? Map.of("omissionReason", reason) : Map.of();
    }

    private static void deleteStaged(Path staged) {
        if (staged == null) {
            return;
        }
        try {
            Files.deleteIfExists(staged);
        } catch (IOException ignored) {
            staged.toFile().deleteOnExit();
        }
    }

    @Override
    public void close() {
        if (stagedNativeTrace == null) {
            return;
        }
        deleteStaged(stagedNativeTrace);
    }

    interface NativeTraceSource {
        boolean isRegularFile(Path path);

        long size(Path path) throws IOException;

        InputStream open(Path path) throws IOException;
    }

    private record NativeArtifact(TraceArchiveWriter.Entry entry, Path stagedPath, boolean omitted,
                                  String omissionReason) {
    }
}
