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
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import org.apache.logging.log4j.Level;

/** Finalized evidence-entry plan used by both schema serialization and ZIP publication. */
final class TraceArtifactManifest implements AutoCloseable {
    private static final int BUFFER_SIZE = 16 * 1024;
    private List<TraceArtifactReference> references;
    private final TraceArchiveWriter.Entry nativeEntry;
    private final Path stagedNativeTrace;
    private Map<String, byte[]> resourceBytes;
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
                                  Path stagedNativeTrace, Map<String, byte[]> resourceBytes) {
        this.references = List.copyOf(references);
        this.nativeEntry = nativeEntry;
        this.stagedNativeTrace = stagedNativeTrace;
        this.resourceBytes = Map.copyOf(resourceBytes);
    }

    static TraceArtifactManifest create(String networkJson, Map<String, byte[]> screenshots, Path nativeTrace,
                                        long maxBytes, String omissionMarker) {
        return create(networkJson, screenshots, nativeTrace, maxBytes, omissionMarker, FILE_SOURCE);
    }

    static TraceArtifactManifest create(String networkJson, Map<String, byte[]> screenshots, Path nativeTrace,
                                        long maxBytes, String omissionMarker, NativeTraceSource nativeSource) {
        return create(networkJson, screenshots, List.of(), nativeTrace, maxBytes, omissionMarker, nativeSource);
    }

    static TraceArtifactManifest create(String networkJson, Map<String, byte[]> screenshots,
                                        List<SnapshotResource> snapshots, Path nativeTrace,
                                        long maxBytes, String omissionMarker) {
        return create(networkJson, screenshots, snapshots, nativeTrace, maxBytes, omissionMarker, FILE_SOURCE);
    }

    private static TraceArtifactManifest create(String networkJson, Map<String, byte[]> screenshots,
                                                List<SnapshotResource> snapshots, Path nativeTrace,
                                                long maxBytes, String omissionMarker, NativeTraceSource nativeSource) {
        List<TraceArtifactReference> references = new ArrayList<>();
        Map<String, byte[]> resources = new java.util.LinkedHashMap<>();
        byte[] networkHar = BrowserObservabilityRecorder.networkHarJson(networkJson)
                .getBytes(StandardCharsets.UTF_8);
        boolean networkOmitted = networkHar.length > maxBytes;
        byte[] networkIntegrityBytes = networkOmitted
                ? omissionMarker.getBytes(StandardCharsets.UTF_8) : networkHar;
        Map<String, String> networkMetadata = new java.util.LinkedHashMap<>(
                omissionMetadata(networkOmitted, omissionMarker));
        networkMetadata.put("sha256", sha256(networkIntegrityBytes));
        networkMetadata.put("sizeBytes", String.valueOf(networkIntegrityBytes.length));
        references.add(new TraceArtifactReference("network", "network", "shaft-network.har",
                "application/json", networkOmitted, networkMetadata));
        screenshots.forEach((id, bytes) -> {
            boolean omitted = bytes.length > maxBytes;
            String digest = sha256(bytes);
            Map<String, String> metadata = new java.util.LinkedHashMap<>(
                    omissionMetadata(omitted, omissionMarker));
            metadata.put("sha256", digest);
            metadata.put("sizeBytes", String.valueOf(bytes.length));
            references.add(new TraceArtifactReference("screenshot-" + id, "screenshot",
                    "resources/" + digest + ".png", "image/png", omitted, metadata));
        });
        snapshots.forEach(snapshot -> {
            SeleniumTraceCapture.Result result = snapshot.result();
            byte[] bytes = snapshot.bytes();
            boolean providerOmitted = !"available".equals(result.status()) && !"truncated".equals(result.status());
            boolean omitted = providerOmitted || bytes.length > maxBytes;
            String omissionReason = providerOmitted ? result.reason() : omissionMarker;
            String digest = sha256(providerOmitted
                    ? omissionReason.getBytes(StandardCharsets.UTF_8) : bytes);
            String path = "resources/" + digest + ".html";
            Map<String, String> metadata = new java.util.LinkedHashMap<>(
                    omissionMetadata(omitted, omissionReason));
            metadata.put("sha256", digest);
            metadata.put("sizeBytes", String.valueOf(bytes.length));
            metadata.put("actionId", snapshot.actionId());
            metadata.put("phase", snapshot.phase());
            metadata.put("provider", result.provider());
            metadata.put("fidelity", result.fidelity());
            metadata.put("status", result.status());
            metadata.put("reason", result.reason());
            metadata.put("truncated", String.valueOf(result.truncated()));
            references.add(new TraceArtifactReference(snapshot.id(), "dom-snapshot", path,
                    "text/html", omitted, metadata));
            resources.putIfAbsent(path, bytes);
        });

        NativeArtifact nativeArtifact = stageNative(nativeTrace, maxBytes, omissionMarker, nativeSource);
        if (nativeArtifact.entry() != null) {
            Map<String, String> nativeMetadata = new java.util.LinkedHashMap<>(
                    omissionMetadata(nativeArtifact.omitted(), nativeArtifact.omissionReason()));
            nativeMetadata.putAll(nativeArtifact.integrity());
            references.add(new TraceArtifactReference("native-trace", "native-trace",
                    nativeArtifact.entry().name(), "application/zip", nativeArtifact.omitted(),
                    nativeMetadata));
        }
        return new TraceArtifactManifest(references, nativeArtifact.entry(), nativeArtifact.stagedPath(), resources);
    }

    List<TraceArtifactReference> references() {
        return references;
    }

    TraceArchiveWriter.Entry nativeEntry() {
        return nativeEntry;
    }

    Path stagedNativeTrace() {
        return stagedNativeTrace;
    }

    Map<String, byte[]> resourceBytes() {
        return resourceBytes;
    }

    List<String> omittedPaths() {
        return references.stream().filter(TraceArtifactReference::omitted).map(TraceArtifactReference::path).toList();
    }

    void markOmitted(List<String> paths, String reason) {
        Set<String> omitted = Set.copyOf(paths);
        references = references.stream().map(reference -> omitted.contains(reference.path()) && !reference.omitted()
                ? new TraceArtifactReference(reference.id(), reference.kind(), reference.path(), reference.mimeType(),
                true, withOmissionReason(reference.metadata(), reason)) : reference).toList();
    }

    void retainActionArtifacts(Set<String> retainedArtifactIds) {
        references = references.stream().filter(reference -> {
            boolean actionOwned = "dom-snapshot".equals(reference.kind()) || "screenshot".equals(reference.kind());
            return !actionOwned || retainedArtifactIds.contains(reference.id());
        }).toList();
        Set<String> retainedPaths = references.stream().map(TraceArtifactReference::path)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        resourceBytes = resourceBytes.entrySet().stream().filter(entry -> retainedPaths.contains(entry.getKey()))
                .collect(java.util.stream.Collectors.toUnmodifiableMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    private static Map<String, String> withOmissionReason(Map<String, String> metadata, String reason) {
        Map<String, String> updated = new java.util.LinkedHashMap<>(metadata);
        updated.put("omissionReason", reason);
        return Map.copyOf(updated);
    }

    private static NativeArtifact stageNative(Path source, long maxBytes, String omissionMarker,
                                               NativeTraceSource nativeSource) {
        if (source == null) {
            return new NativeArtifact(null, null, false, "", Map.of());
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
            byte[] stagedBytes = Files.readAllBytes(staged);
            return new NativeArtifact(TraceArchiveWriter.Entry.optionalFile(name, staged), staged, false, "",
                    integrityMetadata(stagedBytes));
        } catch (IOException ignored) {
            deleteStaged(staged);
            return omittedNative(name, "Omitted because SHAFT could not read the native Playwright trace.");
        }
    }

    private static NativeArtifact omittedNative(String name, String reason) {
        ReportManagerHelper.logDiscrete(reason, Level.WARN);
        return new NativeArtifact(TraceArchiveWriter.Entry.text(name, reason), null, true, reason,
                integrityMetadata(reason.getBytes(StandardCharsets.UTF_8)));
    }

    private static Map<String, String> integrityMetadata(byte[] bytes) {
        Map<String, String> metadata = new java.util.LinkedHashMap<>();
        metadata.put("sha256", sha256(bytes));
        metadata.put("sizeBytes", String.valueOf(bytes.length));
        return metadata;
    }

    private static Map<String, String> omissionMetadata(boolean omitted, String reason) {
        return omitted ? Map.of("omissionReason", reason) : Map.of();
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", exception);
        }
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

    record SnapshotResource(String id, String actionId, String phase, SeleniumTraceCapture.Result result,
                            byte[] bytes) {
    }

    private record NativeArtifact(TraceArchiveWriter.Entry entry, Path stagedPath, boolean omitted,
                                  String omissionReason, Map<String, String> integrity) {
        private NativeArtifact {
            integrity = integrity == null ? Map.of() : Map.copyOf(integrity);
        }
    }
}
