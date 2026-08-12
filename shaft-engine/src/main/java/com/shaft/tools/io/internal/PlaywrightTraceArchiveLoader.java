package com.shaft.tools.io.internal;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Files;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

final class PlaywrightTraceArchiveLoader {
    private static final int MAX_ENTRIES = 2_048;
    private static final int MAX_ENTRY_BYTES = 32 * 1024 * 1024;
    private static final int MAX_ARCHIVE_BYTES = 128 * 1024 * 1024;
    private static final int MAX_ARCHIVE_FILE_BYTES = 64 * 1024 * 1024;
    private static final int MAX_RECORD_BYTES = 4 * 1024 * 1024;
    private static final int MAX_RECORDS = 1_000_000;
    private static final ObjectMapper JSON = new ObjectMapper();

    private PlaywrightTraceArchiveLoader() {
    }

    static LoadedArchive load(Path archive) throws IOException {
        return load(archive, MAX_ENTRY_BYTES, MAX_ARCHIVE_BYTES, MAX_ENTRIES, MAX_ARCHIVE_FILE_BYTES);
    }

    static LoadedArchive load(Path archive, int maximumEntryBytes, int maximumArchiveBytes, int maximumEntries)
            throws IOException {
        return load(archive, maximumEntryBytes, maximumArchiveBytes, maximumEntries, MAX_ARCHIVE_FILE_BYTES);
    }

    static LoadedArchive load(Path archive, int maximumEntryBytes, int maximumArchiveBytes, int maximumEntries,
                              int maximumArchiveFileBytes) throws IOException {
        if (maximumEntryBytes < 1 || maximumArchiveBytes < 1 || maximumEntries < 1 || maximumArchiveFileBytes < 1) {
            throw new IllegalArgumentException("Playwright trace archive limits must be positive.");
        }
        if (Files.size(archive) > maximumArchiveFileBytes) {
            throw new IOException("Playwright trace archive exceeds the " + maximumArchiveFileBytes
                    + " compressed byte limit.");
        }
        Map<String, byte[]> entries = new TreeMap<>();
        int totalBytes = 0;
        int entryCount = 0;
        try (ZipFile zip = new ZipFile(archive.toFile())) {
            var enumeration = zip.entries();
            while (enumeration.hasMoreElements()) {
                ZipEntry entry = enumeration.nextElement();
                entryCount = Math.addExact(entryCount, 1);
                if (entryCount > maximumEntries) {
                    throw new IOException("Playwright trace archive exceeds the " + maximumEntries + " entry limit.");
                }
                String name = safeEntryName(entry.getName(), entry.isDirectory());
                if (entry.isDirectory()) {
                    continue;
                }
                if (entries.containsKey(name)) {
                    throw new IOException("Playwright trace archive contains a duplicate entry: " + name);
                }
                byte[] bytes;
                try (InputStream input = zip.getInputStream(entry)) {
                    bytes = readBounded(input, maximumEntryBytes, maximumArchiveBytes - totalBytes, name);
                }
                totalBytes = Math.addExact(totalBytes, bytes.length);
                entries.put(name, bytes);
            }
        } catch (ArithmeticException exception) {
            throw new IOException("Playwright trace archive decompressed size overflowed.", exception);
        }
        if (entries.keySet().stream().noneMatch(PlaywrightTraceArchiveLoader::isTraceEntry)) {
            throw new IOException("Playwright trace archive contains no trace data.");
        }
        validateTraceData(entries);
        return new LoadedArchive(entries);
    }

    private static byte[] readBounded(InputStream input, int maximumEntryBytes, int remainingArchiveBytes, String name)
            throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        byte[] buffer = new byte[8_192];
        int total = 0;
        int read;
        while ((read = input.read(buffer)) != -1) {
            total = Math.addExact(total, read);
            if (total > maximumEntryBytes) {
                throw new IOException("Playwright trace entry exceeds the " + maximumEntryBytes + " byte limit: " + name);
            }
            if (total > remainingArchiveBytes) {
                throw new IOException("Playwright trace archive exceeds the decompressed byte limit.");
            }
            output.write(buffer, 0, read);
        }
        return output.toByteArray();
    }

    private static String safeEntryName(String name, boolean directory) throws IOException {
        String path = directory && name.endsWith("/") ? name.substring(0, name.length() - 1) : name;
        if (path.isBlank() || path.startsWith("/") || path.startsWith("\\") || path.contains("\\")
                || path.contains(":") || path.chars().anyMatch(character -> character < 32)
                || java.util.Arrays.stream(path.split("/", -1)).anyMatch(segment -> segment.isEmpty()
                || segment.equals(".") || segment.equals(".."))) {
            throw new IOException("Unsafe Playwright trace archive entry: " + name);
        }
        return name;
    }

    private static void validateTraceData(Map<String, byte[]> entries) throws IOException {
        Set<String> references = new HashSet<>();
        boolean contextOptions = false;
        int recordCount = 0;
        for (Map.Entry<String, byte[]> entry : entries.entrySet()) {
            if (!isTraceEntry(entry.getKey())) {
                continue;
            }
            byte[] bytes = entry.getValue();
            if (entry.getKey().endsWith(".stacks")) {
                if (bytes.length > MAX_RECORD_BYTES) {
                    throw new IOException("Playwright trace JSON record exceeds the " + MAX_RECORD_BYTES
                            + " byte limit in " + entry.getKey() + ".");
                }
                recordCount = Math.addExact(recordCount, 1);
                if (recordCount > MAX_RECORDS) {
                    throw new IOException("Playwright trace archive exceeds the " + MAX_RECORDS + " record limit.");
                }
                contextOptions |= validateRecord(bytes, 0, bytes.length, entry.getKey(), 1, references);
                continue;
            }
            int start = 0;
            int recordIndex = 0;
            for (int index = 0; index <= bytes.length; index++) {
                if (index < bytes.length && bytes[index] != '\n') {
                    if (index - start + 1 > MAX_RECORD_BYTES) {
                        throw new IOException("Playwright trace JSON record exceeds the " + MAX_RECORD_BYTES
                                + " byte limit in " + entry.getKey() + ".");
                    }
                    continue;
                }
                int end = index > start && bytes[index - 1] == '\r' ? index - 1 : index;
                if (end > start) {
                    recordCount = Math.addExact(recordCount, 1);
                    recordIndex++;
                    if (recordCount > MAX_RECORDS) {
                        throw new IOException("Playwright trace archive exceeds the " + MAX_RECORDS + " record limit.");
                    }
                    contextOptions |= validateRecord(bytes, start, end - start, entry.getKey(), recordIndex,
                            references);
                }
                start = index + 1;
            }
        }
        if (!contextOptions) {
            throw new IOException("Playwright trace archive has no context-options record.");
        }
        for (String reference : references) {
            if (!entries.containsKey("resources/" + reference)) {
                throw new IOException("Playwright trace archive is missing referenced resource: " + reference);
            }
        }
    }

    private static boolean validateRecord(byte[] bytes, int offset, int length, String name, int index,
                                          Set<String> references) throws IOException {
        try {
            StandardCharsets.UTF_8.newDecoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .decode(ByteBuffer.wrap(bytes, offset, length));
        } catch (CharacterCodingException exception) {
            throw new IOException("Malformed Playwright trace UTF-8 in " + name + " at record " + index + ".",
                    exception);
        }
        JsonNode record;
        try {
            record = JSON.readTree(bytes, offset, length);
        } catch (JacksonException exception) {
            throw new IOException("Malformed Playwright trace JSON in " + name + " at record " + index + ".",
                    exception);
        }
        if (!record.isObject()) {
            throw new IOException("Malformed Playwright trace JSON object in " + name + " at record " + index + ".");
        }
        collectResourceReferences(record, references);
        return "context-options".equals(record.path("type").asText());
    }

    private static void collectResourceReferences(JsonNode node, Set<String> references) {
        if (node.isObject()) {
            for (Map.Entry<String, JsonNode> property : node.properties()) {
                if (("_sha1".equals(property.getKey()) || "sha1".equals(property.getKey())
                        || property.getKey().endsWith("Sha1"))
                        && property.getValue().isString()
                        && !property.getValue().asText().isBlank()) {
                    references.add(property.getValue().asText());
                }
                collectResourceReferences(property.getValue(), references);
            }
        } else if (node.isArray()) {
            for (JsonNode value : node.values()) {
                collectResourceReferences(value, references);
            }
        }
    }

    private static boolean isTraceEntry(String name) {
        return !name.contains("/")
                && (name.endsWith(".trace") || name.endsWith(".network") || name.endsWith(".stacks"));
    }

    static final class LoadedArchive {
        private final Map<String, byte[]> entries;

        private LoadedArchive(Map<String, byte[]> entries) {
            this.entries = Map.copyOf(entries);
        }

        List<String> traceEntryNames() {
            return entries.keySet().stream().filter(PlaywrightTraceArchiveLoader::isTraceEntry).sorted().toList();
        }

        List<String> resourceEntryNames() {
            return entries.keySet().stream().filter(name -> name.startsWith("resources/")).sorted().toList();
        }

        byte[] entry(String name) {
            byte[] bytes = entries.get(name);
            return bytes == null ? null : bytes.clone();
        }

        int entrySize(String name) {
            byte[] bytes = entries.get(name);
            return bytes == null ? -1 : bytes.length;
        }
    }
}
