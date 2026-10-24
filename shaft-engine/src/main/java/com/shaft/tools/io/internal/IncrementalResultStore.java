package com.shaft.tools.io.internal;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Incremental per-test pass store with a validity-stamped merge (#7335).
 *
 * <p>{@link #record} copies every <em>passed</em> Allure result of the current run (with its attachments and
 * the fixture containers that reference it) to {@code <store>/<journey>/<testId>/} plus a {@code manifest.json}
 * holding the {@link ValidityKey}. A failure never overwrites a stored pass; it is appended to
 * {@code failures.jsonl} next to it. {@link #merge} rebuilds one results folder from the stored passes, rejecting
 * entries whose files-hash, app hash or SHAFT version differ from the current key (<em>stale</em>) and passes
 * that have a newer failure on the same files-hash (<em>flagged</em>).
 */
public final class IncrementalResultStore {
    /** Merge exit code: every stored pass was valid. */
    public static final int MERGE_OK = 0;
    /** Merge exit code: at least one stored pass was stale and must be rerun. */
    public static final int MERGE_STALE = 3;
    /** Merge exit code: at least one stored pass has a newer failure on the same files-hash. */
    public static final int MERGE_FLAGGED = 4;

    private static final String MANIFEST = "manifest.json";
    private static final String FAILURES = "failures.jsonl";
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private IncrementalResultStore() {
    }

    /**
     * Validity stamp of a stored pass.
     *
     * @param gitSha       source revision (informational; not used to reject)
     * @param appSha256    sha256 of the application binary under test, or empty
     * @param filesHash    hash of the sources the test depends on
     * @param shaftVersion SHAFT engine version
     */
    public record ValidityKey(String gitSha, String appSha256, String filesHash, String shaftVersion) {
        Map<String, String> asMap() {
            return Map.of("gitSha", gitSha, "appSha256", appSha256, "filesHash", filesHash, "shaftVersion", shaftVersion);
        }
    }

    /**
     * Outcome of a merge.
     *
     * @param merged  test ids copied into the output folder
     * @param stale   test ids rejected because their validity key no longer matches (rerun these)
     * @param flagged test ids rejected because a newer failure on the same files-hash exists
     */
    public record MergeResult(List<String> merged, List<String> stale, List<String> flagged) {
        /** Defensive immutable copies. */
        public MergeResult {
            merged = List.copyOf(merged);
            stale = List.copyOf(stale);
            flagged = List.copyOf(flagged);
        }

        /** @return {@link #MERGE_FLAGGED}, {@link #MERGE_STALE} or {@link #MERGE_OK} */
        public int exitCode() {
            return !flagged.isEmpty() ? MERGE_FLAGGED : !stale.isEmpty() ? MERGE_STALE : MERGE_OK;
        }
    }

    /**
     * Records the current run's results into the store.
     *
     * @param resultsDir  Allure results folder of the run
     * @param journeyDir  {@code <store>/<journey>} folder
     * @param runStartMs  run start; results that started earlier are leftovers and are ignored
     * @param keyForClass validity key for a test class (fully qualified name, may be empty)
     * @return number of passes stored
     * @throws IOException when the store cannot be written
     */
    public static int record(Path resultsDir, Path journeyDir, long runStartMs, Function<String, ValidityKey> keyForClass)
            throws IOException {
        int stored = 0;
        for (Path resultFile : listResults(resultsDir)) {
            JsonNode result = MAPPER.readTree(resultFile.toFile());
            long start = result.path("start").asLong(0);
            String testId = testId(result);
            if (start < runStartMs || testId.isEmpty()) {
                continue;
            }
            ValidityKey key = keyForClass.apply(testClass(result));
            Path testDir = journeyDir.resolve(testId);
            if ("passed".equals(result.path("status").asString(""))) {
                storePass(resultsDir, resultFile, result, testDir, key, start);
                stored++;
            } else {
                appendFailure(testDir, result, key, start);
            }
        }
        return stored;
    }

    /**
     * Merges the newest valid pass of every stored test into {@code outDir}.
     *
     * @param journeyDir {@code <store>/<journey>} folder
     * @param outDir     Allure results folder to fill
     * @param currentKey current validity key for a test class
     * @return merge outcome
     * @throws IOException when the store or output cannot be accessed
     */
    public static MergeResult merge(Path journeyDir, Path outDir, Function<String, ValidityKey> currentKey) throws IOException {
        List<String> merged = new ArrayList<>();
        List<String> stale = new ArrayList<>();
        List<String> flagged = new ArrayList<>();
        if (!Files.isDirectory(journeyDir)) {
            return new MergeResult(merged, stale, flagged);
        }
        Files.createDirectories(outDir);
        try (Stream<Path> dirs = Files.list(journeyDir)) {
            for (Path testDir : dirs.filter(Files::isDirectory).sorted().toList()) {
                Path manifestFile = testDir.resolve(MANIFEST);
                if (!Files.isRegularFile(manifestFile)) {
                    continue;
                }
                JsonNode manifest = MAPPER.readTree(manifestFile.toFile());
                String testId = manifest.path("testId").asString("");
                ValidityKey current = currentKey.apply(manifest.path("testClass").asString(""));
                if (isStale(manifest, current)) {
                    stale.add(testId);
                } else if (hasNewerFailure(testDir, manifest)) {
                    flagged.add(testId);
                } else {
                    copyAllButBookkeeping(testDir, outDir);
                    merged.add(testId);
                }
            }
        }
        return new MergeResult(merged, stale, flagged);
    }

    /**
     * Hashes files (path + content) in a stable order.
     *
     * @param root  project root the files are relative to
     * @param files files to hash; missing files are skipped
     * @return hex sha256
     * @throws IOException when a file cannot be read
     */
    public static String filesHash(Path root, Set<Path> files) throws IOException {
        MessageDigest digest = sha256();
        for (Path file : new TreeSet<>(files)) {
            Path absolute = root.resolve(file).normalize();
            if (Files.isRegularFile(absolute)) {
                digest.update(root.relativize(absolute).toString().replace('\\', '/').getBytes(StandardCharsets.UTF_8));
                digest.update(sha256Hex(absolute).getBytes(StandardCharsets.UTF_8));
            }
        }
        return HexFormat.of().formatHex(digest.digest());
    }

    /**
     * Collects files under {@code root} matching any of the comma-separated glob patterns.
     *
     * @param root  project root
     * @param globs comma-separated glob patterns relative to the root, e.g. {@code pom.xml,src/main/java/**}
     * @return matching files, relative to the root
     * @throws IOException when the tree cannot be walked
     */
    public static Set<Path> filesMatching(Path root, String globs) throws IOException {
        List<PathMatcher> matchers = new ArrayList<>();
        for (String glob : globs.split(",")) {
            if (!glob.isBlank()) {
                matchers.add(FileSystems.getDefault().getPathMatcher("glob:" + glob.trim()));
            }
        }
        Set<Path> matches = new LinkedHashSet<>();
        if (matchers.isEmpty() || !Files.isDirectory(root)) {
            return matches;
        }
        try (Stream<Path> walk = Files.walk(root)) {
            walk.filter(Files::isRegularFile).map(root::relativize)
                    .filter(rel -> !rel.startsWith("target") && !rel.startsWith(".git"))
                    .filter(rel -> matchers.stream().anyMatch(m -> m.matches(rel)))
                    .forEach(matches::add);
        }
        return matches;
    }

    /**
     * @param file file to hash
     * @return hex sha256 of the file content
     * @throws IOException when the file cannot be read
     */
    public static String sha256Hex(Path file) throws IOException {
        return HexFormat.of().formatHex(sha256().digest(Files.readAllBytes(file)));
    }

    static String testId(JsonNode result) {
        String historyId = result.path("historyId").asString("");
        String id = historyId.isEmpty() ? result.path("fullName").asString("") : historyId;
        return id.replaceAll("[^A-Za-z0-9._-]", "_");
    }

    static String testClass(JsonNode result) {
        for (JsonNode label : result.path("labels")) {
            if ("testClass".equals(label.path("name").asString(""))) {
                return label.path("value").asString("");
            }
        }
        return "";
    }

    private static boolean isStale(JsonNode manifest, ValidityKey current) {
        return !manifest.path("filesHash").asString("").equals(current.filesHash())
                || !manifest.path("appSha256").asString("").equals(current.appSha256())
                || !manifest.path("shaftVersion").asString("").equals(current.shaftVersion());
    }

    private static boolean hasNewerFailure(Path testDir, JsonNode manifest) throws IOException {
        Path failures = testDir.resolve(FAILURES);
        if (!Files.isRegularFile(failures)) {
            return false;
        }
        String filesHash = manifest.path("filesHash").asString("");
        long passStart = manifest.path("start").asLong(0);
        for (String line : Files.readAllLines(failures, StandardCharsets.UTF_8)) {
            if (line.isBlank()) {
                continue;
            }
            JsonNode failure = MAPPER.readTree(line);
            if (sameHash(filesHash, failure.path("filesHash").asString("")) && failure.path("start").asLong(0) > passStart) {
                return true;
            }
        }
        return false;
    }

    private static void storePass(Path resultsDir, Path resultFile, JsonNode result, Path testDir, ValidityKey key, long start)
            throws IOException {
        Path failures = testDir.resolve(FAILURES);
        byte[] keptFailures = Files.isRegularFile(failures) ? Files.readAllBytes(failures) : null;
        if (Files.isDirectory(testDir)) {
            try (Stream<Path> old = Files.list(testDir)) {
                for (Path file : old.toList()) {
                    Files.deleteIfExists(file);
                }
            }
        }
        Files.createDirectories(testDir);
        Files.copy(resultFile, testDir.resolve(String.valueOf(resultFile.getFileName())), StandardCopyOption.REPLACE_EXISTING);
        Set<String> sources = new LinkedHashSet<>();
        collectAttachmentSources(result, sources);
        copyReferencingContainers(resultsDir, result.path("uuid").asString(""), testDir, sources);
        copyAttachments(resultsDir, sources, testDir);
        if (keptFailures != null) {
            Files.write(failures, keptFailures);
        }
        ObjectNode manifest = MAPPER.createObjectNode();
        manifest.put("testId", testId(result));
        manifest.put("testClass", testClass(result));
        manifest.put("start", start);
        manifest.put("recordedAt", System.currentTimeMillis());
        key.asMap().forEach(manifest::put);
        Files.writeString(testDir.resolve(MANIFEST), MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(manifest),
                StandardCharsets.UTF_8);
    }

    private static void copyReferencingContainers(Path resultsDir, String uuid, Path testDir, Set<String> sources)
            throws IOException {
        for (Path container : listFiles(resultsDir, "-container.json")) {
            JsonNode node = MAPPER.readTree(container.toFile());
            boolean references = false;
            for (JsonNode child : node.path("children")) {
                references |= uuid.equals(child.asString(""));
            }
            if (references) {
                Files.copy(container, testDir.resolve(String.valueOf(container.getFileName())), StandardCopyOption.REPLACE_EXISTING);
                collectAttachmentSources(node, sources);
            }
        }
    }

    private static void copyAttachments(Path resultsDir, Set<String> sources, Path testDir) throws IOException {
        Path base = resultsDir.normalize();
        for (String source : sources) {
            Path attachment = base.resolve(source).normalize();
            if (attachment.startsWith(base) && Files.isRegularFile(attachment)) {
                Files.copy(attachment, testDir.resolve(String.valueOf(attachment.getFileName())), StandardCopyOption.REPLACE_EXISTING);
            }
        }
    }

    private static boolean sameHash(String expected, String actual) {
        return MessageDigest.isEqual(expected.getBytes(StandardCharsets.UTF_8), actual.getBytes(StandardCharsets.UTF_8));
    }

    private static void appendFailure(Path testDir, JsonNode result, ValidityKey key, long start) throws IOException {
        Files.createDirectories(testDir);
        ObjectNode failure = MAPPER.createObjectNode();
        failure.put("testId", testId(result));
        failure.put("status", result.path("status").asString(""));
        failure.put("start", start);
        key.asMap().forEach(failure::put);
        Files.writeString(testDir.resolve(FAILURES), MAPPER.writeValueAsString(failure) + "\n", StandardCharsets.UTF_8,
                StandardOpenOption.CREATE, StandardOpenOption.APPEND);
    }

    private static void collectAttachmentSources(JsonNode node, Set<String> sources) {
        for (JsonNode attachment : node.path("attachments")) {
            String source = attachment.path("source").asString("");
            if (!source.isEmpty()) {
                sources.add(source);
            }
        }
        for (String nested : List.of("steps", "befores", "afters")) {
            for (JsonNode child : node.path(nested)) {
                collectAttachmentSources(child, sources);
            }
        }
    }

    private static void copyAllButBookkeeping(Path testDir, Path outDir) throws IOException {
        try (Stream<Path> files = Files.list(testDir)) {
            for (Path file : files.filter(Files::isRegularFile).toList()) {
                String name = String.valueOf(file.getFileName());
                if (!MANIFEST.equals(name) && !FAILURES.equals(name)) {
                    Files.copy(file, outDir.resolve(name), StandardCopyOption.REPLACE_EXISTING);
                }
            }
        }
    }

    private static List<Path> listResults(Path resultsDir) throws IOException {
        return listFiles(resultsDir, "-result.json");
    }

    private static List<Path> listFiles(Path dir, String suffix) throws IOException {
        if (!Files.isDirectory(dir)) {
            return List.of();
        }
        try (Stream<Path> files = Files.list(dir)) {
            return files.filter(p -> String.valueOf(p.getFileName()).endsWith(suffix)).sorted().toList();
        }
    }

    private static MessageDigest sha256() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 unavailable", e);
        }
    }
}
