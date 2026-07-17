package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Resolves screenshot/page-snapshot image paths indirectly referenced by a Doctor/Healer MCP
 * tool's raw JSON result (issue #3642).
 *
 * <p>{@code McpAnalysisReport} ({@code shaft-mcp}, returned by {@code doctor_analyze_failed_allure}
 * and {@code doctor_suggest_fix}) and {@code McpHealerRunResult} (returned by
 * {@code healer_run_failed_test}, nesting an {@code McpAnalysisReport} under {@code analysis})
 * never embed evidence bytes or paths directly -- confirmed against
 * {@code McpDoctorRemediationService.build()}, which only copies {@code result.bundlePath()} and
 * {@code result.jsonReportPath()} (plain strings) onto the returned report. Those two fields point
 * at files {@code DoctorAnalyzer.analyze()} (in {@code shaft-doctor}) writes next to each other:
 * {@code doctor-evidence.json} (an {@code EvidenceBundle}) and {@code doctor-report.json} (that
 * same bundle nested under a top-level {@code "bundle"} key, alongside {@code "diagnosis"}; see
 * the golden fixture {@code shaft-doctor/src/test/resources/fixtures/golden/doctor-report.json}).
 * Each bundle's {@code evidence} array holds {@code EvidenceItem} entries; screenshot/page-snapshot
 * items carry {@code category} ({@code SCREENSHOT}/{@code PAGE_SNAPSHOT}, see
 * {@code EvidenceCategory}) and a {@code relativePath} resolved against that bundle file's parent
 * directory (see {@code EvidenceCollector.collectBinary}, which writes the image to
 * {@code outputDirectory.resolve(relativePath)}).
 *
 * <p>Fails soft throughout: a blank/malformed tool result, an unreachable or unreadable evidence
 * file, or an unexpected JSON shape all just resolve to fewer (possibly zero) paths -- never an
 * exception. Callers must still verify each returned path is a readable image before rendering it;
 * this only resolves path references, it never opens image bytes.
 */
final class DoctorEvidenceImageLocator {
    private static final Set<String> IMAGE_EVIDENCE_CATEGORIES = Set.of("SCREENSHOT", "PAGE_SNAPSHOT");
    private static final Set<String> POINTER_FIELDS = Set.of("bundlePath", "jsonReportPath");

    private DoctorEvidenceImageLocator() {
    }

    /**
     * Resolves candidate screenshot/page-snapshot image paths for a Doctor/Healer tool's raw JSON
     * result text.
     *
     * @param rawEvidence raw MCP tool result text, or blank/{@code null}
     * @return resolved candidate paths, deduplicated, in encounter order; never {@code null}
     */
    static List<Path> resolveImagePaths(String rawEvidence) {
        if (rawEvidence == null || rawEvidence.isBlank()) {
            return List.of();
        }
        JsonElement root;
        try {
            root = JsonParser.parseString(rawEvidence);
        } catch (RuntimeException malformedJson) {
            return List.of();
        }
        Set<Path> pointerFiles = new LinkedHashSet<>();
        collectPointerFiles(root, pointerFiles);
        Set<Path> seen = new LinkedHashSet<>();
        List<Path> images = new ArrayList<>();
        for (Path pointerFile : pointerFiles) {
            for (Path imagePath : imagePathsFrom(pointerFile)) {
                if (seen.add(imagePath)) {
                    images.add(imagePath);
                }
            }
        }
        return List.copyOf(images);
    }

    private static void collectPointerFiles(JsonElement element, Set<Path> pointerFiles) {
        if (element == null || !element.isJsonObject() && !element.isJsonArray()) {
            return;
        }
        if (element.isJsonArray()) {
            for (JsonElement item : element.getAsJsonArray()) {
                collectPointerFiles(item, pointerFiles);
            }
            return;
        }
        JsonObject object = element.getAsJsonObject();
        for (String field : POINTER_FIELDS) {
            String value = stringField(object, field);
            if (value != null && !value.isBlank()) {
                addPath(pointerFiles, value);
            }
        }
        for (String key : object.keySet()) {
            collectPointerFiles(object.get(key), pointerFiles);
        }
    }

    private static List<Path> imagePathsFrom(Path pointerFile) {
        JsonObject rootObject = readPointerJson(pointerFile);
        if (rootObject == null) {
            return List.of();
        }
        JsonElement evidence = evidenceArrayFrom(rootObject);
        if (evidence == null) {
            return List.of();
        }
        Path parent = pointerFile.toAbsolutePath().normalize().getParent();
        if (parent == null) {
            return List.of();
        }
        List<Path> paths = new ArrayList<>();
        for (JsonElement itemElement : evidence.getAsJsonArray()) {
            collectImagePath(itemElement, parent, paths);
        }
        return paths;
    }

    private static JsonObject readPointerJson(Path pointerFile) {
        if (!Files.isRegularFile(pointerFile) || !Files.isReadable(pointerFile)) {
            return null;
        }
        JsonElement root;
        try {
            root = JsonParser.parseString(Files.readString(pointerFile, StandardCharsets.UTF_8));
        } catch (IOException | RuntimeException unreadable) {
            return null;
        }
        return root.isJsonObject() ? root.getAsJsonObject() : null;
    }

    private static JsonElement evidenceArrayFrom(JsonObject rootObject) {
        JsonElement bundleElement = rootObject.get("bundle");
        JsonObject bundle = bundleElement != null && bundleElement.isJsonObject()
                ? bundleElement.getAsJsonObject()
                : rootObject;
        JsonElement evidenceElement = bundle.get("evidence");
        return evidenceElement != null && evidenceElement.isJsonArray() ? evidenceElement : null;
    }

    private static void collectImagePath(JsonElement itemElement, Path parent, List<Path> paths) {
        if (!itemElement.isJsonObject()) {
            return;
        }
        JsonObject item = itemElement.getAsJsonObject();
        String category = stringField(item, "category");
        String relativePath = stringField(item, "relativePath");
        if (category != null && IMAGE_EVIDENCE_CATEGORIES.contains(category)
                && relativePath != null && !relativePath.isBlank()) {
            addResolvedPath(paths, parent, relativePath);
        }
    }

    private static void addPath(Set<Path> pointerFiles, String value) {
        try {
            pointerFiles.add(Path.of(value));
        } catch (RuntimeException invalidPath) {
            // Not a valid path on this platform; skip -- fail soft.
        }
    }

    private static void addResolvedPath(List<Path> paths, Path parent, String relativePath) {
        try {
            paths.add(parent.resolve(relativePath).normalize());
        } catch (RuntimeException invalidPath) {
            // Not a valid relative path; skip -- fail soft.
        }
    }

    private static String stringField(JsonObject object, String field) {
        JsonElement value = object.get(field);
        return value != null && value.isJsonPrimitive() ? value.getAsString() : null;
    }
}
