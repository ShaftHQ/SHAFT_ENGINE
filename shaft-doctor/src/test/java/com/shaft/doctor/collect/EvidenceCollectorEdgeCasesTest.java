package com.shaft.doctor.collect;

import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.RedactionSummary;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link EvidenceCollector} branches not exercised by the other collector test classes:
 * plain and zipped SHAFT diagnostics files discovered directly (not as Allure attachments),
 * imported-bundle omission gating, malformed/oversized Allure result handling, attachment
 * discovery edge cases, filename-based category inference, size-limit truncation, and the
 * explicit-allowed-root safety checks.
 */
class EvidenceCollectorEdgeCasesTest {

    // ---- SHAFT diagnostics: plain file, discovered directly (not via a zip or attachment) ----

    @Test
    void plainDiagnosticsJsonFileIsCollectedWithSchemaVersionAttribute(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("diagnostics.json"),
                "{\"schemaVersion\":\"2.0\",\"failure\":{\"message\":\"boom\"}}", StandardCharsets.UTF_8);
        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        List<EvidenceItem> diagnostics = bundle.evidence().stream()
                .filter(item -> "true".equals(item.attributes().get("diagnostics")))
                .toList();
        assertEquals(1, diagnostics.size());
        EvidenceItem item = diagnostics.getFirst();
        assertEquals("false", item.attributes().get("invalid"));
        assertEquals("2.0", item.attributes().get("schemaVersion"));
        assertEquals("shaft-diagnostics-json", item.provenance().adapter());
    }

    @Test
    void suffixedDiagnosticsJsonFileIsAlsoDetected(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("shard1-diagnostics.json"),
                "{\"schemaVersion\":\"3.0\"}", StandardCharsets.UTF_8);
        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        List<EvidenceItem> diagnostics = bundle.evidence().stream()
                .filter(item -> "true".equals(item.attributes().get("diagnostics")))
                .toList();
        assertEquals(1, diagnostics.size());
        assertEquals("3.0", diagnostics.getFirst().attributes().get("schemaVersion"));
    }

    @Test
    void malformedDiagnosticsJsonIsRetainedAsInvalidWithoutSchemaVersion(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("diagnostics.json"), "not valid json {", StandardCharsets.UTF_8);
        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        List<EvidenceItem> diagnostics = bundle.evidence().stream()
                .filter(item -> "true".equals(item.attributes().get("diagnostics")))
                .toList();
        assertEquals(1, diagnostics.size());
        assertEquals("true", diagnostics.getFirst().attributes().get("invalid"));
        assertFalse(diagnostics.getFirst().attributes().containsKey("schemaVersion"));
    }

    // ---- SHAFT diagnostics: zip file discovered directly in the input tree ----

    @Test
    void topLevelDiagnosticsZipDiscoveredDirectlyIsCollected(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        writeZip(inputDir.resolve("run-diagnostics.zip"), "sub/diagnostics.json",
                "{\"schemaVersion\":\"4.0\"}");
        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        List<EvidenceItem> diagnostics = bundle.evidence().stream()
                .filter(item -> "true".equals(item.attributes().get("diagnostics")))
                .toList();
        assertEquals(1, diagnostics.size(), "A diagnostics.json entry nested under a subdirectory must be found");
        assertEquals("4.0", diagnostics.getFirst().attributes().get("schemaVersion"));
    }

    @Test
    void diagnosticsZipWithoutADiagnosticsEntryIsOmitted(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        writeZip(inputDir.resolve("empty-diagnostics.zip"), "other.json", "{}");
        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        assertTrue(bundle.evidence().stream().noneMatch(item -> "true".equals(item.attributes().get("diagnostics"))));
        assertTrue(bundle.redaction().omittedItems() >= 1);
    }

    @Test
    void malformedDiagnosticsZipIsRetainedAsInvalidWithoutThrowing(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        // A zip with an intact local-file-header (so getNextEntry() succeeds) but whose
        // compressed entry data is cut off mid-stream: reading it forces Inflater to hit an
        // unexpected end of the DEFLATE stream, which surfaces as an IOException while
        // EvidenceCollector reads the entry, rather than a clean "no entry found" outcome.
        Files.write(inputDir.resolve("corrupt-diagnostics.zip"), truncatedZipBytes("diagnostics.json"));

        EvidenceBundle bundle = assertDoesNotThrow(() -> collect(temp, inputDir, true, true,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES, DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES),
                "A corrupt diagnostics zip must never throw during collection");

        List<EvidenceItem> diagnostics = bundle.evidence().stream()
                .filter(item -> "true".equals(item.attributes().get("diagnostics")))
                .toList();
        assertEquals(1, diagnostics.size());
        assertEquals("true", diagnostics.getFirst().attributes().get("invalid"));
        assertTrue(diagnostics.getFirst().content().contains("Malformed SHAFT diagnostics zip."));
    }

    // ---- Malformed / non-object Allure result JSON ----

    @Test
    void nonObjectAllureResultRootIsRetainedAsMalformed(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("array-result.json"), "[1,2,3]", StandardCharsets.UTF_8);
        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        List<EvidenceItem> allure = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .toList();
        assertEquals(1, allure.size());
        assertEquals("true", allure.getFirst().attributes().get("invalid"));
        assertTrue(allure.getFirst().content().contains("Malformed or truncated Allure result JSON."));
    }

    @Test
    void prettyPrintedAllureResultExceedingMaxItemBytesIsTruncated(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        // Compact JSON fits comfortably under 40 bytes; the pretty-printed form (with the
        // collector's own indentation) does not, forcing the post-serialization limit() call to
        // truncate rather than the earlier bounded file read.
        String compact = "{\"uuid\":\"x\",\"status\":\"passed\"}";
        assertTrue(compact.length() < 40, "Fixture must start out under the item byte limit: " + compact.length());
        Files.writeString(inputDir.resolve("tiny-result.json"), compact, StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, true, 40, 1000);

        List<EvidenceItem> allure = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ALLURE_RESULT)
                .toList();
        assertEquals(1, allure.size());
        EvidenceItem item = allure.getFirst();
        assertTrue(item.truncated(), "Pretty-printed inflation must trigger truncation");
        assertTrue(item.content().length() <= 40);
    }

    @Test
    void labelsWithoutTestMethodFallsBackToFullNameSuffix(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("no-labels-result.json"), """
                {
                  "uuid": "no-labels",
                  "fullName": "com.example.Suite.shouldWork",
                  "name": "shouldWork",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "labels": []
                }
                """, StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        EvidenceItem item = bundle.evidence().stream()
                .filter(evidence -> evidence.category() == EvidenceCategory.ALLURE_RESULT)
                .findFirst().orElseThrow();
        assertEquals("shouldWork", item.attributes().get("testMethod"));
    }

    // ---- Imported evidence bundles: gated categories and oversized items are omitted ----

    @Test
    void importedBundleOmitsGatedAndOversizedItemsButKeepsNormalOnes(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        EvidenceItem screenshot = new EvidenceItem("e-screenshot", EvidenceCategory.SCREENSHOT, "image/png",
                "artifacts/e-screenshot.png", "sha-screenshot", 10, null, false, false, Map.of(),
                new EvidenceProvenance("screenshot", "root-1/shot.png", "sha-screenshot"));
        EvidenceItem pageSnapshot = new EvidenceItem("e-snapshot", EvidenceCategory.PAGE_SNAPSHOT, "text/html",
                "artifacts/e-snapshot.html", "sha-snapshot", 10, null, false, false, Map.of(),
                new EvidenceProvenance("page-snapshot", "root-1/snap.html", "sha-snapshot"));
        EvidenceItem oversized = new EvidenceItem("e-oversized", EvidenceCategory.OTHER, "text/plain",
                "", "sha-oversized", 99_999_999, "oversized-marker", false, false, Map.of(),
                new EvidenceProvenance("explicit-artifact", "root-1/big.txt", "sha-oversized"));
        EvidenceItem kept = new EvidenceItem("e-kept", EvidenceCategory.OTHER, "text/plain",
                "", "sha-kept", 7, "keep-me", false, false, Map.of(),
                new EvidenceProvenance("explicit-artifact", "root-1/keep.txt", "sha-kept"));
        EvidenceBundle sourceBundle = new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION, "bundle-import-source",
                List.of(screenshot, pageSnapshot, oversized, kept),
                new RedactionSummary(List.of(), List.of(), 0), Map.of());
        new DoctorJsonCodec().write(inputDir.resolve("imported.json"), sourceBundle);

        EvidenceBundle bundle = collect(temp, inputDir, false, false, 1000, 5000);

        List<String> ids = bundle.evidence().stream().map(EvidenceItem::id).toList();
        assertEquals(List.of("e-kept"), ids, "Only the ungated, correctly-sized item should survive import");
        assertTrue(bundle.redaction().omittedItems() >= 3,
                "The screenshot, page snapshot, and oversized item must each be counted as omitted");
    }

    @Test
    void bundleLikeFileThatFailsSchemaValidationFallsThroughToOrdinaryHandling(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        // Contains the "bundleId"/"evidence" substrings the looksLikeBundle() heuristic scans
        // for, but is not a schema-valid bundle: "evidence" must be an array, not a string.
        Files.writeString(inputDir.resolve("fake-bundle.json"),
                "{\"bundleId\":\"not-a-real-bundle\",\"evidence\":\"oops-not-a-list\"}", StandardCharsets.UTF_8);

        EvidenceBundle bundle = assertDoesNotThrow(() -> collect(temp, inputDir, true, true,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES, DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES));

        assertEquals(1, bundle.evidence().size());
        assertTrue(bundle.evidence().getFirst().content().contains("not-a-real-bundle"));
    }

    // ---- Attachment discovery edge cases ----

    @Test
    void attachmentReferencingAMissingFileIsSilentlyIgnored(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "missing-attachment",
                  "name": "testMissingAttachment",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "Screenshot", "source": "does-not-exist.png", "type": "image/png"}
                  ]
                }
                """, StandardCharsets.UTF_8);

        EvidenceBundle bundle = assertDoesNotThrow(() -> collect(temp, inputDir, true, true,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES, DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES));

        assertTrue(bundle.evidence().stream().noneMatch(item -> item.category() == EvidenceCategory.SCREENSHOT));
    }

    @Test
    void attachmentReferencingANonRegularFileIsSilentlyIgnored(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.createDirectories(inputDir.resolve("not-a-file.png"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "directory-attachment",
                  "name": "testDirectoryAttachment",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "Screenshot", "source": "not-a-file.png", "type": "image/png"}
                  ]
                }
                """, StandardCharsets.UTF_8);

        EvidenceBundle bundle = assertDoesNotThrow(() -> collect(temp, inputDir, true, true,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES, DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES));

        assertTrue(bundle.evidence().stream().noneMatch(item -> item.category() == EvidenceCategory.SCREENSHOT));
    }

    @Test
    void screenshotAttachmentIsOmittedWhenIncludeScreenshotsIsFalse(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "gated-screenshot",
                  "name": "testGatedScreenshot",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "Screenshot", "source": "shot.png", "type": "image/png"}
                  ]
                }
                """, StandardCharsets.UTF_8);
        Files.write(inputDir.resolve("shot.png"), minimalPng());

        EvidenceBundle bundle = collect(temp, inputDir, false, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        assertTrue(bundle.evidence().stream().noneMatch(item -> item.category() == EvidenceCategory.SCREENSHOT));
    }

    @Test
    void pageSnapshotAttachmentIsOmittedWhenIncludePageSnapshotsIsFalse(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "gated-snapshot",
                  "name": "testGatedSnapshot",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "Page snapshot", "source": "snap.html", "type": "text/html"}
                  ]
                }
                """, StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("snap.html"), "<html><body>Snapshot</body></html>",
                StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, false, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        assertTrue(bundle.evidence().stream().noneMatch(item -> item.category() == EvidenceCategory.PAGE_SNAPSHOT));
    }

    @Test
    void pageSnapshotDetectedByTypeOrNameRatherThanExtension(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "typed-snapshot",
                  "name": "testTypedSnapshot",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "Weird name", "source": "typed.data", "type": "multipart/related"},
                    {"name": "Full Page Source dump", "source": "named.data", "type": ""}
                  ]
                }
                """, StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("typed.data"), "typed snapshot content", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("named.data"), "named snapshot content", StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        long snapshots = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.PAGE_SNAPSHOT)
                .count();
        assertEquals(2, snapshots, "Both the type- and name-based page snapshot signals must be honored");
    }

    @Test
    void attachmentNamedAsShaftLogIsCollectedAsShaftLog(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "log-attachment",
                  "name": "testLogAttachment",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "SHAFT Action History", "source": "history.data", "type": ""}
                  ]
                }
                """, StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("history.data"), "1. Clicked login button", StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        assertTrue(bundle.evidence().stream()
                .anyMatch(item -> item.category() == EvidenceCategory.SHAFT_LOG
                        && item.content() != null && item.content().contains("Clicked login button")));
    }

    @Test
    void attachmentMatchingNoKnownShapeIsIgnoredEntirely(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("test-result.json"), """
                {
                  "uuid": "unmatched-attachment",
                  "name": "testUnmatchedAttachment",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {"name": "Random Binary Blob", "source": "blob.bin", "type": "application/octet-stream"}
                  ]
                }
                """, StandardCharsets.UTF_8);
        Files.write(inputDir.resolve("blob.bin"), new byte[] {1, 2, 3, 4});

        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        // Only the ALLURE_RESULT item for the (passed, non-failing) test itself is present;
        // the unmatched attachment contributes no evidence of any category.
        assertEquals(1, bundle.evidence().size());
        assertEquals(EvidenceCategory.ALLURE_RESULT, bundle.evidence().getFirst().category());
    }

    // ---- Filename-based category inference for plain text evidence ----

    @Test
    void filenameHeuristicsInferEnvironmentDependencyAndConfigurationCategories(@TempDir Path temp)
            throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("run.log"), "log line", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("system-info.txt"), "os: windows", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("surefire-summary.txt"), "tests: 3", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("settings.properties"), "key=value", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("notes.md"), "just some notes", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("executor.json"), "{\"name\":\"jenkins\"}", StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        Map<String, EvidenceCategory> categoryBySource = new java.util.HashMap<>();
        bundle.evidence().forEach(item -> categoryBySource.put(item.provenance().sourceReference(), item.category()));

        assertEquals(EvidenceCategory.SHAFT_LOG, categoryBySource.get("root-1/input/run.log"));
        assertEquals(EvidenceCategory.ENVIRONMENT, categoryBySource.get("root-1/input/system-info.txt"));
        assertEquals(EvidenceCategory.DEPENDENCY_BUILD, categoryBySource.get("root-1/input/surefire-summary.txt"));
        assertEquals(EvidenceCategory.CONFIGURATION, categoryBySource.get("root-1/input/settings.properties"));
        assertEquals(EvidenceCategory.OTHER, categoryBySource.get("root-1/input/notes.md"));
        assertEquals(EvidenceCategory.ENVIRONMENT, categoryBySource.get("root-1/input/executor.json"));
    }

    @Test
    void malformedJsonTextFileFallsBackToRawRedactionWithoutThrowing(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("executor.json"), "{not valid json", StandardCharsets.UTF_8);

        EvidenceBundle bundle = assertDoesNotThrow(() -> collect(temp, inputDir, true, true,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES, DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES));

        assertEquals(1, bundle.evidence().size());
        assertTrue(bundle.evidence().getFirst().content().contains("{not valid json"));
    }

    @Test
    void unsupportedExtensionFileIsSkippedEntirely(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.write(inputDir.resolve("payload.bin"), new byte[] {9, 9, 9});

        EvidenceBundle bundle = collect(temp, inputDir, true, true, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES);

        assertTrue(bundle.evidence().isEmpty());
        assertEquals(0, bundle.redaction().omittedItems());
    }

    // ---- Bundle-size limits ----

    @Test
    void oncePerBundleByteLimitIsReachedSubsequentFilesAreOmittedWithoutBeingRead(@TempDir Path temp)
            throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("a-first.txt"), "X", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("z-second.txt"), "Y", StandardCharsets.UTF_8);

        EvidenceBundle bundle = collect(temp, inputDir, true, true, 1, 1);

        assertEquals(1, bundle.evidence().size(), "Only the first file fits within the 1-byte bundle budget");
        assertTrue(bundle.redaction().omittedItems() >= 1);
    }

    // ---- Explicit allowed-root safety checks ----

    @Test
    void nonExistentAllowedRootFailsFast(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("a.log"), "line", StandardCharsets.UTF_8);
        Path missingRoot = temp.resolve("does-not-exist");
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir), List.of(), List.of(temp, missingRoot), outputDir,
                true, true, 1, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES, true);

        assertThrows(IllegalArgumentException.class, () -> new EvidenceCollector().collect(request));
    }

    @Test
    void inputOutsideAllowedRootsIsRejected(@TempDir Path temp) throws IOException {
        Path outsideDir = Files.createDirectories(temp.resolve("outside-input"));
        Files.writeString(outsideDir.resolve("a.log"), "line", StandardCharsets.UTF_8);
        Path allowedRoot = Files.createDirectories(temp.resolve("allowed-root"));
        Path outputDir = Files.createDirectories(allowedRoot.resolve("output"));
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(outsideDir), List.of(), List.of(allowedRoot), outputDir,
                true, true, 1, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES, true);

        assertThrows(IllegalArgumentException.class, () -> new EvidenceCollector().collect(request));
    }

    @Test
    void historicalBundlePathThatIsNotARegularFileIsRejected(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("a.log"), "line", StandardCharsets.UTF_8);
        Path historicalDirectory = Files.createDirectories(temp.resolve("not-a-bundle-file"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir), List.of(historicalDirectory), List.of(temp), outputDir,
                true, true, 1, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES, true);

        assertThrows(IllegalArgumentException.class, () -> new EvidenceCollector().loadHistoricalBundles(request));
    }

    @Test
    void portableReferenceUsesTheMatchingRootsOneBasedIndex(@TempDir Path temp) throws IOException {
        Path unrelatedRoot = Files.createDirectories(temp.resolve("unrelated-root"));
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Files.writeString(inputDir.resolve("a.log"), "line", StandardCharsets.UTF_8);
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        // temp is the second allowed root in this list, so a file under temp (but not under
        // unrelatedRoot) must be referenced with the "root-2/" prefix.
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir), List.of(), List.of(unrelatedRoot, temp), outputDir,
                true, true, 1, DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES, true);

        EvidenceBundle bundle = new EvidenceCollector().collect(request);

        assertEquals(1, bundle.evidence().size());
        assertTrue(bundle.evidence().getFirst().provenance().sourceReference().startsWith("root-2/"),
                bundle.evidence().getFirst().provenance().sourceReference());
    }

    // ---- Helpers ----

    private static EvidenceBundle collect(
            Path root,
            Path inputDir,
            boolean includeScreenshots,
            boolean includePageSnapshots,
            long maxItemBytes,
            long maxBundleBytes) throws IOException {
        Path outputDir = Files.createDirectories(root.resolve("output-" + java.util.UUID.randomUUID()));
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir), List.of(), List.of(root), outputDir,
                includeScreenshots, includePageSnapshots, 1, maxItemBytes, maxBundleBytes, true);
        return new EvidenceCollector().collect(request);
    }

    private static void writeZip(Path path, String entryName, String content) throws IOException {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(path))) {
            zip.putNextEntry(new ZipEntry(entryName));
            zip.write(content.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
    }

    private static byte[] truncatedZipBytes(String entryName) throws IOException {
        java.io.ByteArrayOutputStream buffer = new java.io.ByteArrayOutputStream();
        String paddedContent = "{\"schemaVersion\":\"5.0\",\"padding\":\"" + "A".repeat(4_000) + "\"}";
        try (ZipOutputStream zip = new ZipOutputStream(buffer)) {
            zip.putNextEntry(new ZipEntry(entryName));
            zip.write(paddedContent.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
        byte[] fullZipBytes = buffer.toByteArray();
        // Keep the intact local-file header but cut off the back half of the compressed entry
        // data (and the central directory), so getNextEntry() succeeds but decompressing the
        // entry fails partway through.
        return java.util.Arrays.copyOf(fullZipBytes, fullZipBytes.length / 2);
    }

    private static byte[] minimalPng() throws IOException {
        java.awt.image.BufferedImage image = new java.awt.image.BufferedImage(
                4, 4, java.awt.image.BufferedImage.TYPE_INT_RGB);
        java.io.ByteArrayOutputStream output = new java.io.ByteArrayOutputStream();
        javax.imageio.ImageIO.write(image, "png", output);
        return output.toByteArray();
    }
}
