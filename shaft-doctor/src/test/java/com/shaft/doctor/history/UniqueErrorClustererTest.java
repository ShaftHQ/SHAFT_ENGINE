package com.shaft.doctor.history;

import com.shaft.doctor.analysis.DeterministicRuleEngine;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.RedactionSummary;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class UniqueErrorClustererTest {
    private static final Path FIXTURES = Path.of("src/test/resources/fixtures/clusters");

    @Test
    void sharedSignatureFixtureClustersTogether() {
        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(
                FIXTURES.resolve("shared-signature"), null);

        assertFalse(table.empty());
        assertEquals(1, table.clusterCount());
        assertEquals(2, table.impactedTestCount());
        ErrorClusterModels.ErrorCluster cluster = table.clusters().getFirst();
        assertEquals(2, cluster.impactedCount());
        assertEquals(2, cluster.impactedTests().size());
        assertTrue(cluster.impactedTests().stream()
                .anyMatch(test -> "hist-login".equals(test.historyId())));
        assertTrue(cluster.impactedTests().stream()
                .anyMatch(test -> "hist-checkout".equals(test.historyId())));
        assertFalse(cluster.signatureKey().isBlank());
    }

    @Test
    void distinctSignaturesDoNotMerge() {
        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(
                FIXTURES.resolve("distinct-signatures"), null);

        assertFalse(table.empty());
        assertEquals(2, table.clusterCount());
        assertEquals(2, table.impactedTestCount());
        assertEquals(1, table.clusters().get(0).impactedCount());
        assertEquals(1, table.clusters().get(1).impactedCount());
        assertFalse(table.clusters().get(0).signatureKey()
                .equals(table.clusters().get(1).signatureKey()));
    }

    @Test
    void newUniqueSignatureIsOwnCluster() {
        ErrorClusterModels.ClusterTable shared = UniqueErrorClusterer.cluster(
                FIXTURES.resolve("shared-signature"), null);
        ErrorClusterModels.ClusterTable distinct = UniqueErrorClusterer.cluster(
                FIXTURES.resolve("distinct-signatures"), null);
        String sharedKey = shared.clusters().getFirst().signatureKey();
        assertTrue(distinct.clusters().stream()
                .noneMatch(cluster -> sharedKey.equals(cluster.signatureKey())));
        assertEquals(1, distinct.clusters().stream()
                .filter(cluster -> "hist-timeout".equals(
                        cluster.impactedTests().getFirst().historyId()))
                .count());
    }

    @Test
    void emptyResultsYieldEmptyState() {
        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(
                FIXTURES.resolve("empty"), null);
        assertTrue(table.empty());
        assertEquals(0, table.clusterCount());
        assertTrue(table.clusters().isEmpty());
        assertFalse(table.emptyMessage().isBlank());
    }

    @Test
    void missingResultsYieldEmptyState() {
        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(
                Path.of("src/test/resources/fixtures/clusters/does-not-exist"), null);
        assertTrue(table.empty());
        assertTrue(table.clusters().isEmpty());
    }

    @Test
    void evidenceBundleUsesDoctorHistoricalSignatureKeys() {
        EvidenceItem first = clusteredFailure("e1", "login", "fp-shared");
        EvidenceItem second = clusteredFailure("e2", "checkout", "fp-shared");
        EvidenceItem other = clusteredFailure("e3", "timeout", "fp-other");
        EvidenceBundle bundle = new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "bundle-test",
                List.of(first, second, other),
                new RedactionSummary(List.of(), List.of(), 0),
                Map.of());

        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(bundle);
        assertEquals(2, table.clusterCount());
        ErrorClusterModels.ErrorCluster shared = table.clusters().stream()
                .filter(cluster -> "fp-shared".equals(cluster.signatureKey()))
                .findFirst()
                .orElseThrow();
        assertEquals(2, shared.impactedCount());
        assertEquals(
                DeterministicRuleEngine.clusteringKey(first, bundle.evidence()),
                shared.signatureKey());
    }

    private static EvidenceItem clusteredFailure(String id, String name, String fingerprint) {
        return new EvidenceItem(
                id,
                EvidenceCategory.ALLURE_RESULT,
                "application/json",
                "",
                "sha-" + id,
                8,
                name,
                false,
                false,
                Map.of(
                        "status", "failed",
                        "name", name,
                        "historyId", "hist-" + name,
                        "failureMessage", name + " failed",
                        "clusterFingerprint", fingerprint,
                        "signature", fingerprint),
                new EvidenceProvenance("allure-result-json", "root/" + id + ".json", "sha-" + id));
    }

    @Test
    void clusterableFailureAcceptsFailedStatusOrFingerprintOnly() {
        assertTrue(DeterministicRuleEngine.clusterableFailure(clusteredFailure("a", "login", "fp-1")));
        EvidenceItem passed = clusteredFailure("b", "home", "");
        EvidenceItem passedWithoutFingerprint = new EvidenceItem(passed.id(), passed.category(), passed.mediaType(),
                passed.relativePath(), passed.sha256(), passed.sizeBytes(), passed.content(), false, false,
                Map.of("status", "passed"), passed.provenance());
        assertFalse(DeterministicRuleEngine.clusterableFailure(passedWithoutFingerprint));
        EvidenceItem fingerprinted = new EvidenceItem(passed.id(), passed.category(), passed.mediaType(),
                passed.relativePath(), passed.sha256(), passed.sizeBytes(), passed.content(), false, false,
                Map.of("status", "passed", "clusterFingerprint", "fp-2"), passed.provenance());
        assertTrue(DeterministicRuleEngine.clusterableFailure(fingerprinted));
    }

    @Test
    void normalizeSignatureCollapsesDigitsHexAndWhitespace() {
        String first = UniqueErrorClusterer.normalizeSignature(
                "Timeout after 12 ms at 0xdeadbeef", null);
        String second = UniqueErrorClusterer.normalizeSignature(
                "Timeout after 99 ms at 0xabcdef01", "ignored");
        assertEquals(first, second);
        assertFalse(first.isBlank());
        assertEquals(20, first.length());
        assertEquals("", UniqueErrorClusterer.normalizeSignature(null, null));
        assertEquals("", UniqueErrorClusterer.normalizeSignature("  ", "  "));
        String fromTrace = UniqueErrorClusterer.normalizeSignature(null, "first line\nsecond line");
        assertFalse(fromTrace.isBlank());
        assertEquals(fromTrace, UniqueErrorClusterer.normalizeSignature(" ", "first line\nsecond"));
    }

    @Test
    void doctorJsonFailuresClusterByFingerprint(@org.junit.jupiter.api.io.TempDir Path temp) throws Exception {
        Path doctor = temp.resolve("doctor.json");
        // Shape mirrors collectDoctorEvidence: fingerprint + failed/broken status on each node.
        Files.writeString(doctor, """
                [
                  {
                    "id": "d1",
                    "status": "failed",
                    "name": "login",
                    "historyId": "hist-login",
                    "clusterFingerprint": "fp-shared",
                    "failureMessage": "click failed"
                  },
                  {
                    "id": "d2",
                    "status": "broken",
                    "name": "checkout",
                    "historyId": "hist-checkout",
                    "clusterFingerprint": "fp-shared",
                    "failureMessage": "click failed again"
                  },
                  {
                    "id": "d3",
                    "status": "failed",
                    "name": "timeout",
                    "historyId": "hist-timeout",
                    "clusterFingerprint": "fp-other",
                    "failureMessage": "timed out"
                  }
                ]
                """);
        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(null, doctor);
        assertTrue(table.warnings().stream().noneMatch(w -> w.contains("Doctor JSON not used")),
                String.valueOf(table.warnings()));
        assertFalse(table.empty(), table.emptyMessage() + " warnings=" + table.warnings());
        assertEquals(2, table.clusterCount());
        assertTrue(table.clusters().stream().anyMatch(c -> c.impactedCount() == 2));
    }

    @Test
    void malformedDoctorJsonYieldsWarningAndEmptyCluster(@org.junit.jupiter.api.io.TempDir Path temp) throws Exception {
        Path doctor = temp.resolve("bad.json");
        Files.writeString(doctor, "{not-json");
        ErrorClusterModels.ClusterTable table = UniqueErrorClusterer.cluster(
                Path.of("src/test/resources/fixtures/clusters/empty"), doctor);
        assertTrue(table.empty());
        assertFalse(table.warnings().isEmpty());
    }
}
