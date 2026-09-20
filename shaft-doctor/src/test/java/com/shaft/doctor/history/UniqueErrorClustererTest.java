package com.shaft.doctor.history;

import com.shaft.doctor.analysis.DeterministicRuleEngine;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.RedactionSummary;
import org.junit.jupiter.api.Test;

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
}
