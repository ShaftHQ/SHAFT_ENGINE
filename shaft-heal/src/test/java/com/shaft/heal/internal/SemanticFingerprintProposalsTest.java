package com.shaft.heal.internal;

import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;
import com.shaft.heal.model.LocatorFingerprint;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

/**
 * Issue #5820: heal suggestions follow SemanticLocatorStrategy FR-1; structural only when
 * semantic fingerprint fields are empty; evidence + confidence retained (suggestions only).
 */
public class SemanticFingerprintProposalsTest {

    @Test
    public void semanticFingerprintPrefersRoleBeforeTestIdAndId() {
        LocatorFingerprint fingerprint = new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                "button",
                "Save",
                "",
                "Save",
                "save-btn",
                "",
                "button",
                "submit",
                "",
                "",
                Map.of("data-testid", "save"),
                Map.of(),
                "checksum");

        List<SemanticFingerprintProposals.Proposal> proposals =
                SemanticFingerprintProposals.suggestions(fingerprint, false);

        Assert.assertFalse(proposals.isEmpty());
        Assert.assertEquals(proposals.getFirst().strategy(), SemanticLocatorStrategy.ROLE);
        Assert.assertEquals(proposals.getFirst().confidence(), 1.0);
        Assert.assertTrue(proposals.stream().anyMatch(p -> p.strategy() == SemanticLocatorStrategy.TEST_ID));
        Assert.assertTrue(proposals.stream().anyMatch(p -> p.strategy() == SemanticLocatorStrategy.ID));
        int roleIndex = indexOf(proposals, SemanticLocatorStrategy.ROLE);
        int testIdIndex = indexOf(proposals, SemanticLocatorStrategy.TEST_ID);
        int idIndex = indexOf(proposals, SemanticLocatorStrategy.ID);
        Assert.assertTrue(roleIndex < testIdIndex && testIdIndex < idIndex);
    }

    @Test
    public void emptySemanticFingerprintEmitsStructuralOnly() {
        LocatorFingerprint fingerprint = new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                "div",
                "",
                "",
                "",
                "widget-9",
                "",
                "",
                "",
                "",
                "",
                Map.of(),
                Map.of(),
                "checksum");

        Assert.assertFalse(SemanticFingerprintProposals.hasSemanticFields(fingerprint));
        List<SemanticFingerprintProposals.Proposal> proposals =
                SemanticFingerprintProposals.suggestions(fingerprint, false);

        Assert.assertFalse(proposals.isEmpty());
        Assert.assertTrue(proposals.stream().noneMatch(p -> p.strategy().isSemantic()
                && p.strategy() != SemanticLocatorStrategy.TEST_ID));
        Assert.assertEquals(proposals.getFirst().strategy(), SemanticLocatorStrategy.ID);
        Assert.assertTrue(proposals.getFirst().confidence() <= SemanticLocatorStrategy.ID.baseConfidence());
    }

    @Test
    public void proposalRetainsEvidenceMapWithConfidence() {
        LocatorFingerprint fingerprint = new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                "input",
                "Email",
                "Email",
                "",
                "",
                "email",
                "textbox",
                "email",
                "",
                "",
                Map.of(),
                Map.of(),
                "");

        SemanticFingerprintProposals.Proposal first =
                SemanticFingerprintProposals.suggestions(fingerprint, false).getFirst();
        Map<String, Object> evidence = first.toEvidenceMap();
        Assert.assertEquals(evidence.get("strategy"), SemanticLocatorStrategy.ROLE.name());
        Assert.assertTrue(((Number) evidence.get("confidence")).doubleValue() > 0);
        Assert.assertTrue(evidence.get("locator").toString().length() > 0);
    }

    private static int indexOf(List<SemanticFingerprintProposals.Proposal> proposals, SemanticLocatorStrategy strategy) {
        for (int i = 0; i < proposals.size(); i++) {
            if (proposals.get(i).strategy() == strategy) {
                return i;
            }
        }
        Assert.fail("strategy missing: " + strategy);
        return -1;
    }
}
