package com.shaft.heal.internal;

import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingDecision;
import com.shaft.heal.model.HealingScore;
import com.shaft.heal.model.LocatorFingerprint;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.Mockito.mock;

public class DeterministicScorerTest {
    @Test
    public void identicalEvidenceShouldProduceRepeatableScores() {
        DeterministicScorer scorer = new DeterministicScorer(configuration(0.75, 0.10));
        LocatorFingerprint fingerprint = fingerprint("old-id", "Username");

        HealingScore first = scorer.score(fingerprint, fingerprint);
        HealingScore second = scorer.score(fingerprint, fingerprint);

        Assert.assertEquals(first, second);
        Assert.assertEquals(first.deterministicScore(), 1.0);
    }

    @Test
    public void changedIdShouldRecoverFromStableSemanticEvidence() {
        DeterministicScorer scorer = new DeterministicScorer(configuration(0.75, 0.10));

        HealingScore score = scorer.score(
                fingerprint("old-id", "Username"),
                fingerprint("new-id", "Username"));

        Assert.assertTrue(score.deterministicScore() >= 0.75);
        Assert.assertEquals(score.evidenceScores().get("stable-id-name"), 1.0);
        Assert.assertEquals(score.evidenceScores().get("accessibility"), 1.0);
    }

    @Test
    public void equalSafeCandidatesShouldFailAsAmbiguous() {
        HealingConfiguration configuration = configuration(0.75, 0.10);
        RankedCandidate first = candidate("a", 0.9);
        RankedCandidate second = candidate("b", 0.9);

        HealingDecisionEngine.DecisionResult result = HealingDecisionEngine.decide(
                List.of(first, second), configuration, true);

        Assert.assertEquals(result.decision().status(), HealingDecision.Status.AMBIGUOUS);
        Assert.assertNull(result.selected());
    }

    @Test
    public void providerScoreShouldNotOverrideMinimumDeterministicConfidence() {
        HealingConfiguration configuration = configuration(0.75, 0.10);
        RankedCandidate aiBoostedCandidate = candidate("a", 0.70, 0.98, 0.98);

        HealingDecisionEngine.DecisionResult result = HealingDecisionEngine.decide(
                List.of(aiBoostedCandidate), configuration, true);

        Assert.assertEquals(result.decision().status(), HealingDecision.Status.BELOW_THRESHOLD);
        Assert.assertNull(result.selected());
    }

    private static RankedCandidate candidate(String id, double score) {
        return candidate(id, score, null, score);
    }

    private static RankedCandidate candidate(String id, double deterministicScore, Double providerScore, double finalScore) {
        LocatorFingerprint fingerprint = fingerprint(id, "Username");
        HealingCandidate report = new HealingCandidate(
                id,
                By.id(id).toString(),
                fingerprint,
                new HealingScore(deterministicScore, null, providerScore, finalScore, Map.of("accessibility", 1.0)),
                List.of("accessibility=1.000"),
                true,
                true,
                true,
                true);
        return new RankedCandidate(mock(WebElement.class), By.id(id), report);
    }

    static LocatorFingerprint fingerprint(String id, String name) {
        return new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                "input",
                name,
                name,
                "",
                id,
                "username",
                "textbox",
                "text",
                name,
                "",
                Map.of("data-testid", "username-field"),
                Map.of("aria-label", name),
                "dom-checksum");
    }

    static HealingConfiguration configuration(double minimum, double margin) {
        return new HealingConfiguration(
                minimum,
                margin,
                Set.of("accessibility", "label", "test-id", "stable-id-name", "semantic", "dom-fingerprint"),
                List.of("data-testid"),
                true,
                Path.of("target", "shaft-heal-tests", "history.json").toAbsolutePath(),
                10,
                Duration.ofDays(30),
                false,
                false,
                false);
    }
}
