package com.shaft.capture.generate;

import tools.jackson.databind.ObjectMapper;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.LocatorCandidate;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocatorRankerTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    /**
     * Issue #4271 replaced this case's original expectation. It previously asserted that an
     * accessibility-signalled LABEL candidate outranked a unique stable id purely on additive score.
     * Under the tiered policy a bare LABEL carries no self-verified evidence at all, so it is not
     * emittable, while the unique stable id is tier 1 -- the score no longer decides across that
     * boundary. The original breakdown assertion is kept verbatim against the LABEL alternative, so
     * the "every scoring dimension is still reported" coverage is preserved rather than dropped.
     */
    @Test
    void uniqueIdWinsTheTierWhileEveryScoringDimensionIsStillReported() {
        ElementSnapshot target = target(List.of(
                candidate(LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE),
                candidate(LocatorCandidate.LocatorStrategy.LABEL, "Username", 1, true, true,
                        LocatorCandidate.LocatorSignal.ACCESSIBLE,
                        LocatorCandidate.LocatorSignal.LABEL_ASSOCIATED)));

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);

        assertEquals(LocatorCandidate.LocatorStrategy.ID, selection.selected().candidate().strategy());
        assertEquals(LocatorPolicy.Tier.UNIQUE_ID, selection.selected().tier().orElseThrow());
        assertEquals(List.of("context=+20", "interactability=+70", "replay=0", "semanticMatch=+140",
                        "strategy=+700", "uniqueness=+240", "visibility=+80", "volatility=+190"),
                selection.alternatives().getFirst().breakdown());
    }

    @Test
    void userProvidedStableLocatorCanOutrankVolatileSemanticCandidate() {
        ElementSnapshot target = target(List.of(
                candidate(LocatorCandidate.LocatorStrategy.ROLE, "textbox:Username", 3, true, false,
                        LocatorCandidate.LocatorSignal.DYNAMIC_VALUE),
                candidate(LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true,
                        LocatorCandidate.LocatorSignal.USER_PROVIDED,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE)));

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);

        assertEquals(LocatorCandidate.LocatorStrategy.ID, selection.selected().candidate().strategy());
    }

    /**
     * Issue #4271 acceptance (a): a unique, stable id outranks an equally well-verified ARIA role.
     * Both are first-class emittable evidence; the owner policy keeps unique ids first.
     */
    @Test
    void uniqueStableIdOutranksAnEquallyVerifiedRoleCandidate() {
        ElementSnapshot target = target(List.of(
                verifiedRole("textbox:Username"),
                candidate(LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE)));

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);

        assertEquals(LocatorPolicy.Tier.UNIQUE_ID, selection.selected().tier().orElseThrow());
        assertEquals(LocatorCandidate.LocatorStrategy.ID, selection.selected().candidate().strategy());
    }

    /**
     * Issue #4271 acceptance (b): a verified ARIA role outranks a plain self-verified XPath, even
     * when the XPath candidate carries a higher additive score.
     */
    @Test
    void verifiedRoleOutranksAPlainSelfVerifiedXpathFallback() {
        ElementSnapshot target = target(List.of(
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.LABEL, "Username", 1, true, true,
                        Set.of(LocatorCandidate.LocatorSignal.USER_PROVIDED),
                        "//label[normalize-space(.)=\"Username\"]/input"),
                verifiedRole("textbox:Username")));

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);

        assertEquals(LocatorPolicy.Tier.VERIFIED_ROLE, selection.selected().tier().orElseThrow());
    }

    /**
     * Issue #4271, the F2 regression guard (issue #4239 F2): under the old purely additive scorer a
     * {@code USER_PROVIDED} signal was worth +1000 and could carry a positional, generated CSS
     * selector past a unique stable id outright. Tiers are lexicographic, so no signal weight can
     * promote a candidate across a tier boundary -- the CSS candidate is not emittable at all, and
     * its score is only ever consulted to order it against other non-emittable evidence.
     */
    @Test
    void noSignalWeightCanPromoteIneligibleEvidenceAcrossATierBoundary() {
        LocatorCandidate pinnedCss = candidate(LocatorCandidate.LocatorStrategy.CSS, "form > button", 1, true, true,
                LocatorCandidate.LocatorSignal.USER_PROVIDED,
                LocatorCandidate.LocatorSignal.GENERATED);
        ElementSnapshot target = target(List.of(
                candidate(LocatorCandidate.LocatorStrategy.ID, "submit", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE),
                pinnedCss));

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);

        assertEquals(LocatorCandidate.LocatorStrategy.ID, selection.selected().candidate().strategy());
        assertTrue(selection.selected().score()
                        < selection.alternatives().getFirst().score(),
                "the pinned CSS candidate must still out-SCORE the winner -- proving the tier, not the "
                        + "score, is what decided the selection");
        assertEquals(Optional.empty(), selection.alternatives().getFirst().tier());
    }

    /**
     * Issue #4271 acceptance (c): identical inputs always produce identical output, and the result
     * does not depend on the order the recorder happened to emit the evidence in.
     */
    @Test
    void selectionIsDeterministicAndIndependentOfCandidateInputOrder() {
        List<LocatorCandidate> candidates = List.of(
                verifiedRole("textbox:Username"),
                candidate(LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE),
                candidate(LocatorCandidate.LocatorStrategy.CSS, "form > input", 1, true, false,
                        LocatorCandidate.LocatorSignal.GENERATED));

        LocatorRanker.LocatorSelection first =
                new LocatorRanker().select(target(candidates), CaptureFixtures.context(1), true);
        LocatorRanker.LocatorSelection repeated =
                new LocatorRanker().select(target(candidates), CaptureFixtures.context(1), true);
        LocatorRanker.LocatorSelection reversed = new LocatorRanker()
                .select(target(candidates.reversed()), CaptureFixtures.context(1), true);

        assertEquals(order(first), order(repeated));
        assertEquals(order(first), order(reversed));
        assertEquals(List.of("ID", "ROLE", "CSS"), order(first));
    }

    private static List<String> order(LocatorRanker.LocatorSelection selection) {
        return java.util.stream.Stream
                .concat(java.util.stream.Stream.of(selection.selected()), selection.alternatives().stream())
                .map(scored -> scored.candidate().strategy().name())
                .toList();
    }

    private static LocatorCandidate verifiedRole(String expression) {
        return new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE, expression, 1, true, true,
                Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "//input[@id=\"username\"]", true);
    }

    @Test
    void replayAndShadowContextAffectRankingDeterministically() {
        var extensions = Map.<String, tools.jackson.databind.JsonNode>of(
                "shadowHosts", JSON.valueToTree(List.of("#host")),
                "locatorReplay", JSON.valueToTree(Map.of(
                        "#username", "PASSED",
                        "//input[@name='username']", "FAILED")));
        EventContext context = new EventContext(
                1,
                CaptureFixtures.STARTED,
                new com.shaft.capture.model.PageContext(
                        "https://example.test", "Example", "window-1", List.of("frame-1"), 100, 100),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                extensions);
        ElementSnapshot target = target(List.of(
                candidate(LocatorCandidate.LocatorStrategy.CSS, "#username", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE),
                candidate(LocatorCandidate.LocatorStrategy.XPATH, "//input[@name='username']", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE)));

        LocatorRanker.LocatorSelection selection = new LocatorRanker().select(target, context, true);

        assertEquals(LocatorCandidate.LocatorStrategy.CSS, selection.selected().candidate().strategy());
        assertTrue(selection.selected().breakdown().contains("context=+50"));
        assertTrue(selection.selected().breakdown().contains("replay=+180"));
    }

    private static ElementSnapshot target(List<LocatorCandidate> candidates) {
        return new ElementSnapshot(
                "username",
                "input",
                "textbox",
                "Username",
                "Username",
                Map.of("id", "username", "name", "username"),
                new ArrayList<>(candidates),
                true,
                true,
                false);
    }

    private static LocatorCandidate candidate(
            LocatorCandidate.LocatorStrategy strategy,
            String expression,
            int uniqueness,
            boolean visible,
            boolean stable,
            LocatorCandidate.LocatorSignal... signals) {
        return new LocatorCandidate(strategy, expression, uniqueness, visible, stable, Set.of(signals));
    }
}
