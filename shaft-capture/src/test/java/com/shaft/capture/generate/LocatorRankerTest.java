package com.shaft.capture.generate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.LocatorCandidate;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocatorRankerTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void accessibilityWinsStableTieAndReportsEveryScoringDimension() {
        ElementSnapshot target = target(List.of(
                candidate(LocatorCandidate.LocatorStrategy.ID, "username", 1, true, true,
                        LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE),
                candidate(LocatorCandidate.LocatorStrategy.LABEL, "Username", 1, true, true,
                        LocatorCandidate.LocatorSignal.ACCESSIBLE,
                        LocatorCandidate.LocatorSignal.LABEL_ASSOCIATED)));

        LocatorRanker.LocatorSelection selection =
                new LocatorRanker().select(target, CaptureFixtures.context(1), true);

        assertEquals(LocatorCandidate.LocatorStrategy.LABEL, selection.selected().candidate().strategy());
        assertEquals(List.of("context=+20", "interactability=+70", "replay=0", "semanticMatch=+140",
                        "strategy=+700", "uniqueness=+240", "visibility=+80", "volatility=+190"),
                selection.selected().breakdown());
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

    @Test
    void replayAndShadowContextAffectRankingDeterministically() {
        var extensions = Map.<String, com.fasterxml.jackson.databind.JsonNode>of(
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
