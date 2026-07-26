package com.shaft.capture.model;

import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Issue #4026: {@code replayXpath} carries the literal XPath the in-page recorder computed and
 * self-verified for a semantic ({@code ROLE}/{@code ACCESSIBLE_NAME}/{@code LABEL}) candidate, so
 * {@code CaptureGenerator} can emit that exact string instead of re-deriving a predicate from
 * {@code expression()} in Java (the two-independently-derived-strings defect tracker #4024
 * describes). It must be additive: every existing 6-arg {@code new LocatorCandidate(...)} call
 * site across the codebase must keep compiling unchanged.
 */
class LocatorCandidateTest {
    @Test
    void legacySixArgConstructorDefaultsReplayXpathToBlank() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "button:Log in", 1, true, true, Set.of());

        assertEquals("", candidate.replayXpath());
    }

    @Test
    void sevenArgConstructorCarriesTheRecordedReplayXpath() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "alert:Something went wrong", 1, true, true,
                Set.of(), "//div[normalize-space(.)=\"Something went wrong\"]");

        assertEquals("//div[normalize-space(.)=\"Something went wrong\"]", candidate.replayXpath());
    }
}
