package com.shaft.capture.model;

import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

    @Test
    void sevenArgConstructorDefaultsRoleXpathVerifiedToFalse() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "alert:Something went wrong", 1, true, true,
                Set.of(), "//div[normalize-space(.)=\"Something went wrong\"]");

        assertFalse(candidate.roleXpathVerified());
    }

    /**
     * Issue #4239 P1.0a/ladder: {@code roleXpathVerified} records whether the recorder self-verified
     * that {@code SHAFT.GUI.Locator.hasRole(...)}'s EXACT fixed per-role XPath union (not the
     * candidate's own {@code inferredRole}-based uniquenessCount) resolves uniquely to this element
     * in the live DOM. A {@code <div role="button">} truthfully has {@code uniquenessCount == 1} via
     * inferredRole re-derivation, yet {@code hasRole(Role.BUTTON)} matches zero {@code <div>}
     * elements -- this flag is the only signal that can tell the two apart, so the ladder
     * (CaptureGenerator) must be able to gate rung 1 on it.
     */
    @Test
    void eightArgConstructorCarriesTheRecordedRoleXpathVerifiedFlag() {
        LocatorCandidate candidate = new LocatorCandidate(
                LocatorCandidate.LocatorStrategy.ROLE, "button:Real Button", 1, true, true,
                Set.of(), "", true);

        assertTrue(candidate.roleXpathVerified());
    }
}
