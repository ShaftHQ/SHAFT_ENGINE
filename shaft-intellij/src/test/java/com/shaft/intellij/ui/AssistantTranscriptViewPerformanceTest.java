package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Measures the O(N^2)-vs-O(N) behavior identified by the issue #3751 part 2 responsiveness audit:
 * {@code appendLocalAgentOutput}'s Verbose-mode streaming path re-formats and re-renders the ENTIRE
 * accumulated local-agent output buffer on every streamed line (HIGH findings 1 and 3,
 * {@code ShaftAssistantPanel.java:2195-2201} -> {@code AssistantTranscriptView#replaceLast} ->
 * {@code updateLastBubbleIncrementally} -> full {@code convertMarkdown} + {@code htmlPane.setText}
 * re-parse of the whole HTML document), and non-verbose milestone runs grow the transcript's
 * messages/bubbles without bound (MEDIUM finding 4). Both are exercised here exactly as the audit's
 * own "before/after measurement" section describes: driving {@link AssistantTranscriptView} directly
 * (tests already do this, see {@link AssistantTranscriptView#bubbleCreationCountForTest()}), not
 * through {@code ShaftAssistantPanel} -- this isolates the transcript-view-level fix (delta-append +
 * lazy markdown) from the separate EDT-coalescing fix, which is unit-tested on its own in
 * {@link LocalAgentOutputCoalescerTest}.
 *
 * <p>The wall-clock assertion is a generous regression ceiling only, sized well above the actual
 * measured after-fix runtime specifically so it never flakes on a slower/shared CI runner -- see the
 * class javadoc note below with the measured before/after numbers for this environment.
 *
 * <p>Before/after measurement recorded in this environment (Windows, JDK 21, single run):
 * <ul>
 *   <li>BEFORE (full re-render/re-parse per {@code replaceLast}, unbounded {@code append}): see
 *       PR description / commit message for the captured number.</li>
 *   <li>AFTER (delta-append + lazy markdown + bounded transcript cap): see PR description / commit
 *       message for the captured number.</li>
 * </ul>
 */
class AssistantTranscriptViewPerformanceTest {
    private static final int STREAMED_LINES = 2000;
    // Generous regression ceiling: sized well above (see commit message for the actual measured
    // after-fix number) so this never flakes on a slower/shared CI runner, while still catching a
    // real reintroduction of the O(N^2) whole-buffer re-render/re-parse behavior this test exists to
    // guard against.
    private static final long VERBOSE_STREAMING_CEILING_MILLIS = 15_000;

    /**
     * Mirrors {@code ShaftAssistantPanel#formatLocalAgentStreamingResponse}/{@code fencedCodeBlock}'s
     * exact shape (a constant header, then the whole accumulated buffer wrapped in a single fenced
     * code block) -- the real streaming case the audit's delta-append fix targets, and the shape
     * {@link AssistantTranscriptView}'s new fast path recognizes.
     */
    @Test
    void verboseStreamingReplaceLastScalesLinearlyNotQuadratically() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        StringBuilder rawOutput = new StringBuilder();

        long start = System.nanoTime();
        for (int i = 0; i < STREAMED_LINES; i++) {
            if (rawOutput.length() > 0) {
                rawOutput.append('\n');
            }
            rawOutput.append("output line ").append(i).append(" of the streamed local-agent run");
            String message = "**Running local assistant…**\n\n```text\n" + rawOutput + "\n```";
            view.replaceLast("assistant", message);
        }
        long elapsedMillis = (System.nanoTime() - start) / 1_000_000;

        assertAll(
                () -> assertTrue(elapsedMillis < VERBOSE_STREAMING_CEILING_MILLIS,
                        STREAMED_LINES + " streamed replaceLast() calls took " + elapsedMillis
                                + "ms, over the " + VERBOSE_STREAMING_CEILING_MILLIS
                                + "ms regression ceiling -- the whole-buffer O(N^2) re-render/re-parse "
                                + "may have regressed"),
                () -> assertEquals(1, view.bubbleCreationCountForTest(),
                        "Streaming into one placeholder bubble must never construct additional bubbles"),
                () -> assertTrue(view.markdown().contains("output line " + (STREAMED_LINES - 1)),
                        "The final rendered content must still contain the very last streamed line"),
                () -> assertTrue(view.markdown().contains("output line 0"),
                        "The final rendered content must still contain the very first streamed line"));
    }

    /**
     * Non-verbose runs surface one compact milestone bubble per output line (issue #3695) with no
     * prior cap (MEDIUM finding 4): {@code AssistantTranscriptView.messages}/{@code renderedBubbles}
     * grew unbounded across a long run. This asserts the view now applies the same 80-message cap
     * {@code ShaftAssistantChatState} already applies (issue #3622/#3629 lineage), via deterministic
     * counts rather than wall-clock -- counts don't flake on a slow CI runner.
     */
    @Test
    void nonVerboseMilestoneAppendRespectsTheTranscriptCap() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        int totalMilestones = ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION + 50;

        for (int i = 0; i < totalMilestones; i++) {
            view.append("assistant", "Milestone " + i);
        }

        assertAll(
                () -> assertEquals(ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION,
                        view.currentMessageCountForTest(),
                        "The transcript view must cap retained messages at the chat-state trim policy"),
                () -> assertEquals(totalMilestones, view.bubbleCreationCountForTest(),
                        "Every appended milestone must still construct its own bubble once -- the cap "
                                + "trims OLD bubbles afterward, it must not skip constructing new ones"),
                () -> assertTrue(view.markdown().contains("Milestone " + (totalMilestones - 1)),
                        "The most recent milestone must still be visible after trimming"),
                () -> assertTrue(!view.markdown().contains("Milestone 0"),
                        "The oldest milestones must be dropped once the cap is exceeded"));
    }
}
