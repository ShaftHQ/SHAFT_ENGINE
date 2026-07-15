package com.shaft.intellij.ui;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression coverage for issue #3591 item 3: overlapping recordings from different surfaces
 * (Assistant, Guided workflow poller) must not collapse onto one process-wide boolean, and a
 * stray {@code false} must never be published while another session is still active.
 */
class ShaftRecordingActivityTest {
    // Reset before AND after each test: ShaftRecordingActivity's listener list is process-wide
    // static, and in the full module suite another test constructs a ShaftReadinessSummary that
    // registers a real (UI-touching) listener into it. Clearing only afterwards left that leaked
    // listener in place for this class's first test, where a published transition fired it and
    // threw in the headless harness. Clearing beforehand makes each test hermetic.
    @BeforeEach
    @AfterEach
    void resetState() {
        ShaftRecordingActivity.resetForTests();
    }

    private List<Boolean> captureEmissions() {
        List<Boolean> emissions = new ArrayList<>();
        Consumer<Boolean> listener = emissions::add;
        ShaftRecordingActivity.listen(listener);
        return emissions;
    }

    @Test
    void overlappingSessionsStayActiveUntilTheLastOneStops() {
        List<Boolean> emissions = captureEmissions();

        ShaftRecordingActivity.started("a");
        assertTrue(ShaftRecordingActivity.active());
        assertEquals(List.of(true), emissions);

        ShaftRecordingActivity.started("b");
        assertTrue(ShaftRecordingActivity.active());
        assertEquals(List.of(true), emissions); // no new emission: already active

        ShaftRecordingActivity.stopped("a");
        assertTrue(ShaftRecordingActivity.active()); // "b" still recording
        assertEquals(List.of(true), emissions); // no false emitted

        ShaftRecordingActivity.stopped("b");
        assertFalse(ShaftRecordingActivity.active());
        assertEquals(List.of(true, false), emissions);
    }

    @Test
    void startingTheSameKeyTwiceIsIdempotent() {
        ShaftRecordingActivity.started("a");
        ShaftRecordingActivity.started("a");
        ShaftRecordingActivity.stopped("a");

        assertFalse(ShaftRecordingActivity.active());
    }

    @Test
    void stoppingAnUnknownOrAlreadyStoppedKeyIsANoOp() {
        List<Boolean> emissions = captureEmissions();

        ShaftRecordingActivity.stopped("x");

        assertFalse(ShaftRecordingActivity.active());
        assertTrue(emissions.isEmpty());
    }

    @Test
    void aSecondSurfaceStartingAndStoppingNeverPublishesATransientFalse() {
        List<Boolean> emissions = captureEmissions();

        ShaftRecordingActivity.started("assistant");
        assertTrue(ShaftRecordingActivity.active());

        ShaftRecordingActivity.started("guided");
        assertTrue(ShaftRecordingActivity.active());

        ShaftRecordingActivity.stopped("guided");
        assertTrue(ShaftRecordingActivity.active()); // "assistant" is still recording

        assertEquals(List.of(true), emissions); // only the initial true; never a false
    }

    @Test
    void listenersOnlyFireOnTransitionEdges() {
        List<Boolean> emissions = captureEmissions();

        ShaftRecordingActivity.started("a"); // false -> true
        ShaftRecordingActivity.started("b"); // true -> true (no edge)
        ShaftRecordingActivity.started("c"); // true -> true (no edge)
        ShaftRecordingActivity.stopped("c"); // true -> true (no edge)
        ShaftRecordingActivity.stopped("b"); // true -> true (no edge)
        ShaftRecordingActivity.stopped("a"); // true -> false

        assertEquals(List.of(true, false), emissions);
    }
}
