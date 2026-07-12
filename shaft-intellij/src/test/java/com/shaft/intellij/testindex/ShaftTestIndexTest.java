package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftTestIndexTest {
    @Test
    void recordRunMarksZeroExitCodeAsPass() {
        ShaftTestIndex index = new ShaftTestIndex();
        index.recordRun("SignInTest", 0, 1_000L);

        List<ShaftTestIndex.TestRowState> rows = index.snapshot();
        assertEquals(1, rows.size());
        assertEquals(ShaftTestIndex.Status.PASS, rows.get(0).status());
        assertEquals(0, rows.get(0).lastExitCode());
    }

    @Test
    void recordRunMarksNonZeroExitCodeAsFail() {
        ShaftTestIndex index = new ShaftTestIndex();
        index.recordRun("CheckoutTest", 1, 1_000L);

        List<ShaftTestIndex.TestRowState> rows = index.snapshot();
        assertEquals(1, rows.size());
        assertEquals(ShaftTestIndex.Status.FAIL, rows.get(0).status());
    }

    @Test
    void recordRunOverwritesThePreviousResultForTheSameTestId() {
        ShaftTestIndex index = new ShaftTestIndex();
        index.recordRun("SearchTest", 1, 1_000L);
        index.recordRun("SearchTest", 0, 2_000L);

        List<ShaftTestIndex.TestRowState> rows = index.snapshot();
        assertEquals(1, rows.size());
        assertEquals(ShaftTestIndex.Status.PASS, rows.get(0).status());
        assertEquals(2_000L, rows.get(0).lastRunAtMillis());
    }

    @Test
    void snapshotOrdersMostRecentRunFirst() {
        ShaftTestIndex index = new ShaftTestIndex();
        index.recordRun("OldestTest", 0, 1_000L);
        index.recordRun("NewestTest", 0, 3_000L);
        index.recordRun("MiddleTest", 0, 2_000L);

        List<ShaftTestIndex.TestRowState> rows = index.snapshot();
        assertEquals(List.of("NewestTest", "MiddleTest", "OldestTest"),
                rows.stream().map(ShaftTestIndex.TestRowState::testId).toList());
    }

    @Test
    void clearRemovesAllRows() {
        ShaftTestIndex index = new ShaftTestIndex();
        index.recordRun("SignInTest", 0, 1_000L);
        index.clear();

        assertTrue(index.snapshot().isEmpty());
    }

    @Test
    void recordRunIgnoresBlankOrNullTestId() {
        ShaftTestIndex index = new ShaftTestIndex();
        index.recordRun(null, 0, 1_000L);
        index.recordRun("  ", 0, 1_000L);

        assertTrue(index.snapshot().isEmpty());
    }

    @Test
    void getInstanceReturnsAFreshInstanceForNullProject() {
        ShaftTestIndex first = ShaftTestIndex.getInstance(null);
        ShaftTestIndex second = ShaftTestIndex.getInstance(null);

        first.recordRun("SignInTest", 0, 1_000L);
        assertTrue(second.snapshot().isEmpty(), "null-project instances should not share state");
    }
}
