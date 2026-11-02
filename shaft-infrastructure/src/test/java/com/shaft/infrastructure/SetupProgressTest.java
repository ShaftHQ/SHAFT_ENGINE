package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SetupProgressTest {
    @Test
    void percentageUsesExactFloorArithmeticAtLongBoundaries() {
        assertEquals(99, SetupProgress.of(SetupProfile.LOCAL_AI, "VERIFYING",
                Long.MAX_VALUE - 1, Long.MAX_VALUE).percentage());
        assertEquals(100, SetupProgress.of(SetupProfile.LOCAL_AI, "READY",
                Long.MAX_VALUE, Long.MAX_VALUE).percentage());
        assertEquals(0, SetupProgress.of(SetupProfile.LOCAL_AI, "STARTING", 0, 0).percentage());
    }

    @Test
    void rejectsPercentageOrByteClaimsThatDoNotMatch() {
        assertThrows(IllegalArgumentException.class,
                () -> new SetupProgress(SetupProfile.LOCAL_AI, "VERIFYING", 1, 2, 49));
        assertThrows(IllegalArgumentException.class,
                () -> SetupProgress.of(SetupProfile.LOCAL_AI, "VERIFYING", 2, 1));
    }
}
