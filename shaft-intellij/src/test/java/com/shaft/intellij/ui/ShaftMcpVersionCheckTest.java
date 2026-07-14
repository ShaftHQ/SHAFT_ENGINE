package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Pure, no-IO coverage of {@link ShaftMcpVersionCheck}'s state branches (issue #3538).
 */
class ShaftMcpVersionCheckTest {
    @Test
    void blankInstalledIsNotInstalledRegardlessOfLatest() {
        assertAll(
                () -> assertEquals(ShaftMcpVersionCheck.State.NOT_INSTALLED,
                        ShaftMcpVersionCheck.check("", "10.3.20260710").state()),
                () -> assertEquals(ShaftMcpVersionCheck.State.NOT_INSTALLED,
                        ShaftMcpVersionCheck.check(null, "").state()),
                () -> assertEquals(ShaftMcpVersionCheck.State.NOT_INSTALLED,
                        ShaftMcpVersionCheck.check("   ", "10.3.20260710").state()));
    }

    @Test
    void blankOrNonVersionLikeLatestIsUnknownButNeverBlocking() {
        ShaftMcpVersionCheck.Result blankLatest = ShaftMcpVersionCheck.check("10.3.20260703", "");
        ShaftMcpVersionCheck.Result garbageLatest = ShaftMcpVersionCheck.check("10.3.20260703", "RELEASE");
        assertAll(
                () -> assertEquals(ShaftMcpVersionCheck.State.LATEST_UNKNOWN, blankLatest.state()),
                () -> assertEquals("10.3.20260703", blankLatest.installedVersion()),
                () -> assertEquals(ShaftMcpVersionCheck.State.LATEST_UNKNOWN, garbageLatest.state()));
    }

    @Test
    void nonVersionLikeInstalledIsAlsoUnknownEvenWithGoodLatest() {
        assertEquals(ShaftMcpVersionCheck.State.LATEST_UNKNOWN,
                ShaftMcpVersionCheck.check("dev-build", "10.3.20260710").state());
    }

    @Test
    void installedAtOrAboveLatestIsUpToDate() {
        assertAll(
                () -> assertEquals(ShaftMcpVersionCheck.State.UP_TO_DATE,
                        ShaftMcpVersionCheck.check("10.3.20260710", "10.3.20260710").state()),
                () -> assertEquals(ShaftMcpVersionCheck.State.UP_TO_DATE,
                        ShaftMcpVersionCheck.check("10.3.20260801", "10.3.20260710").state()));
    }

    @Test
    void installedBelowLatestIsUpgradeAvailableWithBothVersionsPreserved() {
        ShaftMcpVersionCheck.Result result = ShaftMcpVersionCheck.check("10.2.20260101", "10.3.20260710");
        assertAll(
                () -> assertEquals(ShaftMcpVersionCheck.State.UPGRADE_AVAILABLE, result.state()),
                () -> assertEquals("10.2.20260101", result.installedVersion()),
                () -> assertEquals("10.3.20260710", result.latestVersion()));
    }
}
