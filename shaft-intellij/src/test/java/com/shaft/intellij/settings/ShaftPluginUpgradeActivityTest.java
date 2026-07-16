package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises the upgrade-decision logic and the {@code checkForUpgrade} orchestration in isolation
 * from a real {@link com.intellij.openapi.startup.ProjectActivity} / running IDE, mirroring the
 * constructor-injection pattern already used by {@link ShaftPluginResetServiceTest}.
 */
class ShaftPluginUpgradeActivityTest {

    @Test
    void shouldResetForUpgradeTreatsABlankOrMissingStoredVersionAsAFreshInstall() {
        assertAll(
                () -> assertEquals(ShaftPluginUpgradeActivity.UpgradeDecision.FRESH_INSTALL,
                        ShaftPluginUpgradeActivity.shouldResetForUpgrade(null, "1.2.3")),
                () -> assertEquals(ShaftPluginUpgradeActivity.UpgradeDecision.FRESH_INSTALL,
                        ShaftPluginUpgradeActivity.shouldResetForUpgrade("", "1.2.3")),
                () -> assertEquals(ShaftPluginUpgradeActivity.UpgradeDecision.FRESH_INSTALL,
                        ShaftPluginUpgradeActivity.shouldResetForUpgrade("   ", "1.2.3")));
    }

    @Test
    void shouldResetForUpgradeTreatsADifferentStoredVersionAsAnUpgrade() {
        assertEquals(ShaftPluginUpgradeActivity.UpgradeDecision.UPGRADED,
                ShaftPluginUpgradeActivity.shouldResetForUpgrade("1.2.2", "1.2.3"));
    }

    @Test
    void shouldResetForUpgradeTreatsAMatchingStoredVersionAsUnchanged() {
        assertEquals(ShaftPluginUpgradeActivity.UpgradeDecision.UNCHANGED,
                ShaftPluginUpgradeActivity.shouldResetForUpgrade("1.2.3", "1.2.3"));
    }

    @Test
    void checkForUpgradeOnAFreshInstallRecordsTheRunningVersionWithoutResetting() {
        ShaftSettingsState settingsState = new ShaftSettingsState();
        boolean[] resetRan = {false};
        ShaftPluginResetService resetService = new ShaftPluginResetService(
                () -> resetRan[0] = true, () -> CompletableFuture.completedFuture(null), () -> { }, List::of, () -> { });

        ShaftPluginUpgradeActivity.checkForUpgrade("1.2.3", settingsState, resetService);

        assertAll(
                () -> assertFalse(resetRan[0], "A fresh install must not trigger a reset"),
                () -> assertEquals("1.2.3", settingsState.getState().lastSeenPluginVersion));
    }

    @Test
    void checkForUpgradeOnAVersionChangeResetsStaleStateAndRecordsTheNewVersionAfterward() {
        ShaftSettingsState settingsState = new ShaftSettingsState();
        settingsState.getState().lastSeenPluginVersion = "1.2.2";
        settingsState.getState().mcpCommand = "old-shaft-mcp-command";
        settingsState.getState().mcpSetupComplete = true;
        boolean[] resetRan = {false};
        // Mirror the production wiring (ShaftPluginResetService's no-arg constructor uses
        // resetSettings(ShaftSettingsState.getInstance()) as its settingsReset step) so this proves the
        // real interaction: resetSettings() wipes lastSeenPluginVersion back to "" via
        // factoryDefaults(), and checkForUpgrade must still land on the running version afterward.
        ShaftPluginResetService resetService = new ShaftPluginResetService(
                () -> {
                    resetRan[0] = true;
                    ShaftPluginResetService.resetSettings(settingsState);
                },
                () -> CompletableFuture.completedFuture(null), () -> { }, List::of, () -> { });

        ShaftPluginUpgradeActivity.checkForUpgrade("1.2.3", settingsState, resetService);

        assertAll(
                () -> assertTrue(resetRan[0], "A version change must trigger a reset"),
                () -> assertEquals("", settingsState.getState().mcpCommand,
                        "The stale mcpCommand must not survive an upgrade reset"),
                () -> assertFalse(settingsState.getState().mcpSetupComplete),
                () -> assertEquals("1.2.3", settingsState.getState().lastSeenPluginVersion,
                        "The running version must be persisted after the reset, not wiped by it"));
    }

    @Test
    void checkForUpgradeWhenVersionsMatchDoesNothing() {
        ShaftSettingsState settingsState = new ShaftSettingsState();
        settingsState.getState().lastSeenPluginVersion = "1.2.3";
        boolean[] resetRan = {false};
        ShaftPluginResetService resetService = new ShaftPluginResetService(
                () -> resetRan[0] = true, () -> CompletableFuture.completedFuture(null), () -> { }, List::of, () -> { });

        ShaftPluginUpgradeActivity.checkForUpgrade("1.2.3", settingsState, resetService);

        assertAll(
                () -> assertFalse(resetRan[0], "Matching versions must not trigger a reset"),
                () -> assertEquals("1.2.3", settingsState.getState().lastSeenPluginVersion));
    }

    @Test
    void checkForUpgradeWithAnUnresolvedRunningVersionIsANoOp() {
        ShaftSettingsState settingsState = new ShaftSettingsState();
        settingsState.getState().lastSeenPluginVersion = "1.2.2";
        boolean[] resetRan = {false};
        ShaftPluginResetService resetService = new ShaftPluginResetService(
                () -> resetRan[0] = true, () -> CompletableFuture.completedFuture(null), () -> { }, List::of, () -> { });

        ShaftPluginUpgradeActivity.checkForUpgrade(null, settingsState, resetService);

        assertAll(
                () -> assertFalse(resetRan[0], "An unresolved running version must not trigger a reset"),
                () -> assertEquals("1.2.2", settingsState.getState().lastSeenPluginVersion,
                        "Stored version must be untouched when the running version is unknown"));
    }
}
