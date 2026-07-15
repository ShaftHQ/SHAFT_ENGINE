package com.shaft.intellij.settings;

import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.ProjectActivity;
import kotlin.Unit;
import kotlin.coroutines.Continuation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Detects a plugin version upgrade (the running version differs from the last-seen version
 * persisted in settings) once per IDE session and, on upgrade, factory-resets the stale local
 * UI/setup state -- settings, stored provider credentials, and tool approvals -- while
 * preserving every open project's Assistant chat history, then re-renders any open SHAFT tool
 * window. This fixes two upgrade-time bugs: the setup page failing to re-render after an update,
 * and the recorder launching an old shaft-mcp because a stale {@code mcpCommand} survived the
 * update.
 *
 * <p>{@link com.intellij.openapi.startup.ProjectActivity} fires once per opened project, but the
 * version compare-and-reset must run exactly once per IDE session; {@link #CHECKED} guards that,
 * mirroring the process-wide {@code AtomicBoolean} pattern used by
 * {@link com.shaft.intellij.ui.ShaftRecordingActivity#active()}.
 */
public final class ShaftPluginUpgradeActivity implements ProjectActivity {
    private static final AtomicBoolean CHECKED = new AtomicBoolean();
    private static final String PLUGIN_ID = "io.github.shafthq.shaft";

    /** Outcome of comparing the stored last-seen version against the running version. */
    enum UpgradeDecision {
        /** No version was ever stored: a fresh install, not an upgrade -- no reset. */
        FRESH_INSTALL,
        /** Stored version differs from the running version: an upgrade -- reset required. */
        UPGRADED,
        /** Stored version matches the running version: nothing to do. */
        UNCHANGED
    }

    @Nullable
    @Override
    public Object execute(@NotNull Project project, @NotNull Continuation<? super Unit> continuation) {
        if (CHECKED.compareAndSet(false, true)) {
            checkForUpgrade(runningPluginVersion(), ShaftSettingsState.getInstance(),
                    ShaftPluginResetService.getInstance());
        }
        return Unit.INSTANCE;
    }

    /**
     * Compares the running plugin version against the last-seen version stored in
     * {@code settingsState}, resetting via {@code resetService} on an upgrade and persisting the
     * running version afterward (issue: plugin-upgrade auto-reset). A no-op when the running
     * version could not be resolved.
     *
     * @param runningVersion the currently running plugin version, or {@code null} if unresolved
     * @param settingsState  the application settings state holding {@code lastSeenPluginVersion}
     * @param resetService   the service used to reset stale state on an upgrade
     */
    static void checkForUpgrade(@Nullable String runningVersion,
                                 @NotNull ShaftSettingsState settingsState,
                                 @NotNull ShaftPluginResetService resetService) {
        if (runningVersion == null || runningVersion.isBlank()) {
            return;
        }
        ShaftSettingsState.Settings settings = settingsState.getState();
        UpgradeDecision decision = shouldResetForUpgrade(settings.lastSeenPluginVersion, runningVersion);
        if (decision == UpgradeDecision.UPGRADED) {
            resetService.resetForUpgrade();
        }
        if (decision != UpgradeDecision.UNCHANGED) {
            // getState() always returns the same live Settings instance (loadState() copies field
            // values onto it via XmlSerializerUtil.copyBean rather than swapping the reference), so
            // this write lands after resetForUpgrade()'s factory-defaults reset above and survives it,
            // exactly as required: lastSeenPluginVersion must reflect the running version once this
            // method returns, whether or not a reset happened.
            settings.lastSeenPluginVersion = runningVersion;
        }
    }

    /**
     * Decides what to do given the stored (last-seen) and running plugin versions. Extracted as a
     * pure function so the decision is unit-testable without a running IDE / real
     * {@link ProjectActivity}.
     *
     * @param storedVersion  the last-seen version persisted in settings; blank or {@code null}
     *                       means no version has ever been recorded
     * @param runningVersion the currently running plugin version (assumed non-blank)
     * @return the resulting decision
     */
    static UpgradeDecision shouldResetForUpgrade(@Nullable String storedVersion, @NotNull String runningVersion) {
        if (storedVersion == null || storedVersion.isBlank()) {
            return UpgradeDecision.FRESH_INSTALL;
        }
        return storedVersion.equals(runningVersion) ? UpgradeDecision.UNCHANGED : UpgradeDecision.UPGRADED;
    }

    @Nullable
    private static String runningPluginVersion() {
        IdeaPluginDescriptor plugin = PluginManagerCore.getPlugin(PluginId.getId(PLUGIN_ID));
        return plugin == null ? null : plugin.getVersion();
    }
}
