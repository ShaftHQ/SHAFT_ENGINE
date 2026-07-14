package com.shaft.intellij.ui;

/**
 * Real check for the setup flow's "SHAFT MCP version" step (issue #3538): compares the installed
 * shaft-mcp version — detected from the newest {@code versions/<version>/shaft-mcp.args} on disk —
 * against the latest released version. Mirrors {@link ShaftProjectVersionCheck}'s shape but is
 * scoped to shaft-mcp itself rather than the project's SHAFT Engine dependency, and never reports a
 * blocking outcome: an unknown latest version is a neutral state, not a failure.
 */
final class ShaftMcpVersionCheck {

    /**
     * Outcome of one check.
     */
    enum State {
        /** No installed shaft-mcp was found on disk. */
        NOT_INSTALLED,
        /** Installed shaft-mcp is at or above the latest known release. */
        UP_TO_DATE,
        /** Installed shaft-mcp is below the latest known release. */
        UPGRADE_AVAILABLE,
        /** The latest release (or the installed version) could not be determined (offline). */
        LATEST_UNKNOWN
    }

    /**
     * One check result.
     *
     * @param state            comparison outcome
     * @param installedVersion version detected on disk, or blank when not installed
     * @param latestVersion    latest released version used for comparison, or blank when unknown
     */
    record Result(State state, String installedVersion, String latestVersion) {
    }

    private ShaftMcpVersionCheck() {
        throw new IllegalStateException("Utility class");
    }

    static Result check(String installed, String latest) {
        String installedTrimmed = installed == null ? "" : installed.trim();
        String latestTrimmed = latest == null ? "" : latest.trim();
        if (installedTrimmed.isBlank()) {
            return new Result(State.NOT_INSTALLED, "", latestTrimmed);
        }
        if (latestTrimmed.isBlank() || !ShaftProjectVersionCheck.isVersionLike(latestTrimmed)
                || !ShaftProjectVersionCheck.isVersionLike(installedTrimmed)) {
            // Can't compare either way — still surface the install/update command rather than
            // blocking, since offline is expected and must never fail the setup flow.
            return new Result(State.LATEST_UNKNOWN, installedTrimmed, latestTrimmed);
        }
        return new Result(
                ShaftProjectVersionCheck.compareVersions(installedTrimmed, latestTrimmed) >= 0
                        ? State.UP_TO_DATE
                        : State.UPGRADE_AVAILABLE,
                installedTrimmed,
                latestTrimmed);
    }
}
