package com.shaft.commandline.session;

/**
 * The current state of the shaft-cli daemon session.
 *
 * @param running whether the session's process is alive
 * @param info    the persisted session metadata, or {@code null} when no session file exists
 */
public record SessionStatus(boolean running, SessionInfo info) {
}
