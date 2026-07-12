package com.shaft.commandline.session;

/**
 * Metadata about a running shaft-cli daemon session, persisted so subsequent CLI
 * invocations can find and reuse it.
 *
 * @param port      the HTTP daemon's listening port on {@code 127.0.0.1}
 * @param pid       the daemon process id
 * @param jarPath   the resolved shaft-mcp jar path used to launch the daemon
 * @param startedAt the daemon's start time as an ISO-8601 instant string
 */
public record SessionInfo(int port, long pid, String jarPath, String startedAt) {
}
