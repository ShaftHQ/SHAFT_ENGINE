package com.shaft.intellij.mcp;

/**
 * A single MCP {@code notifications/progress} update relayed to a caller-registered progress
 * callback (issue #3546). The server sends these best-effort, only for tools that opted in
 * (currently {@code capture_generate_replay}) and only when the caller requested progress by
 * supplying a callback; callers that pass no callback never see this type.
 *
 * @param progress current progress value; the MCP spec favors a 0.0-1.0 fraction but does not
 *                 constrain it server-side, so this is passed through verbatim
 * @param total    optional total {@code progress} is measured against, or {@code null} when the
 *                 server did not report one
 * @param message  optional human-readable milestone text, or {@code null} when absent
 */
public record ShaftMcpProgress(double progress, Double total, String message) {
}
