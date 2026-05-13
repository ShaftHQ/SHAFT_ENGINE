package com.shaft.cli.internal;

/**
 * Parameters for establishing a JSch SSH session (identity optional).
 */
public record SshConnectionOptions(
        String username,
        String host,
        int port,
        String identityPath,
        String strictHostKeyChecking,
        int connectTimeoutMs,
        int serverAliveIntervalMs) {
}
