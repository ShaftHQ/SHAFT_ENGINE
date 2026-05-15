package com.shaft.cli.internal;

/**
 * Parameters for establishing a JSch SSH session (identity, password, and known_hosts optional).
 */
public record SshConnectionOptions(
        String username,
        String host,
        int port,
        String identityPath,
        String strictHostKeyChecking,
        int connectTimeoutMs,
        int serverAliveIntervalMs,
        String knownHostsPath,
        String password,
        String keyPassphrase) {
}
