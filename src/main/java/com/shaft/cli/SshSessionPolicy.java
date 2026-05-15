package com.shaft.cli;

/**
 * How {@link RemoteSshClient} manages the underlying JSch {@link com.jcraft.jsch Session}.
 */
public enum SshSessionPolicy {
    /** One SSH session; multiple {@code exec} channels until {@link RemoteSshClient#close()}. */
    REUSE_SESSION,
    /** Disconnect after each remote command (closer to legacy {@link TerminalActions} remote behavior). */
    NEW_SESSION_PER_COMMAND
}
