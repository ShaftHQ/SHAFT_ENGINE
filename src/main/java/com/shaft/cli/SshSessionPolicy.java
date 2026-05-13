package com.shaft.cli;

/**
 * How {@link RemoteSshClient} manages the underlying JSch {@link com.jcraft.jsch.Session}
 * across multiple {@link RemoteSshClient#performCommands(java.util.List)} calls.
 *
 * <p><b>REUSE_SESSION</b> (default) keeps one TCP/SSH login until {@link RemoteSshClient#close()}.</p>
 * <p><b>NEW_SESSION_PER_COMMAND</b> disconnects after each remote exec (legacy {@code TerminalActions}-style
 * isolation), useful for long-running work or servers that drop idle sessions.</p>
 */
public enum SshSessionPolicy {
    REUSE_SESSION,
    NEW_SESSION_PER_COMMAND
}
