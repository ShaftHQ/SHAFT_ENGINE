package com.shaft.cli;

import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

/**
 * Local port forward created via {@link RemoteSshClient#forwardLocalPort(int, String, int)}.
 * Closing removes the forward without tearing down the SSH session.
 */
public final class SshLocalPortForward implements AutoCloseable {
    private final Session session;
    private final int boundLocalPort;
    private final Runnable unregister;

    SshLocalPortForward(Session session, int boundLocalPort, Runnable unregister) {
        this.session = session;
        this.boundLocalPort = boundLocalPort;
        this.unregister = unregister;
    }

    public int getBoundLocalPort() {
        return boundLocalPort;
    }

    @Override
    public void close() {
        try {
            if (session != null && session.isConnected()) {
                session.delPortForwardingL(boundLocalPort);
            }
        } catch (JSchException ignored) {
        } finally {
            unregister.run();
        }
    }
}
