package com.shaft.cli;

import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

/**
 * Remote port forward created via {@link RemoteSshClient#forwardRemotePort(int, String, int)}.
 */
public final class SshRemotePortForward implements AutoCloseable {
    private final Session session;
    private final int remotePort;
    private final Runnable unregister;

    SshRemotePortForward(Session session, int remotePort, Runnable unregister) {
        this.session = session;
        this.remotePort = remotePort;
        this.unregister = unregister;
    }

    public int getRemotePort() {
        return remotePort;
    }

    @Override
    public void close() {
        try {
            if (session != null && session.isConnected()) {
                session.delPortForwardingR(remotePort);
            }
        } catch (JSchException ignored) {
        } finally {
            unregister.run();
        }
    }
}
