package com.shaft.cli.internal;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

import java.util.Properties;

/**
 * Opens a connected JSch {@link Session} from {@link SshConnectionOptions}.
 * Does not apply command-level timeouts; callers set {@link Session#setTimeout(int)} after connect if needed.
 */
public final class JschSessionFactory {
    private JschSessionFactory() {
    }

    public static Session connect(SshConnectionOptions options) throws JSchException {
        JSch jsch = new JSch();
        if (options.identityPath() != null && !options.identityPath().isEmpty()) {
            jsch.addIdentity(options.identityPath());
        }
        Session session = jsch.getSession(options.username(), options.host(), options.port());
        Properties config = new Properties();
        config.put("StrictHostKeyChecking", options.strictHostKeyChecking());
        session.setConfig(config);
        if (options.serverAliveIntervalMs() > 0) {
            session.setServerAliveInterval(options.serverAliveIntervalMs());
        }
        session.connect(options.connectTimeoutMs());
        return session;
    }
}
