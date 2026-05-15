package com.shaft.cli;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.SftpException;

/**
 * SFTP operations on a channel opened from an existing {@link RemoteSshClient} session.
 */
public final class SshSftp implements AutoCloseable {
    private final ChannelSftp channel;

    SshSftp(ChannelSftp channel) {
        this.channel = channel;
    }

    public void upload(String localPath, String remotePath) throws SftpException {
        channel.put(localPath, remotePath);
    }

    public void download(String remotePath, String localPath) throws SftpException {
        channel.get(remotePath, localPath);
    }

    public void mkdir(String path) throws SftpException {
        channel.mkdir(path);
    }

    public void rm(String path) throws SftpException {
        channel.rm(path);
    }

    /**
     * Exposes the underlying JSch channel for advanced use. Do not {@link ChannelSftp#disconnect()} while this
     * wrapper is open unless you also stop using {@link RemoteSshClient} SFTP helpers.
     */
    public ChannelSftp channel() {
        return channel;
    }

    @Override
    public void close() {
        if (channel != null && channel.isConnected()) {
            channel.disconnect();
        }
    }
}
