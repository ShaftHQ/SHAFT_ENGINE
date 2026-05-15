package com.shaft.cli;

import com.jcraft.jsch.ChannelShell;
import com.jcraft.jsch.JSchException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Interactive SSH {@code shell} with PTY (timing- and prompt-sensitive). For scripted checks use
 * {@link RemoteSshClient#performCommand(String)} instead.
 */
public final class RemoteSshInteractiveShell implements AutoCloseable {
    private final ChannelShell channel;

    private RemoteSshInteractiveShell(ChannelShell channel) {
        this.channel = channel;
    }

    /**
     * Opens a shell channel on the client's session. The client must remain connected for the lifetime of this shell.
     */
    public static RemoteSshInteractiveShell open(RemoteSshClient client) throws JSchException {
        client.connect();
        ChannelShell ch = (ChannelShell) client.getJschSession().openChannel("shell");
        ch.setPty(true);
        ch.connect();
        return new RemoteSshInteractiveShell(ch);
    }

    public InputStream getInputStream() throws IOException {
        return channel.getInputStream();
    }

    public OutputStream getOutputStream() throws IOException {
        return channel.getOutputStream();
    }

    public ChannelShell jschChannel() {
        return channel;
    }

    @Override
    public void close() {
        if (channel != null && channel.isConnected()) {
            channel.disconnect();
        }
    }
}
