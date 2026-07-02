package com.shaft.cli;

import com.jcraft.jsch.ChannelShell;
import com.shaft.tools.io.ReportManager;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.regex.Pattern;

/**
 * Experimental interactive SSH shell session backed by one JSch {@link ChannelShell}.
 */
public final class SshShellSession implements AutoCloseable {
    private final TerminalActions parent;
    private final ChannelShell channel;
    private final InputStream shellInput;
    private final OutputStream shellOutput;
    private final SshShellOptions options;
    private final StringBuilder outputBuffer = new StringBuilder();
    private final Object outputLock = new Object();
    private final ExecutorService outputReader;
    private volatile boolean closed;

    SshShellSession(TerminalActions parent, ChannelShell channel, SshShellOptions options) throws IOException {
        this(parent, channel, channel.getInputStream(), channel.getOutputStream(), options);
    }

    SshShellSession(TerminalActions parent, InputStream shellInput, OutputStream shellOutput, SshShellOptions options) {
        this(parent, null, shellInput, shellOutput, options);
    }

    private SshShellSession(TerminalActions parent, ChannelShell channel, InputStream shellInput, OutputStream shellOutput,
                            SshShellOptions options) {
        this.parent = parent;
        this.channel = channel;
        this.shellInput = shellInput;
        this.shellOutput = shellOutput;
        this.options = options;
        this.outputReader = Executors.newSingleThreadExecutor(threadFactory());
        startOutputReader();
    }

    /**
     * Writes exact text to the remote shell without appending a newline.
     */
    public void send(String text) {
        verifyOpen();
        String payload = text == null ? "" : text;
        writeToShell(payload);
        ReportManager.logDiscrete("Sent SSH shell input: \"" + parent.redactShellLog(payload) + "\".");
    }

    /**
     * Writes text plus a newline to the remote shell.
     */
    public void sendLine(String text) {
        send((text == null ? "" : text) + '\n');
    }

    /**
     * Writes exact text to the remote shell without logging the raw payload.
     */
    public void sendSecret(String text) {
        verifyOpen();
        writeToShell(text == null ? "" : text);
        ReportManager.logDiscrete("Sent secret SSH shell input (redacted).");
    }

    /**
     * Reads shell output until the supplied pattern matches using the configured default timeout.
     */
    public String readUntil(Pattern pattern) {
        return readUntil(pattern, options.getDefaultTimeout());
    }

    /**
     * Reads shell output until the supplied pattern matches or the timeout elapses.
     *
     * @return all shell output read through the end of the first match
     */
    public String readUntil(Pattern pattern, Duration timeout) {
        verifyOpen();
        if (pattern == null) {
            parent.failShellAction("SSH shell read", new IllegalArgumentException("Shell read pattern must not be null."));
        }
        Duration effectiveTimeout = timeout == null ? options.getDefaultTimeout() : timeout;
        if (effectiveTimeout.isZero() || effectiveTimeout.isNegative()) {
            parent.failShellAction("SSH shell read", new IllegalArgumentException("Shell read timeout must be positive."));
        }

        long deadlineNanos = System.nanoTime() + effectiveTimeout.toNanos();
        while (System.nanoTime() < deadlineNanos) {
            String matchedOutput = takeMatchedOutput(pattern);
            if (matchedOutput != null) {
                return matchedOutput;
            }
            sleepBriefly();
        }
        parent.failShellReadTimeout(pattern, effectiveTimeout);
        return "";
    }

    /**
     * Closes the shell channel and removes it from the parent terminal session.
     */
    @Override
    public synchronized void close() {
        if (closed) {
            return;
        }
        closed = true;
        shutdownReader();
        disconnectChannel();
        parent.unregisterShellSession(this);
    }

    synchronized void closeSilently() {
        if (closed) {
            return;
        }
        closed = true;
        shutdownReader();
        disconnectChannel();
    }

    private void verifyOpen() {
        if (closed) {
            parent.failShellAction("SSH shell", new IllegalStateException("SSH shell session is already closed."));
        }
    }

    private void writeToShell(String payload) {
        try {
            shellOutput.write(payload.getBytes(StandardCharsets.UTF_8));
            shellOutput.flush();
        } catch (IOException exception) {
            parent.failShellAction("SSH shell write", exception);
        }
    }

    private String takeMatchedOutput(Pattern pattern) {
        synchronized (outputLock) {
            java.util.regex.Matcher matcher = pattern.matcher(outputBuffer);
            if (!matcher.find()) {
                return null;
            }
            int matchEnd = matcher.end();
            String matchedOutput = outputBuffer.substring(0, matchEnd);
            outputBuffer.delete(0, matchEnd);
            return matchedOutput;
        }
    }

    private void sleepBriefly() {
        try {
            Thread.sleep(50);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            parent.failShellAction("SSH shell read", exception);
        }
    }

    private void startOutputReader() {
        outputReader.submit(() -> {
            byte[] buffer = new byte[1024];
            try {
                int read;
                while (!closed && (read = shellInput.read(buffer)) != -1) {
                    String chunk = new String(buffer, 0, read, StandardCharsets.UTF_8);
                    synchronized (outputLock) {
                        outputBuffer.append(chunk);
                    }
                    parent.logShellOutput(chunk);
                }
            } catch (IOException exception) {
                if (!closed) {
                    parent.failShellAction("SSH shell read", exception);
                }
            }
        });
    }

    private void shutdownReader() {
        outputReader.shutdownNow();
    }

    private void disconnectChannel() {
        if (channel != null && channel.isConnected()) {
            channel.disconnect();
        }
    }

    private static java.util.concurrent.ThreadFactory threadFactory() {
        return runnable -> {
            Thread thread = new Thread(runnable, "SHAFT-SSH-Shell-Reader");
            thread.setDaemon(true);
            return thread;
        };
    }
}
