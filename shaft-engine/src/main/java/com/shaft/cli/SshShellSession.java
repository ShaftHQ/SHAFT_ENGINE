package com.shaft.cli;

import com.jcraft.jsch.ChannelShell;
import com.jcraft.jsch.JSchException;
import com.shaft.tools.io.ReportManager;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.regex.Pattern;

/**
 * Experimental prompt-response SSH shell session backed by one JSch {@link ChannelShell}.
 */
public final class SshShellSession implements AutoCloseable {
    private final ChannelShell channel;
    private final InputStream inputStream;
    private final OutputStream outputStream;
    private final Duration defaultTimeout;
    private final Runnable closeCallback;
    private volatile boolean closed;

    SshShellSession(ChannelShell channel, SshShellOptions options, Runnable closeCallback) throws IOException, JSchException {
        this(channel, channel.getInputStream(), channel.getOutputStream(), options.getDefaultTimeout(), closeCallback);
        configureChannel(channel, options);
        channel.connect(Math.toIntExact(options.getDefaultTimeout().toMillis()));
    }

    SshShellSession(InputStream inputStream, OutputStream outputStream, Duration defaultTimeout) {
        this(null, inputStream, outputStream, defaultTimeout, null);
    }

    private SshShellSession(ChannelShell channel, InputStream inputStream, OutputStream outputStream,
                            Duration defaultTimeout, Runnable closeCallback) {
        this.channel = channel;
        this.inputStream = Objects.requireNonNull(inputStream);
        this.outputStream = Objects.requireNonNull(outputStream);
        this.defaultTimeout = Objects.requireNonNull(defaultTimeout);
        this.closeCallback = closeCallback;
    }

    private static void configureChannel(ChannelShell channel, SshShellOptions options) {
        channel.setPty(options.isPty());
        if (options.isPty()) {
            channel.setPtyType(options.getPtyType(), options.getColumns(), options.getRows(), 0, 0);
        }
        options.getEnvironmentVariables().forEach(channel::setEnv);
    }

    public void send(String text) {
        write(text);
        ReportManager.logDiscrete("Sent SSH shell input.");
    }

    public void sendLine(String text) {
        send(Objects.requireNonNullElse(text, "") + "\n");
    }

    public void sendSecret(String text) {
        write(text);
        ReportManager.logDiscrete("Sent SSH shell secret input.");
    }

    public String readUntil(Pattern pattern) {
        return readUntil(pattern, defaultTimeout);
    }

    public String readUntil(Pattern pattern, Duration timeout) {
        Objects.requireNonNull(pattern, "pattern");
        validateTimeout(timeout);
        long deadline = System.nanoTime() + timeout.toNanos();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            while (System.nanoTime() < deadline) {
                drainAvailableBytes(output);
                String currentOutput = output.toString(StandardCharsets.UTF_8);
                if (pattern.matcher(currentOutput).find()) {
                    ReportManager.logDiscrete("Read SSH shell output until pattern: \"" + pattern + "\".");
                    return currentOutput;
                }
                sleepBriefly();
            }
        } catch (IOException exception) {
            throw new RuntimeException("Failed to read SSH shell output.", exception);
        }
        TimeoutException timeoutException = new TimeoutException("Timed out after " + timeout
                + " waiting for SSH shell output matching pattern: " + pattern);
        throw new RuntimeException(timeoutException.getMessage(), timeoutException);
    }

    private void drainAvailableBytes(ByteArrayOutputStream output) throws IOException {
        while (inputStream.available() > 0) {
            output.write(inputStream.read());
        }
    }

    private void write(String text) {
        if (closed) {
            throw new IllegalStateException("SSH shell session is closed.");
        }
        try {
            outputStream.write(Objects.requireNonNullElse(text, "").getBytes(StandardCharsets.UTF_8));
            outputStream.flush();
        } catch (IOException exception) {
            throw new RuntimeException("Failed to write SSH shell input.", exception);
        }
    }

    private void validateTimeout(Duration timeout) {
        if (timeout == null || timeout.isZero() || timeout.isNegative()) {
            throw new IllegalArgumentException("SSH shell read timeout must be positive.");
        }
    }

    private void sleepBriefly() {
        try {
            Thread.sleep(25);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    @Override
    public synchronized void close() {
        if (closed) {
            return;
        }
        closed = true;
        if (channel != null && channel.isConnected()) {
            channel.disconnect();
        }
        if (closeCallback != null) {
            closeCallback.run();
        }
    }
}
