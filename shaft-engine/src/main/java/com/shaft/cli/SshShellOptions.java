package com.shaft.cli;

import java.time.Duration;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable options for an experimental reusable SSH shell channel.
 */
public final class SshShellOptions {
    private static final String DEFAULT_PTY_TYPE = "vt100";
    private static final int DEFAULT_COLUMNS = 80;
    private static final int DEFAULT_ROWS = 24;
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);

    private final boolean pty;
    private final String ptyType;
    private final int columns;
    private final int rows;
    private final Duration defaultTimeout;
    private final Map<String, String> environment;

    private SshShellOptions(Builder builder) {
        pty = builder.pty;
        ptyType = builder.ptyType;
        columns = builder.columns;
        rows = builder.rows;
        defaultTimeout = builder.defaultTimeout;
        environment = Collections.unmodifiableMap(new LinkedHashMap<>(builder.environment));
    }

    /**
     * Starts building SSH shell options.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Validates the options and throws when a value is out of range.
     */
    public void validate() {
        validatePtyType();
        validateDimensions();
        validateTimeout();
    }

    private void validatePtyType() {
        if (ptyType == null || ptyType.isBlank()) {
            throw new IllegalArgumentException("SSH shell options require a non-blank PTY type.");
        }
    }

    private void validateDimensions() {
        if (columns <= 0 || rows <= 0) {
            throw new IllegalArgumentException("SSH shell PTY columns and rows must be positive.");
        }
    }

    private void validateTimeout() {
        if (defaultTimeout == null || defaultTimeout.isZero() || defaultTimeout.isNegative()) {
            throw new IllegalArgumentException("SSH shell options require a positive default timeout.");
        }
    }

    /**
     * Returns whether a pseudo-terminal is requested.
     */
    public boolean isPty() {
        return pty;
    }

    /**
     * Returns the requested pseudo-terminal type.
     */
    public String getPtyType() {
        return ptyType;
    }

    /**
     * Returns the pseudo-terminal width in columns.
     */
    public int getColumns() {
        return columns;
    }

    /**
     * Returns the pseudo-terminal height in rows.
     */
    public int getRows() {
        return rows;
    }

    /**
     * Returns the default timeout for shell commands.
     */
    public Duration getDefaultTimeout() {
        return defaultTimeout;
    }

    /**
     * Returns the environment variables set for the shell.
     */
    public Map<String, String> getEnvironment() {
        return environment;
    }

    public static final class Builder {
        private boolean pty;
        private String ptyType = DEFAULT_PTY_TYPE;
        private int columns = DEFAULT_COLUMNS;
        private int rows = DEFAULT_ROWS;
        private Duration defaultTimeout = DEFAULT_TIMEOUT;
        private final Map<String, String> environment = new LinkedHashMap<>();

        /**
         * Sets whether a pseudo-terminal is requested.
         */
        public Builder pty(boolean value) {
            pty = value;
            return this;
        }

        /**
         * Sets the pseudo-terminal type, such as {@code xterm}.
         */
        public Builder ptyType(String value) {
            ptyType = value;
            return this;
        }

        /**
         * Sets the pseudo-terminal width in columns.
         */
        public Builder columns(int value) {
            columns = value;
            return this;
        }

        /**
         * Sets the pseudo-terminal height in rows.
         */
        public Builder rows(int value) {
            rows = value;
            return this;
        }

        /**
         * Sets the default timeout for shell commands.
         */
        public Builder defaultTimeout(Duration value) {
            defaultTimeout = value;
            return this;
        }

        /**
         * Replaces the environment variables set for the shell.
         */
        public Builder environment(Map<String, String> value) {
            environment.clear();
            if (value != null) {
                environment.putAll(value);
            }
            return this;
        }

        /**
         * Adds one environment variable for the shell.
         */
        public Builder putEnvironment(String key, String value) {
            environment.put(Objects.requireNonNull(key), Objects.requireNonNull(value));
            return this;
        }

        /**
         * Builds and validates the shell options.
         */
        public SshShellOptions build() {
            SshShellOptions options = new SshShellOptions(this);
            options.validate();
            return options;
        }
    }
}
