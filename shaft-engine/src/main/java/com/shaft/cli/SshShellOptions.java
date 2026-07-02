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

    public static Builder builder() {
        return new Builder();
    }

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

    public boolean isPty() {
        return pty;
    }

    public String getPtyType() {
        return ptyType;
    }

    public int getColumns() {
        return columns;
    }

    public int getRows() {
        return rows;
    }

    public Duration getDefaultTimeout() {
        return defaultTimeout;
    }

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

        public Builder pty(boolean value) {
            pty = value;
            return this;
        }

        public Builder ptyType(String value) {
            ptyType = value;
            return this;
        }

        public Builder columns(int value) {
            columns = value;
            return this;
        }

        public Builder rows(int value) {
            rows = value;
            return this;
        }

        public Builder defaultTimeout(Duration value) {
            defaultTimeout = value;
            return this;
        }

        public Builder environment(Map<String, String> value) {
            environment.clear();
            if (value != null) {
                environment.putAll(value);
            }
            return this;
        }

        public Builder putEnvironment(String key, String value) {
            environment.put(Objects.requireNonNull(key), Objects.requireNonNull(value));
            return this;
        }

        public SshShellOptions build() {
            SshShellOptions options = new SshShellOptions(this);
            options.validate();
            return options;
        }
    }
}
