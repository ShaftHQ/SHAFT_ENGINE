package com.shaft.cli;

import java.time.Duration;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Options for an experimental reusable SSH shell session.
 */
public final class SshShellOptions {
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);
    private static final String DEFAULT_PTY_TYPE = "vt100";
    private static final int DEFAULT_COLUMNS = 80;
    private static final int DEFAULT_ROWS = 24;

    private final boolean pty;
    private final String ptyType;
    private final int columns;
    private final int rows;
    private final Duration defaultTimeout;
    private final Map<String, String> environmentVariables;

    private SshShellOptions(Builder builder) {
        pty = builder.pty;
        ptyType = builder.ptyType == null || builder.ptyType.isBlank() ? DEFAULT_PTY_TYPE : builder.ptyType;
        columns = builder.columns;
        rows = builder.rows;
        defaultTimeout = builder.defaultTimeout == null ? DEFAULT_TIMEOUT : builder.defaultTimeout;
        environmentVariables = Collections.unmodifiableMap(new LinkedHashMap<>(builder.environmentVariables));
        validate();
    }

    public static Builder builder() {
        return new Builder();
    }

    public static SshShellOptions defaults() {
        return builder().build();
    }

    private void validate() {
        if (columns <= 0) {
            throw new IllegalArgumentException("SSH shell columns must be positive.");
        }
        if (rows <= 0) {
            throw new IllegalArgumentException("SSH shell rows must be positive.");
        }
        if (defaultTimeout.isZero() || defaultTimeout.isNegative()) {
            throw new IllegalArgumentException("SSH shell default timeout must be positive.");
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

    public Map<String, String> getEnvironmentVariables() {
        return environmentVariables;
    }

    public static final class Builder {
        private boolean pty;
        private String ptyType = DEFAULT_PTY_TYPE;
        private int columns = DEFAULT_COLUMNS;
        private int rows = DEFAULT_ROWS;
        private Duration defaultTimeout = DEFAULT_TIMEOUT;
        private final Map<String, String> environmentVariables = new LinkedHashMap<>();

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

        public Builder environmentVariable(String key, String value) {
            environmentVariables.put(Objects.requireNonNull(key), Objects.requireNonNull(value));
            return this;
        }

        public Builder environmentVariables(Map<String, String> values) {
            if (values != null) {
                values.forEach(this::environmentVariable);
            }
            return this;
        }

        public SshShellOptions build() {
            return new SshShellOptions(this);
        }
    }
}
