package com.shaft.pilot.agent;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable local agent execution request.
 *
 * @param client local agent client
 * @param mode Autobot mode
 * @param prompt prompt written to the local agent stdin
 * @param workingDirectory command working directory
 * @param command optional custom command; defaults are used when empty
 * @param environment additional process environment variables
 * @param timeout maximum execution time
 * @param allowSourceMutation explicit approval for agent mode to edit local source
 */
public record LocalAgentRequest(
        LocalAgentClient client,
        LocalAgentMode mode,
        String prompt,
        Path workingDirectory,
        List<String> command,
        Map<String, String> environment,
        Duration timeout,
        boolean allowSourceMutation) {
    private static final Duration DEFAULT_TIMEOUT = Duration.ofMinutes(5);

    /**
     * Creates and defensively copies a request.
     */
    public LocalAgentRequest {
        client = Objects.requireNonNull(client, "client");
        mode = Objects.requireNonNull(mode, "mode");
        prompt = Objects.requireNonNullElse(prompt, "").trim();
        if (prompt.isEmpty()) {
            throw new IllegalArgumentException("Local agent prompt is required.");
        }
        workingDirectory = workingDirectory == null ? Path.of(".") : workingDirectory;
        command = command == null
                ? List.of()
                : command.stream().filter(value -> value != null && !value.isBlank()).map(String::trim).toList();
        environment = environment == null
                ? Map.of()
                : Map.copyOf(environment);
        timeout = timeout == null ? DEFAULT_TIMEOUT : timeout;
        if (timeout.isZero() || timeout.isNegative()) {
            throw new IllegalArgumentException("Local agent timeout must be positive.");
        }
    }

    /**
     * Starts a request builder.
     *
     * @param client local agent client
     * @param mode Autobot mode
     * @param prompt prompt text
     * @return request builder
     */
    public static Builder builder(LocalAgentClient client, LocalAgentMode mode, String prompt) {
        return new Builder(client, mode, prompt);
    }

    /**
     * Fluent builder for {@link LocalAgentRequest}.
     */
    public static final class Builder {
        private final LocalAgentClient client;
        private final LocalAgentMode mode;
        private final String prompt;
        private Path workingDirectory = Path.of(".");
        private List<String> command = List.of();
        private Map<String, String> environment = Map.of();
        private Duration timeout = DEFAULT_TIMEOUT;
        private boolean allowSourceMutation;

        private Builder(LocalAgentClient client, LocalAgentMode mode, String prompt) {
            this.client = client;
            this.mode = mode;
            this.prompt = prompt;
        }

        /**
         * Sets the process working directory.
         *
         * @param value working directory
         * @return this builder
         */
        public Builder workingDirectory(Path value) {
            workingDirectory = value;
            return this;
        }

        /**
         * Sets a custom command that overrides SHAFT defaults.
         *
         * @param value command and arguments
         * @return this builder
         */
        public Builder command(List<String> value) {
            command = value == null ? List.of() : new ArrayList<>(value);
            return this;
        }

        /**
         * Sets additional process environment variables.
         *
         * @param value environment variables
         * @return this builder
         */
        public Builder environment(Map<String, String> value) {
            environment = value == null ? Map.of() : new LinkedHashMap<>(value);
            return this;
        }

        /**
         * Sets the process timeout.
         *
         * @param value timeout
         * @return this builder
         */
        public Builder timeout(Duration value) {
            timeout = value;
            return this;
        }

        /**
         * Allows agent mode to perform source mutations.
         *
         * @param value true when the caller has explicitly approved mutation
         * @return this builder
         */
        public Builder allowSourceMutation(boolean value) {
            allowSourceMutation = value;
            return this;
        }

        /**
         * Builds the immutable request.
         *
         * @return request
         */
        public LocalAgentRequest build() {
            return new LocalAgentRequest(client, mode, prompt, workingDirectory, command, environment, timeout,
                    allowSourceMutation);
        }
    }
}
