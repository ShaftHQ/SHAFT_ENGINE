package com.shaft.cli;

import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable SSH connection settings for reusable {@link TerminalActions} remote terminals.
 */
public final class SshConnectionOptions {
    private final String host;
    private final int port;
    private final String username;
    private final Path privateKey;
    private final String privateKeyPassphrase;
    private final String password;
    private final Path knownHosts;
    private final boolean strictHostKeyChecking;
    private final KeyboardInteractive keyboardInteractive;
    private final Map<String, String> extraJschConfig;
    private final boolean verbose;
    private final boolean legacyFacade;

    private SshConnectionOptions(Builder builder) {
        host = builder.host;
        port = builder.port;
        username = builder.username;
        privateKey = builder.privateKey;
        privateKeyPassphrase = builder.privateKeyPassphrase;
        password = builder.password;
        knownHosts = builder.knownHosts;
        strictHostKeyChecking = builder.strictHostKeyChecking != null
                ? builder.strictHostKeyChecking
                : builder.knownHosts != null;
        keyboardInteractive = builder.keyboardInteractive;
        extraJschConfig = Collections.unmodifiableMap(new LinkedHashMap<>(builder.extraJschConfig));
        verbose = builder.verbose;
        legacyFacade = builder.legacyFacade;
    }

    /**
     * Starts building SSH connection options.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builds permissive legacy options for existing string-based {@code remoteTerminal(...)} overloads.
     */
    public static SshConnectionOptions fromKeyFile(String host, int port, String username,
                                                   String sshKeyFileFolderName, String sshKeyFileName, boolean verbose) {
        Builder builder = builder()
                .host(host)
                .port(port)
                .username(username)
                .strictHostKeyChecking(false)
                .verbose(verbose)
                .legacyFacade(true);
        if (sshKeyFileName != null && !sshKeyFileName.isBlank()) {
            String absoluteKeyPath = FileActions.getInstance(true).getAbsolutePath(sshKeyFileFolderName, sshKeyFileName);
            builder.privateKey(Path.of(absoluteKeyPath));
        }
        return builder.build();
    }

    /**
     * Validates the options and throws when the host, user or authentication settings are
     * inconsistent.
     */
    public void validate() {
        validateHost();
        validateUsername();
        validatePort();
        validateAuthentication();
        validateKnownHostsForStrictChecking();
    }

    private void validateHost() {
        if (host == null || host.isBlank()) {
            throw new IllegalArgumentException("SSH connection options require a non-blank host.");
        }
    }

    private void validateUsername() {
        if (username == null || username.isBlank()) {
            throw new IllegalArgumentException("SSH connection options require a non-blank username.");
        }
    }

    private void validatePort() {
        if (port <= 0) {
            throw new IllegalArgumentException("SSH connection options require a positive port.");
        }
    }

    private void validateAuthentication() {
        if (legacyFacade || hasAuthenticationMethod()) {
            return;
        }
        throw new IllegalArgumentException("SSH connection options require a private key, password, or keyboard-interactive handler.");
    }

    private boolean hasAuthenticationMethod() {
        return privateKey != null
                || (password != null && !password.isBlank())
                || keyboardInteractive != null;
    }

    private void validateKnownHostsForStrictChecking() {
        if (strictHostKeyChecking && knownHosts == null) {
            throw new IllegalArgumentException("SSH strict host key checking requires a known_hosts file path.");
        }
    }

    /**
     * Describes the connection without secrets, safe for logs and reports.
     */
    public String toRedactedDescription() {
        StringBuilder description = new StringBuilder();
        description.append(host).append(", ").append(port).append(", ").append(username);
        if (privateKey != null) {
            description.append(", privateKey=").append(privateKey);
        }
        if (knownHosts != null) {
            description.append(", knownHosts=").append(knownHosts);
        }
        description.append(", strictHostKeyChecking=").append(strictHostKeyChecking);
        if (privateKeyPassphrase != null && !privateKeyPassphrase.isBlank()) {
            description.append(", privateKeyPassphrase=***");
        }
        if (password != null && !password.isBlank()) {
            description.append(", password=***");
        }
        if (keyboardInteractive != null) {
            description.append(", keyboardInteractive=enabled");
        }
        return description.toString();
    }

    /**
     * Returns the SSH host name or address.
     */
    public String getHost() {
        return host;
    }

    /**
     * Returns the SSH port.
     */
    public int getPort() {
        return port;
    }

    /**
     * Returns the SSH user name.
     */
    public String getUsername() {
        return username;
    }

    /**
     * Returns the private key file used for authentication, or {@code null}.
     */
    public Path getPrivateKey() {
        return privateKey;
    }

    /**
     * Returns the passphrase of the private key, or {@code null}.
     */
    public String getPrivateKeyPassphrase() {
        return privateKeyPassphrase;
    }

    /**
     * Returns the password used for authentication, or {@code null}.
     */
    public String getPassword() {
        return password;
    }

    /**
     * Returns the known-hosts file used to verify the server key, or {@code null}.
     */
    public Path getKnownHosts() {
        return knownHosts;
    }

    /**
     * Returns whether the server host key must match a known-hosts entry.
     */
    public boolean isStrictHostKeyChecking() {
        return strictHostKeyChecking;
    }

    /**
     * Returns the keyboard-interactive responder, or {@code null}.
     */
    public KeyboardInteractive getKeyboardInteractive() {
        return keyboardInteractive;
    }

    /**
     * Returns extra JSch configuration entries applied to the session.
     */
    public Map<String, String> getExtraJschConfig() {
        return extraJschConfig;
    }

    /**
     * Returns whether verbose SSH logging is enabled.
     */
    public boolean isVerbose() {
        return verbose;
    }

    /**
     * Supplies responses for JSch keyboard-interactive authentication prompts.
     */
    @FunctionalInterface
    public interface KeyboardInteractive {
        /**
         * @param destination  remote host identifier
         * @param name         authentication method name
         * @param instruction  server instruction text
         * @param prompt       prompt labels
         * @param echo         whether each response should be echoed
         * @return one response per prompt
         */
        String[] respond(String destination, String name, String instruction, String[] prompt, boolean[] echo);
    }

    public static final class Builder {
        private String host;
        private int port = 22;
        private String username;
        private Path privateKey;
        private String privateKeyPassphrase;
        private String password;
        private Path knownHosts;
        private Boolean strictHostKeyChecking;
        private KeyboardInteractive keyboardInteractive;
        private final Map<String, String> extraJschConfig = new LinkedHashMap<>();
        private boolean verbose;
        private boolean legacyFacade;

        /**
         * Marks the options as created by the legacy terminal facade.
         */
        public Builder legacyFacade(boolean value) {
            legacyFacade = value;
            return this;
        }

        /**
         * Sets the SSH host name or address.
         */
        public Builder host(String value) {
            host = value;
            return this;
        }

        /**
         * Sets the SSH port.
         */
        public Builder port(int value) {
            port = value;
            return this;
        }

        /**
         * Sets the SSH user name.
         */
        public Builder username(String value) {
            username = value;
            return this;
        }

        /**
         * Sets the private key file used for authentication.
         */
        public Builder privateKey(Path value) {
            privateKey = value;
            return this;
        }

        /**
         * Sets the passphrase of the private key.
         */
        public Builder privateKeyPassphrase(String value) {
            privateKeyPassphrase = value;
            return this;
        }

        /**
         * Sets the password used for authentication.
         */
        public Builder password(String value) {
            password = value;
            return this;
        }

        /**
         * Sets the known-hosts file used to verify the server key.
         */
        public Builder knownHosts(Path value) {
            knownHosts = value;
            return this;
        }

        /**
         * Sets whether the server host key must match a known-hosts entry.
         */
        public Builder strictHostKeyChecking(boolean value) {
            strictHostKeyChecking = value;
            return this;
        }

        /**
         * Sets the keyboard-interactive responder.
         */
        public Builder keyboardInteractive(KeyboardInteractive value) {
            keyboardInteractive = value;
            return this;
        }

        /**
         * Adds an extra JSch configuration entry.
         */
        public Builder extraJschConfig(String key, String value) {
            extraJschConfig.put(Objects.requireNonNull(key), Objects.requireNonNull(value));
            return this;
        }

        /**
         * Enables or disables verbose SSH logging.
         */
        public Builder verbose(boolean value) {
            verbose = value;
            return this;
        }

        /**
         * Builds and validates the connection options.
         */
        public SshConnectionOptions build() {
            SshConnectionOptions options = new SshConnectionOptions(this);
            options.validate();
            return options;
        }
    }
}
