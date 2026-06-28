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

    public void validate() {
        if (host == null || host.isBlank()) {
            throw new IllegalArgumentException("SSH connection options require a non-blank host.");
        }
        if (username == null || username.isBlank()) {
            throw new IllegalArgumentException("SSH connection options require a non-blank username.");
        }
        if (port <= 0) {
            throw new IllegalArgumentException("SSH connection options require a positive port.");
        }
        boolean hasPrivateKey = privateKey != null;
        boolean hasPassword = password != null && !password.isBlank();
        boolean hasKeyboardInteractive = keyboardInteractive != null;
        if (!legacyFacade && !hasPrivateKey && !hasPassword && !hasKeyboardInteractive) {
            throw new IllegalArgumentException("SSH connection options require a private key, password, or keyboard-interactive handler.");
        }
        if (strictHostKeyChecking && knownHosts == null) {
            throw new IllegalArgumentException("SSH strict host key checking requires a known_hosts file path.");
        }
    }

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

    public String getHost() {
        return host;
    }

    public int getPort() {
        return port;
    }

    public String getUsername() {
        return username;
    }

    public Path getPrivateKey() {
        return privateKey;
    }

    public String getPrivateKeyPassphrase() {
        return privateKeyPassphrase;
    }

    public String getPassword() {
        return password;
    }

    public Path getKnownHosts() {
        return knownHosts;
    }

    public boolean isStrictHostKeyChecking() {
        return strictHostKeyChecking;
    }

    public KeyboardInteractive getKeyboardInteractive() {
        return keyboardInteractive;
    }

    public Map<String, String> getExtraJschConfig() {
        return extraJschConfig;
    }

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

        public Builder legacyFacade(boolean value) {
            legacyFacade = value;
            return this;
        }

        public Builder host(String value) {
            host = value;
            return this;
        }

        public Builder port(int value) {
            port = value;
            return this;
        }

        public Builder username(String value) {
            username = value;
            return this;
        }

        public Builder privateKey(Path value) {
            privateKey = value;
            return this;
        }

        public Builder privateKeyPassphrase(String value) {
            privateKeyPassphrase = value;
            return this;
        }

        public Builder password(String value) {
            password = value;
            return this;
        }

        public Builder knownHosts(Path value) {
            knownHosts = value;
            return this;
        }

        public Builder strictHostKeyChecking(boolean value) {
            strictHostKeyChecking = value;
            return this;
        }

        public Builder keyboardInteractive(KeyboardInteractive value) {
            keyboardInteractive = value;
            return this;
        }

        public Builder extraJschConfig(String key, String value) {
            extraJschConfig.put(Objects.requireNonNull(key), Objects.requireNonNull(value));
            return this;
        }

        public Builder verbose(boolean value) {
            verbose = value;
            return this;
        }

        public SshConnectionOptions build() {
            SshConnectionOptions options = new SshConnectionOptions(this);
            options.validate();
            return options;
        }
    }
}
