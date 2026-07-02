package testPackage.integration;

import com.jcraft.jsch.Session;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * Env-gated integration coverage for reusable remote SSH terminals.
 *
 * <p>Skipped unless {@code TEST_SSH_HOST}, {@code TEST_SSH_PORT}, {@code TEST_SSH_USERNAME},
 * {@code TEST_SSH_KEY_FOLDER}, {@code TEST_SSH_KEY_NAME}, and {@code TEST_SSH_TMP_DIR} are set.</p>
 */
public class TerminalActionsRemoteSshIntegrationTest {
    private static final String SKIP_MESSAGE =
            "Required TEST_SSH_* environment variables are not configured "
                    + "(TEST_SSH_HOST, TEST_SSH_PORT, TEST_SSH_USERNAME, TEST_SSH_KEY_FOLDER, "
                    + "TEST_SSH_KEY_NAME, TEST_SSH_TMP_DIR).";

    private record SshConfig(
            String host,
            int port,
            String username,
            String keyFolder,
            String keyName,
            String remoteTempDir) {
    }

    @Test(description = "Reusable remote terminal should execute multiple commands on one session")
    public void reusableRemoteTerminalShouldExecuteMultipleCommands() {
        SshConfig config = requireConfig();
        TerminalActions remote = createRemoteTerminal(config);
        try {
            String whoami = remote.performTerminalCommand("whoami");
            String marker = "shaft-remote-" + UUID.randomUUID();
            String echoed = remote.performTerminalCommand("printf " + marker);

            Validations.assertThat().object(whoami).doesNotEqual("").perform();
            Validations.assertThat().object(echoed).contains(marker).perform();
        } finally {
            remote.quit();
        }
    }

    @Test(description = "SFTP upload and download should round-trip file content")
    public void sftpUploadDownloadShouldRoundTripFileContent() throws IOException {
        SshConfig config = requireConfig();
        TerminalActions remote = createRemoteTerminal(config);
        String marker = "shaft-sftp-" + UUID.randomUUID();
        Path localSource = Files.createTempFile("shaft-ssh-upload-", ".txt");
        Path localDownloadDir = Path.of("target", "ssh-integration");
        Files.createDirectories(localDownloadDir);
        Path localDestination = localDownloadDir.resolve("shaft-download-" + UUID.randomUUID() + ".txt");
        String remotePath = joinRemotePath(config.remoteTempDir(), "shaft-upload-" + UUID.randomUUID() + ".txt");

        try {
            Files.writeString(localSource, marker, StandardCharsets.UTF_8);
            String uploadedPath = remote.uploadFile(localSource.toString(), remotePath);
            String downloadedPath = remote.downloadFile(uploadedPath, localDestination.toString());

            Validations.assertThat().object(uploadedPath).isEqualTo(remotePath).perform();
            Validations.assertThat().object(downloadedPath).contains(localDestination.getFileName().toString()).perform();
            Validations.assertThat().object(Files.readString(Path.of(downloadedPath), StandardCharsets.UTF_8))
                    .contains(marker).perform();
        } finally {
            remote.performTerminalCommand("rm -f " + shellQuote(remotePath));
            remote.quit();
            Files.deleteIfExists(localSource);
            Files.deleteIfExists(localDestination);
        }
    }

    @Test(description = "quit should disconnect the reusable SSH session")
    public void quitShouldDisconnectReusableSession() {
        SshConfig config = requireConfig();
        TerminalActions remote = createRemoteTerminal(config);
        try {
            remote.performTerminalCommand("echo session-warmup");
            Session session = remote.getJschSession();
            Validations.assertThat().object(session.isConnected()).isEqualTo(true).perform();

            remote.quit();

            Validations.assertThat().object(session.isConnected()).isEqualTo(false).perform();
            remote.quit();
        } finally {
            remote.quit();
        }
    }

    @Test(description = "Local port forwarding should assign a usable local port")
    public void forwardLocalPortShouldReturnAssignedPort() {
        SshConfig config = requireConfig();
        TerminalActions remote = createRemoteTerminal(config);
        String forwardHost = optionalEnv("TEST_SSH_FORWARD_TARGET_HOST").orElse("127.0.0.1");
        int forwardPort = optionalEnv("TEST_SSH_FORWARD_TARGET_PORT")
                .map(Integer::parseInt)
                .orElse(config.port());

        try {
            String localPort = remote.forwardLocalPort(0, forwardHost, forwardPort);
            int assignedPort = Integer.parseInt(localPort);

            Validations.assertThat().number((double) assignedPort).isGreaterThan(0.0).perform();
        } finally {
            remote.quit();
        }
    }

    @Test(description = "Optional env-var passthrough when TEST_SSH_ACCEPT_ENV=true")
    public void optionalEnvVarPassthroughWhenAcceptEnvEnabled() {
        if (!Boolean.parseBoolean(optionalEnv("TEST_SSH_ACCEPT_ENV").orElse("false"))) {
            throw new SkipException("Set TEST_SSH_ACCEPT_ENV=true to run env-var passthrough coverage.");
        }

        SshConfig config = requireConfig();
        TerminalActions remote = createRemoteTerminal(config);
        try {
            String output = remote.performTerminalCommand(
                    "sh -c 'printf %s \"$SHAFT_REMOTE_MARKER\"'",
                    Map.of("SHAFT_REMOTE_MARKER", "integration-env-marker"));

            Validations.assertThat().object(output).contains("integration-env-marker").perform();
        } finally {
            remote.quit();
        }
    }

    private static SshConfig requireConfig() {
        return loadConfig().orElseThrow(() -> new SkipException(SKIP_MESSAGE));
    }

    private static Optional<SshConfig> loadConfig() {
        String host = System.getenv("TEST_SSH_HOST");
        String port = System.getenv("TEST_SSH_PORT");
        String username = System.getenv("TEST_SSH_USERNAME");
        String keyFolder = System.getenv("TEST_SSH_KEY_FOLDER");
        String keyName = System.getenv("TEST_SSH_KEY_NAME");
        String remoteTempDir = System.getenv("TEST_SSH_TMP_DIR");

        if (isBlank(host) || isBlank(port) || isBlank(username) || isBlank(keyFolder)
                || isBlank(keyName) || isBlank(remoteTempDir)) {
            return Optional.empty();
        }

        return Optional.of(new SshConfig(
                host.trim(),
                Integer.parseInt(port.trim()),
                username.trim(),
                keyFolder.trim(),
                keyName.trim(),
                remoteTempDir.trim()));
    }

    private static TerminalActions createRemoteTerminal(SshConfig config) {
        return SHAFT.CLI.remoteTerminal(
                config.host(),
                config.port(),
                config.username(),
                config.keyFolder(),
                config.keyName());
    }

    private static Optional<String> optionalEnv(String name) {
        String value = System.getenv(name);
        return isBlank(value) ? Optional.empty() : Optional.of(value.trim());
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }

    private static String joinRemotePath(String directory, String fileName) {
        if (directory.endsWith("/")) {
            return directory + fileName;
        }
        return directory + "/" + fileName;
    }

    private static String shellQuote(String value) {
        return "'" + value.replace("'", "'\"'\"'") + "'";
    }
}
