package testPackage.legacy;

import com.shaft.cli.SshShellOptions;
import com.shaft.cli.SshShellSession;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.regex.Pattern;

public class SshShellIntegrationTest {
    @Test(description = "SSH shell prompt-response flow over reusable remote terminal")
    public void sshShellPromptResponseFlow() {
        String sshHostName = System.getenv("TEST_SSH_HOST");
        String sshPort = System.getenv("TEST_SSH_PORT");
        String sshUsername = System.getenv("TEST_SSH_USERNAME");
        String sshKeyFileFolderName = System.getenv("TEST_SSH_KEY_FOLDER");
        String sshKeyFileName = System.getenv("TEST_SSH_KEY_NAME");

        if (isBlank(sshHostName) || isBlank(sshPort) || isBlank(sshUsername)
                || isBlank(sshKeyFileFolderName) || isBlank(sshKeyFileName)) {
            throw new SkipException("TEST_SSH_* environment variables are not configured.");
        }

        TerminalActions remote = SHAFT.CLI.remoteTerminal(
                sshHostName,
                Integer.parseInt(sshPort),
                sshUsername,
                sshKeyFileFolderName,
                sshKeyFileName);

        SshShellOptions shellOptions = SshShellOptions.builder()
                .pty(true)
                .defaultTimeout(Duration.ofSeconds(30))
                .build();

        try (SshShellSession shell = remote.openShell(shellOptions)) {
            shell.sendLine("printf ready; read line; echo done-$line");
            shell.readUntil(Pattern.compile("ready"));
            shell.sendLine("shaft-line");
            String finalOutput = shell.readUntil(Pattern.compile("done-shaft-line"));
            Validations.assertThat().object(finalOutput).contains("done-shaft-line");
        } finally {
            remote.quit();
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }
}
