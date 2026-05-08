package testPackage.legacy;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

public class ChecksumTests {
    @Test(enabled = false)
    public void readLocalFileChecksum() {
        String targetFileFolderPath = "";
        String targetFileName = "pom.xml";
        // Expected SHA-256 checksum for the current contents of pom.xml in targetFileFolderPath.
        // Regenerate/verify when pom.xml changes (for example: `sha256sum pom.xml`).
        String expectedHash = "f31105c8059fb10caba965b489fa97a88212ed529916bf3d8841e5ca7d076eb8";

        TerminalActions terminalSession = new TerminalActions();
        String actualHash = FileActions.getInstance().getFileChecksum(terminalSession, targetFileFolderPath, targetFileName);

        Validations.assertThat().object(actualHash).isEqualTo(expectedHash);
    }

    @Test(enabled = false)
    public void readRemoteFileChecksum() {
        String targetFileFolderPath = "/home/incorta/Farid/";
        String targetFileName = "View_SV.zip";
        // Expected SHA-256 checksum for /home/incorta/Farid/View_SV.zip on the configured remote host.
        // Recompute after replacing the file (for example on remote: `sha256sum /home/incorta/Farid/View_SV.zip`).
        String expectedHash = "c391f54506dd203515c5206d0dd4094c1ece134af5b61a2c17b4c48a45199fd3";

        String sshHostName = System.getenv("TEST_SSH_HOST");
        int sshPortNumber = Integer.parseInt(System.getenv("TEST_SSH_PORT"));
        String sshUsername = System.getenv("TEST_SSH_USERNAME");
        String sshKeyFileFolderName = System.getenv("TEST_SSH_KEY_FOLDER");
        String sshKeyFileName = System.getenv("TEST_SSH_KEY_NAME");

        TerminalActions terminalSession = new TerminalActions(sshHostName, sshPortNumber, sshUsername,
                sshKeyFileFolderName, sshKeyFileName);
        String actualHash = FileActions.getInstance().getFileChecksum(terminalSession, targetFileFolderPath, targetFileName);

        Validations.assertThat().object(actualHash).isEqualTo(expectedHash);
    }

    @Test(enabled = false)
    public void readRemoteDockerizedFileChecksum() {
        String targetFileFolderPath = "/home/incorta/Farid/";
        String targetFileName = "Ahmed.csv";
        // Expected SHA-256 checksum for /home/incorta/Farid/Ahmed.csv when accessed via the configured dockerized session.
        // Regenerate whenever Ahmed.csv content changes (for example: `sha256sum /home/incorta/Farid/Ahmed.csv`).
        String expectedHash = "9269bb9b52326511b8ff4eaf050050d8437032915536fb70296101e3a70ff173";
        String pathToTempDirectoryOnRemoteMachine = "/home/incorta/temp";

        String sshHostName = System.getenv("TEST_SSH_HOST");
        int sshPortNumber = Integer.parseInt(System.getenv("TEST_SSH_PORT"));
        String sshUsername = System.getenv("TEST_SSH_USERNAME");
        String sshKeyFileFolderName = System.getenv("TEST_SSH_KEY_FOLDER");
        String sshKeyFileName = System.getenv("TEST_SSH_KEY_NAME");
        String dockerName = System.getenv("TEST_DOCKER_NAME");
        String dockerUsername = System.getenv("TEST_DOCKER_USERNAME");

        TerminalActions terminalSession = new TerminalActions(sshHostName, sshPortNumber, sshUsername,
                sshKeyFileFolderName, sshKeyFileName, dockerName, dockerUsername);
        String actualHash = FileActions.getInstance().getFileChecksum(terminalSession, targetFileFolderPath, targetFileName,
                pathToTempDirectoryOnRemoteMachine);

        Validations.assertThat().object(actualHash).isEqualTo(expectedHash);
    }
}
