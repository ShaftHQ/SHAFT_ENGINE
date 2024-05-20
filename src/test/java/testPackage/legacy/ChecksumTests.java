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
        String expectedHash = "f31105c8059fb10caba965b489fa97a88212ed529916bf3d8841e5ca7d076eb8";

        TerminalActions terminalSession = new TerminalActions();
        String actualHash = FileActions.getInstance().getFileChecksum(terminalSession, targetFileFolderPath, targetFileName);

        Validations.assertThat().object(actualHash).equals(expectedHash);
    }

    @Test(enabled = false)
    public void readRemoteFileChecksum() {
        String targetFileFolderPath = "/home/incorta/Farid/";
        String targetFileName = "View_SV.zip";
        String expectedHash = "c391f54506dd203515c5206d0dd4094c1ece134af5b61a2c17b4c48a45199fd3";

        String sshHostName = "72.55.136.25";
        int sshPortNumber = 6444;
        String sshUsername = "incorta";
        String sshKeyFileFolderName = "/home/incorta-mohab/git/qa/src/test/resources/TestDataFiles/";
        String sshKeyFileName = "incorta.key";

        TerminalActions terminalSession = new TerminalActions(sshHostName, sshPortNumber, sshUsername,
                sshKeyFileFolderName, sshKeyFileName);
        String actualHash = FileActions.getInstance().getFileChecksum(terminalSession, targetFileFolderPath, targetFileName);

        Validations.assertThat().object(actualHash).equals(expectedHash);
    }

    @Test(enabled = false)
    public void readRemoteDockerizedFileChecksum() {
        String targetFileFolderPath = "/home/incorta/Farid/";
        String targetFileName = "Ahmed.csv";
        String expectedHash = "9269bb9b52326511b8ff4eaf050050d8437032915536fb70296101e3a70ff173";
        String pathToTempDirectoryOnRemoteMachine = "/home/incorta/temp";

        String sshHostName = "72.55.136.25";
        int sshPortNumber = 6444;
        String sshUsername = "incorta";
        String sshKeyFileFolderName = "/home/incorta-mohab/git/qa/src/test/resources/TestDataFiles/";
        String sshKeyFileName = "incorta.key";
        String dockerName = "farid";
        String dockerUsername = "incorta";

        TerminalActions terminalSession = new TerminalActions(sshHostName, sshPortNumber, sshUsername,
                sshKeyFileFolderName, sshKeyFileName, dockerName, dockerUsername);
        String actualHash = FileActions.getInstance().getFileChecksum(terminalSession, targetFileFolderPath, targetFileName,
                pathToTempDirectoryOnRemoteMachine);

        Validations.assertThat().object(actualHash).equals(expectedHash);
    }
}
