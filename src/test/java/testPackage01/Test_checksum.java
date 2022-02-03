package testPackage01;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.validation.Assertions;
import org.testng.annotations.Test;

public class Test_checksum {
    @Test
    public void readLocalFileChecksum() {
        String targetFileFolderPath = "";
        String targetFileName = "generate_allure_report.sh";
        String expectedHash = "5f389bb767ff94e5f39fd6d588fb4b8c4a46b7c69f8a129fc1c7be6c59f43f74";

        TerminalActions terminalSession = new TerminalActions();
        String actualHash = FileActions.getFileChecksum(terminalSession, targetFileFolderPath, targetFileName);

        Assertions.assertEquals(expectedHash, actualHash);
    }

    @Test
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
        String actualHash = FileActions.getFileChecksum(terminalSession, targetFileFolderPath, targetFileName);

        Assertions.assertEquals(expectedHash, actualHash);
    }

    @Test
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
        String actualHash = FileActions.getFileChecksum(terminalSession, targetFileFolderPath, targetFileName,
                pathToTempDirectoryOnRemoteMachine);

        Assertions.assertEquals(expectedHash, actualHash);
    }
}
