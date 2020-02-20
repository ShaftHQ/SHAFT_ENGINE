package testPackage01;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import org.testng.annotations.Test;

import java.util.Arrays;

public class Test_TerminalActions {
    @Test(enabled = false)
    public void local_copy() {
        String fileName = "generate_allure_report.sh";
        String shellResponse = "";
        TerminalActions terminalSession = new TerminalActions();
        shellResponse = FileActions.copyFile(terminalSession, FileActions.getAbsolutePath(""),
                FileActions.getAbsolutePath("copiedFiles"), fileName);
        Assertions.assertEquals("sent", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);

        shellResponse = FileActions.listFilesInDirectory(terminalSession, FileActions.getAbsolutePath("copiedFiles"));
        Assertions.assertEquals(fileName, shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Test(enabled = false)
    public void remote_list() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                System.getProperty("testDataFolderPath"), "newAutomationEnvironment.key");
        String shellResponse = FileActions.listFilesInDirectory(terminalSession, "");
        Assertions.assertEquals("backup", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);

        shellResponse = FileActions.listFilesInDirectory(terminalSession, "/home/");
        Assertions.assertEquals("incorta", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Test(enabled = false)
    public void remoteDockerized_list() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                System.getProperty("testDataFolderPath"), "newAutomationEnvironment.key", "analytics-mysql", "incorta");
        String shellResponse = "";

        shellResponse = FileActions.listFilesInDirectory(terminalSession, "");
        Assertions.assertEquals("bin", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);

        shellResponse = FileActions.listFilesInDirectory(terminalSession, "/bin/");
        Assertions.assertEquals("su", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);

        shellResponse = FileActions.listFilesInDirectory(terminalSession, "lib/");
        Assertions.assertEquals("firmware", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Test
    public void local_singleTerminalCommand() {
        TerminalActions terminalSession = new TerminalActions();
        String shellResponse = "";

        shellResponse = terminalSession.performTerminalCommand("ls");
        Assertions.assertEquals("README.md", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Test
    public void local_chainedTerminalCommands() {
        TerminalActions terminalSession = new TerminalActions();
        String shellResponse = "";

        shellResponse = terminalSession
                .performTerminalCommands(Arrays.asList("ls /home/incorta-mohab/git/qa/src/test/resources",
                        "ls /home/incorta-mohab/git/qa/src/test/resources/TestDataFiles"));
        Assertions.assertEquals("jacoco-0.8.2.zip", shellResponse, AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }

    @Test
    public void remote_singleTerminalCommand() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                System.getProperty("testDataFolderPath"), "newAutomationEnvironment.key");
        String shellResponse = "";

        shellResponse = terminalSession.performTerminalCommand("ls /home/incorta/Automation_latest_Tenant_bkp");
        Assertions.assertEquals("content", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Test
    public void remote_chainedTerminalCommands() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                System.getProperty("testDataFolderPath"), "newAutomationEnvironment.key");
        String shellResponse = "";

        shellResponse = terminalSession
                .performTerminalCommands(Arrays.asList("cd /home/incorta/Automation_latest_Tenant_bkp", "ls"));
        Assertions.assertEquals("content", shellResponse, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Test
    public void remoteDockerized_singleTerminalCommand() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                System.getProperty("testDataFolderPath"), "newAutomationEnvironment.key", "analytics-mysql", "incorta");
        String shellResponse = "";

        shellResponse = terminalSession
                .performTerminalCommand("ls /home/incorta/IncortaAnalytics_Analytics_Mysql/IncortaNode/bin");
        Assertions.assertEquals("Automation_Base.sh", shellResponse, AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }

    @Test
    public void remoteDockerized_chainedTerminalCommands() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                System.getProperty("testDataFolderPath"), "newAutomationEnvironment.key", "analytics-mysql", "incorta");
        String shellResponse = "";

        shellResponse = terminalSession.performTerminalCommands(
                Arrays.asList("cd /home/incorta/IncortaAnalytics_Analytics_Mysql/IncortaNode/bin", "ls"));
        Assertions.assertEquals("Automation_Base.sh", shellResponse, AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }

}
