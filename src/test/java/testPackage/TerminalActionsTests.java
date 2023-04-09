package testPackage;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

import java.util.Arrays;

public class TerminalActionsTests {
    @Test(enabled = false)
    public void local_copy() {
        String fileName = "generate_allure_report.sh";
        String shellResponse;
        TerminalActions terminalSession = new TerminalActions();
        shellResponse = FileActions.getInstance().copyFile(terminalSession, FileActions.getInstance().getAbsolutePath(""),
                FileActions.getInstance().getAbsolutePath("copiedFiles"), fileName);
        Validations.assertThat().object(shellResponse).contains("sent").perform();

        shellResponse = FileActions.getInstance().listFilesInDirectory(terminalSession, FileActions.getInstance().getAbsolutePath("copiedFiles"));
        Validations.assertThat().object(shellResponse).contains(fileName).perform();
    }

    @Test(enabled = false)
    public void remote_list() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                SHAFT.Properties.paths.testData(), "newAutomationEnvironment.key");
        String shellResponse = FileActions.getInstance().listFilesInDirectory(terminalSession, "");
        Validations.assertThat().object(shellResponse).contains("backup").perform();

        shellResponse = FileActions.getInstance().listFilesInDirectory(terminalSession, "/home/");
        Validations.assertThat().object(shellResponse).contains("incorta").perform();
    }

    @Test(enabled = false)
    public void remoteDockerized_list() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                SHAFT.Properties.paths.testData(), "newAutomationEnvironment.key", "analytics-mysql", "incorta");
        String shellResponse;

        shellResponse = FileActions.getInstance().listFilesInDirectory(terminalSession, "");
        Validations.assertThat().object(shellResponse).contains("bin").perform();

        shellResponse = FileActions.getInstance().listFilesInDirectory(terminalSession, "/bin/");
        Validations.assertThat().object(shellResponse).contains("su").perform();

        shellResponse = FileActions.getInstance().listFilesInDirectory(terminalSession, "lib/");
        Validations.assertThat().object(shellResponse).contains("firmware").perform();
    }

    @Test
    public void local_singleTerminalCommand() {
        TerminalActions terminalSession = new TerminalActions();
        String shellResponse;

        shellResponse = terminalSession.performTerminalCommand("ls");
        Validations.assertThat().object(shellResponse).contains("README.md").perform();
    }

    @Test
    public void local_chainedTerminalCommands() {
        TerminalActions terminalSession = new TerminalActions();
        String shellResponse;

        shellResponse = terminalSession
                .performTerminalCommands(Arrays.asList("ls /home/incorta-mohab/git/qa/src/test/resources",
                        "ls /home/incorta-mohab/git/qa/src/test/resources/TestDataFiles"));
        Validations.assertThat().object(shellResponse).contains("jacoco-0.8.2.zip").perform();
    }

    @Test
    public void remote_singleTerminalCommand() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                SHAFT.Properties.paths.testData(), "newAutomationEnvironment.key");
        String shellResponse;

        shellResponse = terminalSession.performTerminalCommand("ls /home/incorta/Automation_latest_Tenant_bkp");
        Validations.assertThat().object(shellResponse).contains("content").perform();
    }

    @Test
    public void remote_chainedTerminalCommands() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                SHAFT.Properties.paths.testData(), "newAutomationEnvironment.key");
        String shellResponse;

        shellResponse = terminalSession
                .performTerminalCommands(Arrays.asList("cd /home/incorta/Automation_latest_Tenant_bkp", "ls"));
        Validations.assertThat().object(shellResponse).contains("content").perform();
    }

    @Test
    public void remoteDockerized_singleTerminalCommand() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                SHAFT.Properties.paths.testData(), "newAutomationEnvironment.key", "analytics-mysql", "incorta");
        String shellResponse;

        shellResponse = terminalSession
                .performTerminalCommand("ls /home/incorta/IncortaAnalytics_Analytics_Mysql/IncortaNode/bin");
        Validations.assertThat().object(shellResponse).contains("Automation_Base.sh").perform();
    }

    @Test
    public void remoteDockerized_chainedTerminalCommands() {
        TerminalActions terminalSession = new TerminalActions("35.184.27.139", 22, "incorta",
                SHAFT.Properties.paths.testData(), "newAutomationEnvironment.key", "analytics-mysql", "incorta");
        String shellResponse;

        shellResponse = terminalSession.performTerminalCommands(
                Arrays.asList("cd /home/incorta/IncortaAnalytics_Analytics_Mysql/IncortaNode/bin", "ls"));
        Validations.assertThat().object(shellResponse).contains("Automation_Base.sh").perform();
    }

}
