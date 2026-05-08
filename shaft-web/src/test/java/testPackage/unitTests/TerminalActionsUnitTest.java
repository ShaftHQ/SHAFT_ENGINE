package testPackage.unitTests;

import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.BufferedReader;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Unit tests for {@link TerminalActions}.
 * Validates constructors, factory methods, and terminal state checks
 * without requiring external services (SSH, Docker).
 */
public class TerminalActionsUnitTest {
    private static final Path TEMP_DIR = Path.of("target", "temp", "terminal-actions-unit");

    // --- Constructor and Factory Method Tests ---

    @Test(description = "Default constructor should create a local, non-remote, non-dockerized terminal")
    public void defaultConstructorShouldCreateLocalTerminal() {
        TerminalActions terminal = new TerminalActions();
        Assert.assertFalse(terminal.isRemoteTerminal(),
                "Default terminal should not be remote");
        Assert.assertFalse(terminal.isDockerizedTerminal(),
                "Default terminal should not be dockerized");
    }

    @Test(description = "Constructor with asynchronous flag should create a local terminal")
    public void asyncConstructorShouldCreateLocalTerminal() {
        TerminalActions terminal = new TerminalActions(true);
        Assert.assertFalse(terminal.isRemoteTerminal(),
                "Async terminal should not be remote");
        Assert.assertFalse(terminal.isDockerizedTerminal(),
                "Async terminal should not be dockerized");
    }

    @Test(description = "Docker constructor should create a dockerized, non-remote terminal")
    public void dockerConstructorShouldCreateDockerizedTerminal() {
        TerminalActions terminal = new TerminalActions("myContainer", "root");
        Assert.assertFalse(terminal.isRemoteTerminal(),
                "Docker terminal should not be remote");
        Assert.assertTrue(terminal.isDockerizedTerminal(),
                "Docker terminal should be dockerized");
        Assert.assertEquals(terminal.getDockerName(), "myContainer",
                "Docker name should match constructor argument");
        Assert.assertEquals(terminal.getDockerUsername(), "root",
                "Docker username should match constructor argument");
    }

    @Test(description = "SSH constructor should create a remote, non-dockerized terminal")
    public void sshConstructorShouldCreateRemoteTerminal() {
        TerminalActions terminal = new TerminalActions(
                "host.example.com", 22, "user", "/keys/", "id_rsa");
        Assert.assertTrue(terminal.isRemoteTerminal(),
                "SSH terminal should be remote");
        Assert.assertFalse(terminal.isDockerizedTerminal(),
                "SSH terminal should not be dockerized");
        Assert.assertEquals(terminal.getSshHostName(), "host.example.com",
                "SSH host name should match");
        Assert.assertEquals(terminal.getSshPortNumber(), 22,
                "SSH port number should match");
        Assert.assertEquals(terminal.getSshUsername(), "user",
                "SSH username should match");
        Assert.assertEquals(terminal.getSshKeyFileFolderName(), "/keys/",
                "SSH key folder should match");
        Assert.assertEquals(terminal.getSshKeyFileName(), "id_rsa",
                "SSH key file name should match");
    }

    @Test(description = "Combined SSH + Docker constructor should create both remote and dockerized terminal")
    public void combinedSshDockerConstructorShouldCreateBoth() {
        TerminalActions terminal = new TerminalActions(
                "host.example.com", 2222, "admin", "/ssh/", "key.pem",
                "appContainer", "appUser");
        Assert.assertTrue(terminal.isRemoteTerminal(),
                "Combined terminal should be remote");
        Assert.assertTrue(terminal.isDockerizedTerminal(),
                "Combined terminal should be dockerized");
        Assert.assertEquals(terminal.getSshHostName(), "host.example.com");
        Assert.assertEquals(terminal.getSshPortNumber(), 2222);
        Assert.assertEquals(terminal.getDockerName(), "appContainer");
        Assert.assertEquals(terminal.getDockerUsername(), "appUser");
    }

    // --- Factory Method Tests ---

    @Test(description = "getInstance() should return a non-null local terminal")
    public void getInstanceShouldReturnLocalTerminal() {
        TerminalActions terminal = TerminalActions.getInstance();
        Assert.assertNotNull(terminal, "getInstance() should return a non-null instance");
        Assert.assertFalse(terminal.isRemoteTerminal());
        Assert.assertFalse(terminal.isDockerizedTerminal());
    }

    @Test(description = "getInstance(boolean) should return a non-null terminal")
    public void getInstanceWithAsyncShouldReturnTerminal() {
        TerminalActions terminal = TerminalActions.getInstance(true);
        Assert.assertNotNull(terminal, "getInstance(true) should return a non-null instance");
        Assert.assertFalse(terminal.isRemoteTerminal());
    }

    @Test(description = "getInstance(boolean, boolean) should return a non-null terminal")
    public void getInstanceWithAsyncAndVerboseShouldReturnTerminal() {
        TerminalActions terminal = TerminalActions.getInstance(true, true);
        Assert.assertNotNull(terminal, "getInstance(true, true) should return a non-null instance");
    }

    @Test(description = "getInstance(boolean, boolean, boolean) should return a non-null terminal")
    public void getInstanceWithAllFlagsShouldReturnTerminal() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        Assert.assertNotNull(terminal, "getInstance(false, false, true) should return a non-null instance");
    }

    // --- Default SSH Values ---

    @Test(description = "Default SSH port should be 22")
    public void defaultSshPortShouldBe22() {
        TerminalActions terminal = new TerminalActions();
        Assert.assertEquals(terminal.getSshPortNumber(), 22,
                "Default SSH port should be 22");
    }

    @Test(description = "Default SSH host name should be empty string")
    public void defaultSshHostNameShouldBeEmpty() {
        TerminalActions terminal = new TerminalActions();
        Assert.assertEquals(terminal.getSshHostName(), "",
                "Default SSH host name should be empty");
    }

    @Test(description = "Default docker name should be empty string")
    public void defaultDockerNameShouldBeEmpty() {
        TerminalActions terminal = new TerminalActions();
        Assert.assertEquals(terminal.getDockerName(), "",
                "Default docker name should be empty");
    }

    // --- isRemoteTerminal / isDockerizedTerminal edge cases ---

    @Test(description = "isRemoteTerminal should be false when sshHostName is empty")
    public void isRemoteTerminalShouldBeFalseWhenHostEmpty() {
        TerminalActions terminal = new TerminalActions();
        Assert.assertFalse(terminal.isRemoteTerminal(),
                "Terminal with empty SSH host name should not be remote");
    }

    @Test(description = "isDockerizedTerminal should be false when dockerName is empty")
    public void isDockerizedTerminalShouldBeFalseWhenDockerNameEmpty() {
        TerminalActions terminal = new TerminalActions();
        Assert.assertFalse(terminal.isDockerizedTerminal(),
                "Terminal with empty docker name should not be dockerized");
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        if (Files.exists(TEMP_DIR)) {
            com.shaft.cli.FileActions.getInstance(true).deleteFolder(TEMP_DIR.toString());
        }
    }

    @Test(description = "performTerminalCommands should execute split commands and capture logs")
    public void performTerminalCommandsShouldExecuteSplitCommands() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo first && echo second");
        Assert.assertTrue(log.contains("first"), "Expected first command output in logs.");
        Assert.assertTrue(log.contains("second"), "Expected second command output in logs.");
    }

    @Test(description = "performTerminalCommands should execute from cd-prefixed directory")
    public void performTerminalCommandsShouldRunFromChangedDirectory() throws Exception {
        Files.createDirectories(TEMP_DIR);
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommands(List.of("cd " + TEMP_DIR.toAbsolutePath(), "pwd"));
        Assert.assertTrue(log.contains(TEMP_DIR.toAbsolutePath().toString()),
                "Expected command log to include changed working directory.");
    }

    @Test(description = "performTerminalCommands should split semicolon-separated command chains")
    public void performTerminalCommandsShouldSplitSemicolonCommands() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo alpha ; echo beta");
        Assert.assertTrue(log.contains("alpha"));
        Assert.assertTrue(log.contains("beta"));
    }

    @Test(description = "performTerminalCommand should report action when running through public non-internal flow")
    public void performTerminalCommandShouldRunPublicReportingFlow() {
        TerminalActions terminal = new TerminalActions();
        String log = terminal.performTerminalCommand("echo public-flow");
        Assert.assertTrue(log.contains("public-flow"));
    }

    @Test(description = "verbose local execution should cover bat normalization and log streaming paths")
    public void performTerminalCommandShouldNormalizeBatAndLogVerboseOutput() {
        TerminalActions terminal = TerminalActions.getInstance(false, true, true);
        String log = terminal.performTerminalCommand("dummy.bat");
        Assert.assertTrue(log.toLowerCase().contains("dummy.bat"));
    }

    @Test(description = "interrupted shell wait should propagate runtime failure from local execution")
    public void performTerminalCommandShouldHandleInterruptedExecutionPath() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        try {
            Thread.currentThread().interrupt();
            Assert.expectThrows(RuntimeException.class,
                    () -> terminal.performTerminalCommand("echo interrupted"));
        } finally {
            Thread.interrupted();
        }
    }

    @Test(description = "asynchronous execution should force shutdownNow when timeout is zero")
    public void performTerminalCommandShouldCoverAsyncTimeoutShutdownPath() {
        long originalTimeout = SHAFT.Properties.timeouts.shellSessionTimeout();
        try {
            SHAFT.Properties.timeouts.set().shellSessionTimeout(0);
            TerminalActions terminal = TerminalActions.getInstance(true, false, true);
            String log = terminal.performTerminalCommand("sleep 1");
            Assert.assertEquals(log, "");
        } finally {
            SHAFT.Properties.timeouts.set().shellSessionTimeout(originalTimeout);
        }
    }

    @Test(description = "asynchronous execution should return empty logs and asynchronous status")
    public void performTerminalCommandShouldHandleAsynchronousExecution() {
        TerminalActions terminal = TerminalActions.getInstance(true, false, true);
        String log = terminal.performTerminalCommand("echo async");
        Assert.assertEquals(log, "", "Asynchronous execution should not capture command output.");
    }

    @Test(description = "buildLongCommand should prepend docker exec wrapper for dockerized terminals")
    public void buildLongCommandShouldWrapDockerCommand() throws Exception {
        TerminalActions terminal = new TerminalActions("myContainer", "root");
        Method buildLongCommand = TerminalActions.class.getDeclaredMethod("buildLongCommand", List.class);
        buildLongCommand.setAccessible(true);

        String command = (String) buildLongCommand.invoke(terminal, List.of("echo alpha", "echo beta"));

        Assert.assertTrue(command.startsWith("docker exec -u root -i myContainer timeout "),
                "Dockerized terminal command should be prefixed with docker exec.");
        Assert.assertTrue(command.contains("echo alpha && echo beta"),
                "Dockerized command should keep original command chain.");
    }

    @Test(description = "getProcessBuilder should use expected command template based on platform and flags")
    public void getProcessBuilderShouldBuildCommandByFlags() throws Exception {
        Method getProcessBuilder = TerminalActions.class.getDeclaredMethod("getProcessBuilder", String.class, String.class, boolean.class);
        getProcessBuilder.setAccessible(true);

        TerminalActions windowsSyncTerminal = new TerminalActions();
        ProcessBuilder windowsSyncBuilder = (ProcessBuilder) getProcessBuilder.invoke(windowsSyncTerminal, "echo hello", ".", true);
        Assert.assertTrue(windowsSyncBuilder.command().contains("powershell.exe"));
        Assert.assertTrue(windowsSyncBuilder.command().contains("-Command"));

        TerminalActions windowsAsyncVerboseTerminal = TerminalActions.getInstance(true, true, true);
        ProcessBuilder windowsAsyncVerboseBuilder = (ProcessBuilder) getProcessBuilder.invoke(windowsAsyncVerboseTerminal, "echo hello", ".", true);
        Assert.assertTrue(windowsAsyncVerboseBuilder.command().stream().anyMatch(part -> part.contains("Start-Process")),
                "Asynchronous verbose windows command should use Start-Process.");

        TerminalActions linuxTerminal = new TerminalActions();
        ProcessBuilder linuxBuilder = (ProcessBuilder) getProcessBuilder.invoke(linuxTerminal, "echo hello", ".", false);
        Assert.assertEquals(linuxBuilder.command(), List.of("sh", "-c", "echo hello"));
    }

    @Test(description = "readConsoleLogs should aggregate lines and handle null readers safely")
    public void readConsoleLogsShouldHandleContentAndNull() throws Exception {
        Method readConsoleLogs = TerminalActions.class.getDeclaredMethod("readConsoleLogs", BufferedReader.class);
        readConsoleLogs.setAccessible(true);
        TerminalActions terminal = new TerminalActions();

        String multiLineLogs = (String) readConsoleLogs.invoke(terminal,
                new BufferedReader(new StringReader("line1" + System.lineSeparator() + "line2")));
        Assert.assertTrue(multiLineLogs.contains("line1"));
        Assert.assertTrue(multiLineLogs.contains("line2"));

        String nullReaderLogs = (String) readConsoleLogs.invoke(terminal, new Object[]{null});
        Assert.assertEquals(nullReaderLogs, "");
    }

    @Test(description = "reportActionResult should format pass and fail messages for different attachment shapes")
    public void reportActionResultShouldHandlePassAndFailScenarios() throws Exception {
        Method reportActionResult = TerminalActions.class.getDeclaredMethod("reportActionResult",
                String.class, String.class, String.class, Boolean.class, Exception[].class);
        reportActionResult.setAccessible(true);

        String passMessage = (String) reportActionResult.invoke(null,
                "performTerminalCommand", "inputData", "sample logs", true, new Exception[]{});
        Assert.assertTrue(passMessage.contains("successfully performed"));
        Assert.assertTrue(passMessage.contains("inputData"));

        String longTestData = "x".repeat(600);
        String failMessage = (String) reportActionResult.invoke(null,
                "performTerminalCommand", longTestData, "sample logs", false,
                new Exception[]{new RuntimeException("forced failure")});
        Assert.assertTrue(failMessage.contains("failed"));

        String passWithoutAttachments = (String) reportActionResult.invoke(null,
                "performTerminalCommand", null, null, true, new Exception[]{});
        Assert.assertTrue(passWithoutAttachments.contains("successfully performed"));
    }

    @Test(description = "private pass/fail action helpers should execute and propagate failures")
    public void privatePassAndFailActionHelpersShouldBeCovered() throws Exception {
        TerminalActions terminal = new TerminalActions();
        Method passActionWithActionName = TerminalActions.class.getDeclaredMethod("passAction", String.class, String.class, String.class);
        Method passActionWithoutActionName = TerminalActions.class.getDeclaredMethod("passAction", String.class, String.class);
        Method failActionWithActionName = TerminalActions.class.getDeclaredMethod("failAction", String.class, String.class, Exception[].class);
        Method failActionWithoutActionName = TerminalActions.class.getDeclaredMethod("failAction", String.class, Exception[].class);

        passActionWithActionName.setAccessible(true);
        passActionWithoutActionName.setAccessible(true);
        failActionWithActionName.setAccessible(true);
        failActionWithoutActionName.setAccessible(true);

        passActionWithActionName.invoke(terminal, "performTerminalCommand", "input", "log");
        passActionWithoutActionName.invoke(terminal, "input", "log");

        InvocationTargetException firstFailure = Assert.expectThrows(InvocationTargetException.class,
                () -> failActionWithActionName.invoke(terminal, "performTerminalCommand", "input",
                        (Object) new Exception[]{new RuntimeException("forced failure")}));
        Assert.assertTrue(firstFailure.getCause() instanceof RuntimeException);

        InvocationTargetException secondFailure = Assert.expectThrows(InvocationTargetException.class,
                () -> failActionWithoutActionName.invoke(terminal, "input",
                        (Object) new Exception[]{new RuntimeException("forced failure")}));
        Assert.assertTrue(secondFailure.getCause() instanceof RuntimeException);
    }

    @Test(description = "createSSHsession should surface connection failures for invalid settings")
    public void createSSHsessionShouldHandleConnectionFailurePath() throws Exception {
        TerminalActions terminal = new TerminalActions("non-existing-host", 22, "user", "", "");
        Method createSshSession = TerminalActions.class.getDeclaredMethod("createSSHsession");
        createSshSession.setAccessible(true);

        InvocationTargetException failure = Assert.expectThrows(InvocationTargetException.class,
                () -> createSshSession.invoke(terminal));
        Assert.assertTrue(failure.getCause() instanceof RuntimeException);
    }

    @Test(description = "createSSHsession should handle invalid key path when identity loading fails")
    public void createSSHsessionShouldHandleInvalidIdentityPath() throws Exception {
        TerminalActions terminal = new TerminalActions("127.0.0.1", 22, "user",
                TEMP_DIR.toAbsolutePath().toString() + "/", "missing_key");
        Method createSshSession = TerminalActions.class.getDeclaredMethod("createSSHsession");
        createSshSession.setAccessible(true);

        InvocationTargetException failure = Assert.expectThrows(InvocationTargetException.class,
                () -> createSshSession.invoke(terminal));
        Assert.assertTrue(failure.getCause() instanceof RuntimeException);
    }
}
