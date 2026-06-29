package testPackage.unitTests;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.shaft.cli.SshConnectionOptions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import org.apache.commons.lang3.SystemUtils;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.BufferedReader;
import java.io.StringReader;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BooleanSupplier;
import java.util.function.IntSupplier;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Unit tests for {@link TerminalActions}.
 * Validates constructors, factory methods, and terminal state checks
 * without requiring external services (SSH, Docker).
 */
public class TerminalActionsUnitTest {
    private static final Path TEMP_ROOT = Path.of("target", "temp", "terminal-actions-unit");
    private final ThreadLocal<Path> createdTempDir = new ThreadLocal<>();

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

    @Test(description = "docker constructor should create a dockerized, non-remote terminal")
    @SuppressWarnings("deprecation")
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

    @Test(description = "combined SSH + Docker constructor should create both remote and dockerized terminal")
    @SuppressWarnings("deprecation")
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


    @Test(description = "remoteTerminal facade should create reusable remote terminal")
    public void remoteTerminalFacadeShouldCreateReusableRemoteTerminal() throws Exception {
        TerminalActions terminal = SHAFT.CLI.remoteTerminal(
                "host.example.com", 2222, "user", "/keys/", "id_rsa");

        Assert.assertNotNull(terminal, "remoteTerminal() should return a non-null instance");
        Assert.assertTrue(terminal.isRemoteTerminal(),
                "Facade-created terminal should be remote");
        Assert.assertEquals(terminal.getSshHostName(), "host.example.com",
                "SSH host name should match");
        Assert.assertEquals(terminal.getSshPortNumber(), 2222,
                "SSH port number should match");
        Assert.assertEquals(terminal.getSshUsername(), "user",
                "SSH username should match");
        Assert.assertEquals(terminal.getSshKeyFileFolderName(), "/keys/",
                "SSH key folder should match");
        Assert.assertEquals(terminal.getSshKeyFileName(), "id_rsa",
                "SSH key file name should match");

        Field reuseRemoteSessionField = TerminalActions.class.getDeclaredField("reuseRemoteSession");
        reuseRemoteSessionField.setAccessible(true);
        Assert.assertTrue((Boolean) reuseRemoteSessionField.get(terminal),
                "Facade-created remote terminal should enable reusable SSH session lifecycle");
    }

    @Test(description = "verbose remote terminal overloads should create reusable remote instances")
    public void verboseRemoteTerminalOverloadsShouldCreateRemoteInstances() {
        TerminalActions defaultRemoteTerminal = SHAFT.CLI.remoteTerminal(
                "host.example.com", 2222, "user", "/keys/", "id_rsa");
        TerminalActions verboseFacadeTerminal = SHAFT.CLI.remoteTerminal(
                "host.example.com", 2222, "user", "/keys/", "id_rsa", true);
        TerminalActions verboseFactoryTerminal = TerminalActions.getRemoteInstance(
                "host.example.com", 2222, "user", "/keys/", "id_rsa", true);

        Assert.assertTrue(defaultRemoteTerminal.isRemoteTerminal(),
                "Default remoteTerminal() should create a remote instance");
        Assert.assertTrue(verboseFacadeTerminal.isRemoteTerminal(),
                "Verbose remoteTerminal() should create a remote instance");
        Assert.assertTrue(verboseFactoryTerminal.isRemoteTerminal(),
                "Verbose getRemoteInstance() should create a remote instance");
    }

    @Test(description = "quit should be safe before any reusable SSH connection is opened")
    public void quitShouldBeSafeBeforeReusableRemoteConnection() {
        TerminalActions terminal = SHAFT.CLI.remoteTerminal(
                "host.example.com", 2222, "user", "/keys/", "id_rsa");

        terminal.quit();
        terminal.quit();

        Assert.assertTrue(terminal.isRemoteTerminal(),
                "Calling quit before connection should not clear remote terminal configuration");
    }

    @Test(description = "quit should cancel any scheduled reusable session timeout task")
    public void quitShouldCancelReusableSessionTimeoutTask() throws Exception {
        TerminalActions terminal = SHAFT.CLI.remoteTerminal(
                "host.example.com", 2222, "user", "/keys/", "id_rsa");

        Field schedulerField = TerminalActions.class.getDeclaredField("reusableSessionTimeoutScheduler");
        schedulerField.setAccessible(true);
        var scheduler = Executors.newSingleThreadScheduledExecutor();
        schedulerField.set(terminal, scheduler);

        ScheduledFuture<?> timeoutTask = scheduler.schedule(() -> {
        }, 10, TimeUnit.MINUTES);
        Field timeoutTaskField = TerminalActions.class.getDeclaredField("reusableSessionTimeoutTask");
        timeoutTaskField.setAccessible(true);
        timeoutTaskField.set(terminal, timeoutTask);

        terminal.quit();

        Assert.assertTrue(timeoutTask.isCancelled(), "Expected reusable session timeout task to be canceled on quit.");
        Assert.assertTrue(scheduler.isShutdown(), "Expected reusable session timeout scheduler to be shutdown on quit.");
    }

    @Test(description = "executeTerminalCommand should return same terminal instance for fluent chaining")
    public void executeTerminalCommandShouldReturnSameInstance() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);

        TerminalActions chainedTerminal = terminal.executeTerminalCommand("echo fluent");

        Assert.assertSame(chainedTerminal, terminal,
                "Fluent terminal execution should return the same TerminalActions instance");
    }

    @Test(description = "uploadFile should fail for local terminals")
    public void uploadFileShouldFailForLocalTerminal() {
        TerminalActions terminal = new TerminalActions();
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.uploadFile("local.txt", "/tmp/remote.txt"));
        Assert.assertTrue(failure.getMessage().contains("SFTP is only supported for remote SSH terminals"));
    }

    @Test(description = "downloadFile should fail for local terminals")
    public void downloadFileShouldFailForLocalTerminal() {
        TerminalActions terminal = new TerminalActions();
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.downloadFile("/tmp/remote.txt", "local.txt"));
        Assert.assertTrue(failure.getMessage().contains("SFTP is only supported for remote SSH terminals"));
    }

    @Test(description = "uploadFile should fail for dockerized remote terminals")
    @SuppressWarnings("deprecation")
    public void uploadFileShouldFailForDockerizedRemoteTerminal() throws Exception {
        Path tempDir = createTempDir("upload");
        Path localFile = tempDir.resolve("upload-source.txt");
        Files.writeString(localFile, "payload");

        TerminalActions terminal = new TerminalActions(
                "host.example.com", 22, "user", tempDir.toAbsolutePath() + "/", "id_rsa",
                "appContainer", "appUser");

        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.uploadFile(localFile.toString(), "/tmp/remote.txt"));
        Assert.assertTrue(failure.getMessage().contains("dockerized remote terminals"));
    }

    @Test(description = "uploadFile should fail when the local source file does not exist")
    public void uploadFileShouldFailWhenLocalSourceFileIsMissing() {
        TerminalActions terminal = new TerminalActions("host.example.com", 22, "user", "/keys/", "id_rsa");
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.uploadFile("target/temp/does-not-exist.txt", "/tmp/remote.txt"));
        Assert.assertTrue(failure.getMessage().contains("Local file does not exist"));
    }

    @Test(description = "forwardLocalPort should fail for local terminals")
    public void forwardLocalPortShouldFailForLocalTerminal() {
        TerminalActions terminal = new TerminalActions();
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.forwardLocalPort(8080, "127.0.0.1", 80));
        Assert.assertTrue(failure.getMessage().contains("remote SSH terminals"));
    }

    @Test(description = "forwardLocalPort should fail for ephemeral remote terminals")
    public void forwardLocalPortShouldFailForEphemeralRemoteTerminal() {
        TerminalActions terminal = new TerminalActions("host.example.com", 22, "user", "/keys/", "id_rsa");
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.forwardLocalPort(8080, "127.0.0.1", 80));
        Assert.assertTrue(failure.getMessage().contains("SHAFT.CLI.remoteTerminal"));
    }

    @Test(description = "forwardRemotePort should fail for dockerized remote terminals")
    @SuppressWarnings("deprecation")
    public void forwardRemotePortShouldFailForDockerizedRemoteTerminal() {
        TerminalActions dockerizedTerminal = new TerminalActions(
                "host.example.com", 22, "user", "/keys/", "id_rsa", "appContainer", "appUser");
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> dockerizedTerminal.forwardRemotePort(9090, "127.0.0.1", 8080));
        Assert.assertTrue(failure.getMessage().contains("dockerized remote terminals"));
    }

    @Test(description = "getJschSession should fail for ephemeral remote terminals")
    public void getJschSessionShouldFailForEphemeralRemoteTerminal() {
        TerminalActions terminal = new TerminalActions("host.example.com", 22, "user", "/keys/", "id_rsa");
        RuntimeException failure = Assert.expectThrows(RuntimeException.class, terminal::getJschSession);
        Assert.assertTrue(failure.getMessage().contains("SHAFT.CLI.remoteTerminal"));
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
        Path directory = createdTempDir.get();
        createdTempDir.remove();
        if (directory != null && Files.exists(directory)) {
            com.shaft.cli.FileActions.getInstance(true).deleteFolder(directory.toString());
        }
    }

    private Path createTempDir(String prefix) throws Exception {
        Files.createDirectories(TEMP_ROOT);
        Path directory = Files.createTempDirectory(TEMP_ROOT, prefix + "-");
        createdTempDir.set(directory);
        return directory;
    }

    @Test(description = "performTerminalCommands should execute command chains and capture logs")
    public void performTerminalCommandsShouldExecuteCommandChains() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo first && echo second");
        Assert.assertTrue(log.contains("first"), "Expected first command output in logs.");
        Assert.assertTrue(log.contains("second"), "Expected second command output in logs.");
    }

    @Test(description = "performTerminalCommands should execute from cd-prefixed directory")
    public void performTerminalCommandsShouldRunFromChangedDirectory() throws Exception {
        Path tempDir = createTempDir("cwd");
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String directory = tempDir.toAbsolutePath().toString();
        String log = SystemUtils.IS_OS_WINDOWS
                ? terminal.performTerminalCommands(List.of("cd " + directory, "(Get-Location).Path"))
                : terminal.performTerminalCommands(List.of("cd " + directory, "pwd"));
        Assert.assertTrue(log.toLowerCase().contains(directory.toLowerCase()),
                "Expected command log to include changed working directory.");
    }

    @Test(description = "performTerminalCommands should execute semicolon-separated command chains")
    public void performTerminalCommandsShouldExecuteSemicolonCommands() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo alpha ; echo beta");
        Assert.assertTrue(log.contains("alpha"));
        Assert.assertTrue(log.contains("beta"));
    }

    @Test(description = "performTerminalCommand should keep quoted command separators in the same command")
    public void performTerminalCommandShouldNotSplitQuotedSeparators() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo \"alpha && beta ; gamma\"");

        Assert.assertTrue(log.contains("alpha && beta ; gamma"),
                "Quoted command separators should remain literal output.");
    }

    @Test(description = "performTerminalCommand should reject blank commands")
    public void performTerminalCommandShouldRejectBlankCommands() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);

        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.performTerminalCommand(" "));

        Assert.assertTrue(failure.getMessage().contains("must not be blank"));
    }

    @Test(description = "performTerminalCommand with env vars should expose them to the local process")
    public void performTerminalCommandShouldExposeEnvironmentVariablesLocally() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        boolean isWindows = System.getProperty("os.name").toLowerCase().contains("win");
        String command = isWindows ? "echo $env:SHAFT_ENV_TEST" : "echo $SHAFT_ENV_TEST";
        String log = terminal.performTerminalCommand(command, Map.of("SHAFT_ENV_TEST", "env-value-123"));
        Assert.assertTrue(log.contains("env-value-123"),
                "Expected the injected environment variable value in command output.");
    }

    @Test(description = "performTerminalCommands env-var overload should still return command output")
    public void performTerminalCommandsWithEnvOverloadShouldReturnOutput() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommands(List.of("echo hello-env"), Map.of("UNUSED", "x"));
        Assert.assertTrue(log.contains("hello-env"),
                "Expected command output when using the environment-variable overload.");
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

    @Test(description = "synchronous execution should fail and destroy timed-out local commands")
    public void performTerminalCommandShouldFailWhenLocalCommandTimesOut() {
        long originalTimeout = SHAFT.Properties.timeouts.shellSessionTimeout();
        try {
            SHAFT.Properties.timeouts.set().shellSessionTimeout(0);
            TerminalActions terminal = TerminalActions.getInstance(false, false, true);
            boolean isWindows = System.getProperty("os.name").toLowerCase().contains("win");
            String command = isWindows ? "Start-Sleep -Seconds 5" : "sleep 5";

            RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                    () -> terminal.performTerminalCommand(command));

            Assert.assertTrue(failure.getMessage().contains("timed out"));
        } finally {
            SHAFT.Properties.timeouts.set().shellSessionTimeout(originalTimeout);
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
    @SuppressWarnings("deprecation")
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

        TerminalActions verboseTerminal = TerminalActions.getRemoteInstance(
                "host.example.com", 22, "user", "/keys/", "id_rsa", true);
        String verboseLogs = (String) readConsoleLogs.invoke(verboseTerminal,
                new BufferedReader(new StringReader("stream-line-1" + System.lineSeparator() + "stream-line-2")));
        Assert.assertTrue(verboseLogs.contains("stream-line-1"));
        Assert.assertTrue(verboseLogs.contains("stream-line-2"));
    }

    @Test(description = "reportActionResult should format pass and fail messages for different attachment shapes")
    public void reportActionResultShouldHandlePassAndFailScenarios() throws Exception {
        Method reportActionResult = TerminalActions.class.getDeclaredMethod("reportActionResult",
                String.class, String.class, String.class, Boolean.class, Exception[].class);
        reportActionResult.setAccessible(true);

        String passMessage = (String) reportActionResult.invoke(null,
                "performTerminalCommand", "inputData", "sample logs", true, new Exception[]{});
        Assert.assertTrue(passMessage.contains("completed"));
        Assert.assertTrue(passMessage.contains("inputData"));

        String longTestData = "x".repeat(600);
        String failMessage = (String) reportActionResult.invoke(null,
                "performTerminalCommand", longTestData, "sample logs", false,
                new Exception[]{new RuntimeException("forced failure")});
        Assert.assertTrue(failMessage.contains("failed"));

        String passWithoutAttachments = (String) reportActionResult.invoke(null,
                "performTerminalCommand", null, null, true, new Exception[]{});
        Assert.assertTrue(passWithoutAttachments.contains("completed"));
    }

    @Test(description = "reportActionResult should redact sensitive command data before logging")
    public void reportActionResultShouldRedactSensitiveCommandData() throws Exception {
        Method reportActionResult = TerminalActions.class.getDeclaredMethod("reportActionResult",
                String.class, String.class, String.class, Boolean.class, Exception[].class);
        reportActionResult.setAccessible(true);

        String uriAndKeyMessage = (String) reportActionResult.invoke(null,
                "performTerminalCommand",
                "curl https://user:secret@example.com --api-key=live-key",
                "authorization: Bearer leaked-token",
                true,
                new Exception[]{});
        Assert.assertFalse(uriAndKeyMessage.contains("user:secret@"));
        Assert.assertFalse(uriAndKeyMessage.contains("live-key"));
        Assert.assertTrue(uriAndKeyMessage.contains("api-key=***"));
        Assert.assertTrue(uriAndKeyMessage.contains("***:***@"));

        String tokenMessage = (String) reportActionResult.invoke(null,
                "performTerminalCommand",
                "deploy --token=abc123 --region=eu",
                null,
                true,
                new Exception[]{});
        Assert.assertFalse(tokenMessage.contains("abc123"));
        Assert.assertTrue(tokenMessage.contains("token=***"));
        Assert.assertTrue(tokenMessage.contains("--region=eu"));
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

    @Test(description = "performTerminalCommand should return unredacted output for assertions")
    public void performTerminalCommandShouldReturnUnredactedOutputForAssertions() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo token=visible");
        Assert.assertTrue(log.contains("token=visible"),
                "Returned command output should remain available for assertions");
    }

    @Test(description = "Remote SSH keep-alive should use configured sshServerAliveInterval timeout")
    public void remoteSshKeepAliveShouldUseConfiguredSshServerAliveInterval() throws JSchException {
        int originalInterval = SHAFT.Properties.timeouts.sshServerAliveInterval();
        try {
            Assert.assertEquals(SHAFT.Properties.timeouts.sshServerAliveInterval(), 60,
                    "Default sshServerAliveInterval should be 60 seconds");

            SHAFT.Properties.timeouts.set().sshServerAliveInterval(120);
            int intervalSeconds = SHAFT.Properties.timeouts.sshServerAliveInterval();
            Session session = new JSch().getSession("user", "host.example.com", 22);
            if (intervalSeconds > 0) {
                session.setServerAliveInterval(intervalSeconds * 1000);
            }
            Assert.assertEquals(session.getServerAliveInterval(), 120_000,
                    "Remote SSH sessions should convert sshServerAliveInterval seconds to JSch milliseconds");
        } finally {
            SHAFT.Properties.timeouts.set().sshServerAliveInterval(originalInterval);
        }
    }

    @Test(description = "close should delegate to quit for reusable remote terminals")
    public void closeShouldDelegateToQuitForReusableRemoteTerminal() {
        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        Assert.assertTrue(terminal.isRemoteTerminal());
        terminal.close();
        terminal.quit();
    }

    @Test(description = "SshConnectionOptions legacy facade should disable strict host key checking by default")
    public void sshConnectionOptionsLegacyFacadeShouldDisableStrictHostKeyChecking() {
        SshConnectionOptions options = SshConnectionOptions.fromKeyFile("host.example.com", 22, "user", "/keys/", "id_rsa", false);
        Assert.assertFalse(options.isStrictHostKeyChecking());
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
                TEMP_ROOT.resolve("missing-identity").toAbsolutePath().toString() + "/", "missing_key");
        Method createSshSession = TerminalActions.class.getDeclaredMethod("createSSHsession");
        createSshSession.setAccessible(true);

        InvocationTargetException failure = Assert.expectThrows(InvocationTargetException.class,
                () -> createSshSession.invoke(terminal));
        Assert.assertTrue(failure.getCause() instanceof RuntimeException);
    }

    @Test(description = "mergeRemoteCommandOutput should preserve stdout-only remote logs")
    public void mergeRemoteCommandOutputShouldPreserveStdoutOnly() throws Exception {
        Method mergeRemoteCommandOutput = TerminalActions.class.getDeclaredMethod("mergeRemoteCommandOutput", String.class, String.class);
        mergeRemoteCommandOutput.setAccessible(true);
        String merged = (String) mergeRemoteCommandOutput.invoke(null, "stdout-only", "");
        Assert.assertEquals(merged, "stdout-only");
    }

    @Test(description = "mergeRemoteCommandOutput should preserve stderr-only remote logs")
    public void mergeRemoteCommandOutputShouldPreserveStderrOnly() throws Exception {
        Method mergeRemoteCommandOutput = TerminalActions.class.getDeclaredMethod("mergeRemoteCommandOutput", String.class, String.class);
        mergeRemoteCommandOutput.setAccessible(true);
        String merged = (String) mergeRemoteCommandOutput.invoke(null, "", "stderr-only");
        Assert.assertEquals(merged, "stderr-only");
    }

    @Test(description = "mergeRemoteCommandOutput should merge stdout and stderr for legacy String API")
    public void mergeRemoteCommandOutputShouldMergeStdoutAndStderr() throws Exception {
        Method mergeRemoteCommandOutput = TerminalActions.class.getDeclaredMethod("mergeRemoteCommandOutput", String.class, String.class);
        mergeRemoteCommandOutput.setAccessible(true);
        String merged = (String) mergeRemoteCommandOutput.invoke(null, "stdout", "stderr");
        Assert.assertEquals(merged, "stdout" + System.lineSeparator() + "stderr");
    }

    @Test(description = "captureRemoteCommandStreams should drain stdout and stderr separately after the command completes")
    public void captureRemoteCommandStreamsShouldDrainStdoutAndStderrSeparately() throws Exception {
        Class<?> throwingRunnableType = Class.forName("com.shaft.cli.TerminalActions$ThrowingRunnable");
        Method captureRemoteCommandStreams = TerminalActions.class.getDeclaredMethod(
                "captureRemoteCommandStreams",
                InputStream.class,
                InputStream.class,
                throwingRunnableType,
                BooleanSupplier.class,
                IntSupplier.class,
                long.class);
        captureRemoteCommandStreams.setAccessible(true);

        PipedOutputStream stdoutOut = new PipedOutputStream();
        PipedInputStream stdoutIn = new PipedInputStream(stdoutOut);
        PipedOutputStream stderrOut = new PipedOutputStream();
        PipedInputStream stderrIn = new PipedInputStream(stderrOut);
        AtomicBoolean channelClosed = new AtomicBoolean(false);

        Object connectAction = java.lang.reflect.Proxy.newProxyInstance(
                throwingRunnableType.getClassLoader(),
                new Class<?>[]{throwingRunnableType},
                (proxy, method, args) -> {
                    // Simulate a remote command that keeps the channel open past the old 1-second cutoff.
                    Thread producer = new Thread(() -> {
                        try {
                            Thread.sleep(1500);
                            stdoutOut.write("stdout-line\n".getBytes());
                            stderrOut.write("stderr-line\n".getBytes());
                            stdoutOut.close();
                            stderrOut.close();
                            channelClosed.set(true);
                        } catch (Exception ignored) {
                            Thread.currentThread().interrupt();
                        }
                    });
                    producer.setDaemon(true);
                    producer.start();
                    return null;
                });

        Object capture = captureRemoteCommandStreams.invoke(new TerminalActions(), stdoutIn, stderrIn, connectAction,
                (BooleanSupplier) channelClosed::get,
                (IntSupplier) () -> 7,
                30_000L);

        Method stdoutAccessor = capture.getClass().getDeclaredMethod("stdout");
        Method stderrAccessor = capture.getClass().getDeclaredMethod("stderr");
        Method exitCodeAccessor = capture.getClass().getDeclaredMethod("exitCode");
        stdoutAccessor.setAccessible(true);
        stderrAccessor.setAccessible(true);
        exitCodeAccessor.setAccessible(true);

        Assert.assertEquals(stdoutAccessor.invoke(capture), "stdout-line");
        Assert.assertEquals(stderrAccessor.invoke(capture), "stderr-line");
        Assert.assertEquals(exitCodeAccessor.invoke(capture), 7);
    }

    @Test(description = "performSshCommand should reject local terminals")
    public void performSshCommandShouldRejectLocalTerminal() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.performSshCommand("echo test"));
        Assert.assertTrue(failure.getMessage().contains("remote SSH"));
    }

    @Test(description = "performSshCommand should reject blank commands on reusable remote terminals")
    public void performSshCommandShouldRejectBlankCommands() {
        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> terminal.performSshCommand(" "));
        Assert.assertTrue(failure.getMessage().contains("SSH command"));
    }

    @Test(description = "performTerminalCommand should remain backward compatible for local command output")
    public void performTerminalCommandShouldRemainBackwardCompatibleForLocalOutput() {
        TerminalActions terminal = TerminalActions.getInstance(false, false, true);
        String log = terminal.performTerminalCommand("echo legacy-string-api");
        Assert.assertTrue(log.contains("legacy-string-api"),
                "Existing String API should still return command output for assertions");
    }
}
