package testPackage.unitTests;

import com.shaft.cli.TerminalActions;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link TerminalActions}.
 * Validates constructors, factory methods, and terminal state checks
 * without requiring external services (SSH, Docker).
 */
public class TerminalActionsUnitTest {

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
}
