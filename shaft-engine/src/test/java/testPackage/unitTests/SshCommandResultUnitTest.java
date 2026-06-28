package testPackage.unitTests;

import com.shaft.cli.SshCommandResult;
import org.testng.Assert;
import org.testng.annotations.Test;

public class SshCommandResultUnitTest {
    @Test(description = "succeeded should be true only for exit code zero")
    public void succeededShouldReflectZeroExitCode() {
        Assert.assertTrue(new SshCommandResult("cmd", "ok", "", 0, 1L).succeeded());
        Assert.assertFalse(new SshCommandResult("cmd", "ok", "", 7, 1L).succeeded());
    }

    @Test(description = "combinedOutput should return stdout when stderr is empty")
    public void combinedOutputShouldReturnStdoutOnly() {
        SshCommandResult result = new SshCommandResult("cmd", "stdout-only", "", 0, 1L);
        Assert.assertEquals(result.combinedOutput(), "stdout-only");
    }

    @Test(description = "combinedOutput should return stderr when stdout is empty")
    public void combinedOutputShouldReturnStderrOnly() {
        SshCommandResult result = new SshCommandResult("cmd", "", "stderr-only", 0, 1L);
        Assert.assertEquals(result.combinedOutput(), "stderr-only");
    }

    @Test(description = "combinedOutput should merge stdout and stderr with a line separator")
    public void combinedOutputShouldMergeStdoutAndStderr() {
        SshCommandResult result = new SshCommandResult("cmd", "stdout", "stderr", 0, 1L);
        Assert.assertEquals(result.combinedOutput(), "stdout" + System.lineSeparator() + "stderr");
    }

    @Test(description = "accessors should expose exact command and stream values")
    public void accessorsShouldExposeCommandStreamsExitCodeAndDuration() {
        SshCommandResult result = new SshCommandResult("systemctl status", "active", "warn", 3, 42L);
        Assert.assertEquals(result.command(), "systemctl status");
        Assert.assertEquals(result.stdout(), "active");
        Assert.assertEquals(result.stderr(), "warn");
        Assert.assertEquals(result.exitCode(), 3);
        Assert.assertEquals(result.durationMillis(), 42L);
    }
}
