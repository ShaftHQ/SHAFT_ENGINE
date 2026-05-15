package testPackage.unitTests;

import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for reusable remote SSH {@link TerminalActions} lifecycle (no live SSH).
 */
public class TerminalActionsReusableSshTest {

    @Test(description = "Non-reusable remote terminal rejects getJschSession")
    public void getJschSessionRequiresReusable() {
        var t = new TerminalActions("host.example", 22, "user", "keys/", "id_ed25519");
        Assert.assertFalse(t.isReusableRemoteSshSession());
        Assert.expectThrows(IllegalStateException.class, () -> t.getJschSession());
    }

    @Test(description = "Non-reusable remote terminal rejects getRemoteSshClient")
    public void getRemoteSshClientRequiresReusable() {
        var t = new TerminalActions("host.example", 22, "user", "keys/", "id_ed25519");
        Assert.expectThrows(IllegalStateException.class, () -> t.getRemoteSshClient());
    }

    @Test(description = "Reusable flag is exposed for remote constructors")
    public void reusableConstructorSetsFlag() {
        var t = new TerminalActions("h", 22, "u", "k/", "key", true);
        Assert.assertTrue(t.isReusableRemoteSshSession());
    }

    @Test(description = "Local terminal rejects getJschSession")
    public void localTerminalRejectsNativeSession() {
        var t = new TerminalActions();
        Assert.expectThrows(IllegalStateException.class, () -> t.getJschSession());
    }

    @Test(description = "SHAFT.CLI.Terminal.quit is safe on local driver")
    public void shaftCliTerminalQuitLocal() {
        var cli = new SHAFT.CLI.Terminal();
        cli.quit();
    }

    @Test(description = "SHAFT.CLI.Terminal forwards getJschSession policy to actions")
    public void shaftCliTerminalThrowsWhenNotReusableRemote() {
        var cli = new SHAFT.CLI.Terminal("h", 22, "u", "k/", "key", false);
        try {
            Assert.expectThrows(IllegalStateException.class, () -> cli.getJschSession());
        } finally {
            cli.quit();
        }
    }

    @Test(description = "Docker+SSH reusable constructor sets flag")
    public void dockerSshReusableFlag() {
        var t = new TerminalActions("h", 22, "u", "k/", "key", "c1", "root", true);
        Assert.assertTrue(t.isReusableRemoteSshSession());
    }
}
