package testPackage.unitTests;

import com.shaft.cli.internal.RemoteCommandBundler;
import com.shaft.cli.internal.ShellCommandNormalizer;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class RemoteCommandBundlerTest {

    @Test
    public void buildLongCommandJoinsWithAnd() {
        String cmd = RemoteCommandBundler.buildLongCommand(List.of("echo a", "echo b"), false, "", "", 60);
        Assert.assertEquals(cmd, "echo a && echo b");
    }

    @Test
    public void buildLongCommandWrapsDockerExec() {
        String cmd = RemoteCommandBundler.buildLongCommand(List.of("uname"), true, "c1", "root", 99);
        Assert.assertTrue(cmd.startsWith("docker exec -u root -i c1 timeout 99"));
        Assert.assertTrue(cmd.contains("uname"));
    }

    @Test
    public void expandSingleCommandChainingSplitsAnd() {
        var expanded = ShellCommandNormalizer.expandSingleCommandChaining(List.of("a && b"));
        Assert.assertEquals(expanded.size(), 2);
    }

    @Test
    public void expandSingleCommandChainingSplitsSemicolon() {
        var expanded = ShellCommandNormalizer.expandSingleCommandChaining(List.of("a ; b"));
        Assert.assertEquals(expanded.size(), 2);
    }
}
