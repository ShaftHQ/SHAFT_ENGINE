package testPackage.unitTests;

import com.shaft.cli.internal.RemoteCommandBundler;
import com.shaft.cli.internal.ShellCommandNormalizer;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class RemoteCommandBundlerTest {

    @Test
    public void buildLongCommandJoinsWithAnd() {
        String cmd = RemoteCommandBundler.buildLongCommand(List.of("cd /tmp", "ls"), false, "", "", 30);
        Assert.assertEquals(cmd, "cd /tmp && ls");
    }

    @Test
    public void buildLongCommandWrapsDockerExec() {
        String cmd = RemoteCommandBundler.buildLongCommand(List.of("echo hello"), true, "c1", "u1", 45);
        Assert.assertEquals(cmd, "docker exec -u u1 -i c1 timeout 45 sh -c 'echo hello'");
    }

    @Test
    public void expandSingleCommandChainingSplitsAnd() {
        List<String> expanded = ShellCommandNormalizer.expandSingleCommandChaining(List.of("a && b"));
        Assert.assertEquals(expanded.size(), 2);
        Assert.assertEquals(expanded.get(0), "a");
        Assert.assertEquals(expanded.get(1), "b");
    }

    @Test
    public void expandSingleCommandChainingSplitsSemicolon() {
        List<String> expanded = ShellCommandNormalizer.expandSingleCommandChaining(List.of("x ; y"));
        Assert.assertEquals(expanded.size(), 2);
        Assert.assertEquals(expanded.get(0), "x");
        Assert.assertEquals(expanded.get(1), "y");
    }
}
