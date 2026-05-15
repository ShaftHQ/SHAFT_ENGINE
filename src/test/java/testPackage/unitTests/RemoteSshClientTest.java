package testPackage.unitTests;

import com.shaft.cli.RemoteSshClient;
import com.shaft.cli.SshCommandResult;
import com.shaft.cli.SshSessionPolicy;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

/**
 * Unit tests for {@link RemoteSshClient} without a live SSH connection.
 */
public class RemoteSshClientTest {

    @Test(description = "performCommand must fail fast when SSH host is empty")
    public void performCommandThrowsWhenHostEmpty() {
        try (RemoteSshClient client = new RemoteSshClient("", 22, "user", "", "")) {
            Assert.expectThrows(IllegalStateException.class, () -> client.performCommand("echo test"));
        }
    }

    @Test(description = "performCommands must fail fast when SSH host is empty")
    public void performCommandsThrowsWhenHostEmpty() {
        try (RemoteSshClient client = new RemoteSshClient("", 2222, "u", "k/", "id_rsa")) {
            Assert.expectThrows(IllegalStateException.class, () -> client.performCommands(List.of("a", "b")));
        }
    }

    @Test(description = "Null host is normalized to empty and performCommand is rejected")
    public void nullHostNormalizedToEmpty() {
        try (RemoteSshClient client = new RemoteSshClient(null, 22, "user", "", "")) {
            Assert.expectThrows(IllegalStateException.class, () -> client.performCommand("x"));
        }
    }

    @Test(description = "Default constructor overload uses REUSE_SESSION")
    public void defaultConstructorUsesReuseSession() {
        try (RemoteSshClient client = new RemoteSshClient("host", 22, "user", "keys/", "id_rsa")) {
            Assert.assertEquals(client.getSessionPolicy(), SshSessionPolicy.REUSE_SESSION);
        }
    }

    @Test(description = "Explicit session policy is exposed via getter")
    public void explicitSessionPolicyIsReturned() {
        try (RemoteSshClient client = new RemoteSshClient(
                "host", 22, "user", "keys/", "id_rsa", SshSessionPolicy.NEW_SESSION_PER_COMMAND)) {
            Assert.assertEquals(client.getSessionPolicy(), SshSessionPolicy.NEW_SESSION_PER_COMMAND);
        }
    }

    @Test(description = "Null session policy falls back to REUSE_SESSION")
    public void nullSessionPolicyFallsBackToReuse() {
        try (RemoteSshClient client = new RemoteSshClient("host", 22, "user", "keys/", "id_rsa", null)) {
            Assert.assertEquals(client.getSessionPolicy(), SshSessionPolicy.REUSE_SESSION);
        }
    }

    @Test(description = "SSH + Docker constructor keeps default reuse policy")
    public void sshDockerConstructorDefaultPolicy() {
        try (RemoteSshClient client = new RemoteSshClient(
                "host", 22, "user", "k/", "key", "c1", "root")) {
            Assert.assertEquals(client.getSessionPolicy(), SshSessionPolicy.REUSE_SESSION);
        }
    }

    @Test(description = "close() without connect should not throw")
    public void closeWithoutConnectIsSafe() {
        try (RemoteSshClient client = new RemoteSshClient("localhost", 22, "u", "", "")) {
            client.close();
            Assert.assertFalse(client.isConnected());
        }
    }

    @Test(description = "isConnected is false before connect()")
    public void notConnectedBeforeConnect() {
        try (RemoteSshClient client = new RemoteSshClient("example.com", 22, "user", "", "")) {
            Assert.assertFalse(client.isConnected());
        }
    }

    @Test(description = "performCommandStreaming requires non-null consumer")
    public void performCommandStreamingRequiresConsumer() {
        try (RemoteSshClient client = new RemoteSshClient("host", 22, "user", "", "")) {
            Assert.expectThrows(NullPointerException.class, () -> client.performCommandStreaming("echo", null));
        }
    }

    @Test(description = "Builder produces client with expected session policy")
    public void builderSetsSessionPolicy() {
        try (RemoteSshClient c = RemoteSshClient.builder("h", 22, "u")
                .sessionPolicy(SshSessionPolicy.NEW_SESSION_PER_COMMAND)
                .build()) {
            Assert.assertEquals(c.getSessionPolicy(), SshSessionPolicy.NEW_SESSION_PER_COMMAND);
        }
    }

    @Test(description = "SHAFT.CLI.remoteSshBuilder delegates to RemoteSshClient.builder")
    public void shaftCliRemoteSshBuilder() {
        var b = com.shaft.driver.SHAFT.CLI.remoteSshBuilder("h", 22, "u").identity("k/", "id");
        try (RemoteSshClient c = b.build()) {
            Assert.assertEquals(c.getSessionPolicy(), SshSessionPolicy.REUSE_SESSION);
        }
    }

    @Test(description = "SshCommandResult.mergedOutput joins stdout and stderr")
    public void mergedOutputJoinsStreams() {
        var r = new SshCommandResult("line1", "err1", 0);
        String merged = r.mergedOutput();
        Assert.assertTrue(merged.contains("line1"));
        Assert.assertTrue(merged.contains("err1"));
    }

    @Test(description = "SshCommandResult.mergedOutput with only stdout")
    public void mergedOutputStdoutOnly() {
        Assert.assertEquals(new SshCommandResult("out", "", 1).mergedOutput(), "out");
    }

    @Test(description = "SshCommandResult.mergedOutput with only stderr")
    public void mergedOutputStderrOnly() {
        Assert.assertEquals(new SshCommandResult("", "e", -1).mergedOutput(), "e");
    }

    @Test(description = "SshCommandResult.mergedOutput empty")
    public void mergedOutputAllEmpty() {
        Assert.assertEquals(new SshCommandResult("", "", 0).mergedOutput(), "");
    }
}
