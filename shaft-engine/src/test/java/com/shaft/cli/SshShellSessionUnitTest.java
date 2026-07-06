package com.shaft.cli;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.regex.Pattern;

public class SshShellSessionUnitTest {
    @Test(description = "send should write exact shell input")
    public void sendShouldWriteExactInput() {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        SshShellSession session = new SshShellSession(new ByteArrayInputStream(new byte[0]), output, Duration.ofSeconds(1));

        session.send("printf ready");

        Assert.assertEquals(output.toString(StandardCharsets.UTF_8), "printf ready");
    }

    @Test(description = "sendLine should append a shell newline")
    public void sendLineShouldAppendShellNewline() {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        SshShellSession session = new SshShellSession(new ByteArrayInputStream(new byte[0]), output, Duration.ofSeconds(1));

        session.sendLine("yes");

        Assert.assertEquals(output.toString(StandardCharsets.UTF_8), "yes\n");
    }

    @Test(description = "sendSecret should write exact input without exposing it through the API")
    public void sendSecretShouldWriteExactInput() {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        SshShellSession session = new SshShellSession(new ByteArrayInputStream(new byte[0]), output, Duration.ofSeconds(1));

        session.sendSecret("sensitive-password\n");

        Assert.assertEquals(output.toString(StandardCharsets.UTF_8), "sensitive-password\n");
    }

    @Test(description = "readUntil should return output through the matching text")
    public void readUntilShouldReturnMatchingOutput() {
        ByteArrayInputStream input = new ByteArrayInputStream("ready\nPassword:".getBytes(StandardCharsets.UTF_8));
        SshShellSession session = new SshShellSession(input, new ByteArrayOutputStream(), Duration.ofSeconds(1));

        String output = session.readUntil(Pattern.compile("Password:"));

        Assert.assertEquals(output, "ready\nPassword:");
    }

    @Test(description = "readUntil should fail when the pattern is not seen before timeout")
    public void readUntilShouldFailOnTimeout() {
        SshShellSession session = new SshShellSession(new ByteArrayInputStream(new byte[0]),
                new ByteArrayOutputStream(), Duration.ofMillis(50));

        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> session.readUntil(Pattern.compile("never"), Duration.ofMillis(50)));

        Assert.assertTrue(failure.getMessage().contains("Timed out"));
        Assert.assertTrue(failure.getMessage().contains("never"));
    }

    @Test(description = "send should reject writes after close")
    public void sendShouldRejectWritesAfterClose() {
        SshShellSession session = new SshShellSession(new ByteArrayInputStream(new byte[0]),
                new ByteArrayOutputStream(), Duration.ofSeconds(1));

        session.close();

        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                () -> session.send("late"));
        Assert.assertTrue(failure.getMessage().contains("closed"));
    }
}
