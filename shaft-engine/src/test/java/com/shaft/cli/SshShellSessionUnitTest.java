package com.shaft.cli;

import com.jcraft.jsch.ChannelShell;
import com.shaft.driver.SHAFT;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

public class SshShellSessionUnitTest {
    @Test(description = "readUntil should return output through the first pattern match")
    public void readUntilShouldReturnMatchedOutput() throws Exception {
        PipedInputStream shellInput = new PipedInputStream();
        PipedOutputStream shellOutput = new PipedOutputStream(shellInput);
        PipedInputStream commandInput = new PipedInputStream();
        PipedOutputStream commandOutput = new PipedOutputStream(commandInput);

        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        SshShellSession shellSession = new SshShellSession(terminal, commandInput, shellOutput,
                SshShellOptions.builder().defaultTimeout(Duration.ofSeconds(2)).build());

        Thread producer = new Thread(() -> {
            try {
                TimeUnit.MILLISECONDS.sleep(100);
                commandOutput.write("line-one\nready-prompt ".getBytes(StandardCharsets.UTF_8));
                commandOutput.flush();
            } catch (Exception ignored) {
                Thread.currentThread().interrupt();
            }
        });
        producer.setDaemon(true);
        producer.start();

        String output = shellSession.readUntil(Pattern.compile("ready-prompt "));
        Assert.assertEquals(output, "line-one\nready-prompt ");
        shellSession.close();
        shellInput.close();
    }

    @Test(description = "readUntil should fail with a timeout message that includes the pattern")
    public void readUntilShouldFailWhenTimeoutExpires() throws Exception {
        PipedInputStream commandInput = new PipedInputStream();
        PipedOutputStream commandOutput = new PipedOutputStream(commandInput);
        PipedInputStream shellInput = new PipedInputStream();
        PipedOutputStream shellOutput = new PipedOutputStream(shellInput);

        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        SshShellSession shellSession = new SshShellSession(terminal, commandInput, shellOutput,
                SshShellOptions.builder().defaultTimeout(Duration.ofMillis(200)).build());

        Pattern pattern = Pattern.compile("never-appears");
        RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                () -> shellSession.readUntil(pattern, Duration.ofMillis(200)));
        Assert.assertTrue(failure.getMessage().contains("never-appears"));
        Assert.assertTrue(failure.getMessage().contains("200"));
        shellSession.close();
        shellOutput.close();
    }

    @Test(description = "close should be idempotent")
    public void closeShouldBeIdempotent() throws Exception {
        PipedInputStream commandInput = new PipedInputStream();
        PipedOutputStream commandOutput = new PipedOutputStream(commandInput);
        PipedInputStream shellInput = new PipedInputStream();
        PipedOutputStream shellOutput = new PipedOutputStream(shellInput);

        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        SshShellSession shellSession = new SshShellSession(terminal, commandInput, shellOutput, SshShellOptions.builder().build());

        shellSession.close();
        shellSession.close();
        shellOutput.close();
    }

    @Test(description = "sendLine should append a newline before writing")
    public void sendLineShouldAppendNewline() throws Exception {
        PipedInputStream shellInput = new PipedInputStream();
        PipedOutputStream shellOutput = new PipedOutputStream(shellInput);
        PipedInputStream commandInput = new PipedInputStream();

        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        SshShellSession shellSession = new SshShellSession(terminal, commandInput, shellOutput, SshShellOptions.builder().build());

        shellSession.sendLine("echo hello");
        byte[] payload = shellInput.readNBytes("echo hello".length() + 1);
        Assert.assertEquals(new String(payload, StandardCharsets.UTF_8), "echo hello\n");
        shellSession.close();
    }

    @Test(description = "close should disconnect a connected ChannelShell opened through openShell")
    public void closeShouldDisconnectConnectedChannelShell() throws Exception {
        ChannelShell shellChannel = Mockito.mock(ChannelShell.class);
        Mockito.when(shellChannel.isConnected()).thenReturn(true);
        Mockito.when(shellChannel.getInputStream()).thenReturn(new ByteArrayInputStream(new byte[0]));
        Mockito.when(shellChannel.getOutputStream()).thenReturn(new ByteArrayOutputStream());

        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        SshShellSession shellSession = new SshShellSession(terminal, shellChannel, SshShellOptions.builder().build());

        shellSession.close();
        Mockito.verify(shellChannel).disconnect();
    }

    @Test(description = "sendSecret should redact shell logs used for reporting")
    public void sendSecretShouldUseShellRedactionPath() throws Exception {
        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        Method redactShellLog = TerminalActions.class.getDeclaredMethod("redactShellLog", String.class);
        redactShellLog.setAccessible(true);

        String redacted = (String) redactShellLog.invoke(terminal, "password=super-secret");
        Assert.assertFalse(redacted.contains("super-secret"));
        Assert.assertTrue(redacted.contains("password=***"));
    }

    @Test(description = "quit should close tracked shell sessions before disconnecting the SSH session")
    public void quitShouldCloseTrackedShellSessions() throws Exception {
        PipedInputStream commandInput = new PipedInputStream();
        PipedOutputStream commandOutput = new PipedOutputStream(commandInput);
        PipedInputStream shellInput = new PipedInputStream();
        PipedOutputStream shellOutput = new PipedOutputStream(shellInput);

        TerminalActions terminal = SHAFT.CLI.remoteTerminal("host.example.com", 22, "user", "", "");
        SshShellSession shellSession = new SshShellSession(terminal, commandInput, shellOutput, SshShellOptions.builder().build());

        java.lang.reflect.Field activeShellSessionsField = TerminalActions.class.getDeclaredField("activeShellSessions");
        activeShellSessionsField.setAccessible(true);
        @SuppressWarnings("unchecked")
        java.util.List<SshShellSession> activeShellSessions =
                (java.util.List<SshShellSession>) activeShellSessionsField.get(terminal);
        activeShellSessions.add(shellSession);

        terminal.quit();
        shellOutput.close();
        Assert.assertTrue(activeShellSessions.isEmpty(), "quit() should clear tracked shell sessions");
    }
}
