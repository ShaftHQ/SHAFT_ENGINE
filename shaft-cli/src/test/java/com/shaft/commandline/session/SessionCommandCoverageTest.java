package com.shaft.commandline.session;

import com.shaft.commandline.command.SessionCommand;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SessionCommandCoverageTest {

    @TempDir
    Path temp;

    @Test
    void rootCallPrintsUsageAndReturnsZero() {
        StringWriter out = new StringWriter();
        CommandLine cli = new CommandLine(new SessionCommand());
        cli.setOut(new PrintWriter(out, true));
        assertEquals(0, cli.execute());
        String help = out.toString().toLowerCase();
        assertTrue(help.contains("start") || help.contains("session"), out.toString());
    }

    @Test
    void startReturnsOneWhenDaemonCannotLaunch() {
        SessionManager manager = new SessionManager(locatorForMissingMain(), new SessionStore(temp));
        StringWriter err = new StringWriter();
        CommandLine cli = new CommandLine(new SessionCommand.Start(manager));
        cli.setErr(new PrintWriter(err, true));
        assertEquals(1, cli.execute());
        String message = err.toString().toLowerCase();
        assertTrue(message.contains("exited")
                || message.contains("fail")
                || message.contains("not found")
                || message.contains("does not exist")
                || message.contains("unable")
                || message.contains("daemon")
                || message.contains("shaft_mcp_jar"), err.toString());
    }

    @Test
    void statusReportsNoSessionWhenStoreIsEmpty() {
        SessionManager manager = new SessionManager(locatorForMissingMain(), new SessionStore(temp));
        StringWriter out = new StringWriter();
        CommandLine cli = new CommandLine(new SessionCommand.Status(manager));
        cli.setOut(new PrintWriter(out, true));
        assertEquals(0, cli.execute());
        assertTrue(out.toString().contains("No shaft-cli session recorded"), out.toString());
    }

    @Test
    void statusReportsStoppedSessionAsOne() throws Exception {
        SessionStore store = new SessionStore(temp);
        Process exited = new ProcessBuilder("true").start();
        assertTrue(exited.waitFor(5, TimeUnit.SECONDS));
        store.save(new SessionInfo(9, exited.pid(), "missing.jar", "2026-11-01T00:00:00Z"));
        SessionManager manager = new SessionManager(locatorForMissingMain(), store);
        StringWriter out = new StringWriter();
        CommandLine cli = new CommandLine(new SessionCommand.Status(manager));
        cli.setOut(new PrintWriter(out, true));
        assertEquals(1, cli.execute());
        assertTrue(out.toString().startsWith("stopped"), out.toString());
    }

    @Test
    void stopClearsADeadSessionAndIsIdempotent() throws Exception {
        SessionStore store = new SessionStore(temp);
        Process exited = new ProcessBuilder("true").start();
        assertTrue(exited.waitFor(5, TimeUnit.SECONDS));
        store.save(new SessionInfo(9, exited.pid(), "missing.jar", "2026-11-01T00:00:00Z"));
        SessionManager manager = new SessionManager(locatorForMissingMain(), store);

        StringWriter out = new StringWriter();
        CommandLine cli = new CommandLine(new SessionCommand.Stop(manager));
        cli.setOut(new PrintWriter(out, true));
        assertEquals(0, cli.execute());
        assertTrue(out.toString().contains("stopped"), out.toString());

        out = new StringWriter();
        cli = new CommandLine(new SessionCommand.Stop(manager));
        cli.setOut(new PrintWriter(out, true));
        assertEquals(0, cli.execute());
        assertTrue(out.toString().contains("No shaft-cli session to stop"), out.toString());
    }

    private static McpLauncherLocator locatorForMissingMain() {
        try {
            Path missing = Files.createTempDirectory("shaft-cli-session-cmd").resolve("missing.jar");
            return new McpLauncherLocator(
                    Map.of("SHAFT_MCP_JAR", missing.toString()),
                    missing.resolveSibling("no-versions"),
                    missing.resolveSibling("no-dev"));
        } catch (IOException exception) {
            throw new IllegalStateException(exception);
        }
    }
}
