package com.shaft.commandline.command;

import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SessionCommandTest {

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
    void helpListsStartStatusAndStop() {
        StringWriter out = new StringWriter();
        CommandLine cli = new CommandLine(new SessionCommand());
        cli.setOut(new PrintWriter(out, true));
        assertEquals(0, cli.execute("--help"));
        String help = out.toString().toLowerCase();
        assertTrue(help.contains("start"), help);
        assertTrue(help.contains("status"), help);
        assertTrue(help.contains("stop"), help);
    }
}
