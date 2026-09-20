package com.shaft.commandline.command;

import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportCommandTest {
    @Test
    void helpListsHistoryFlakeAndClustersActions() {
        StringWriter out = new StringWriter();
        CommandLine commandLine = new CommandLine(new ReportCommand());
        commandLine.setOut(new PrintWriter(out));
        int code = commandLine.execute("--help");
        assertEquals(0, code);
        String help = out.toString();
        assertTrue(help.contains("open"));
        assertTrue(help.contains("summary"));
        assertTrue(help.contains("history"));
        assertTrue(help.contains("flake"));
        assertTrue(help.contains("tags"));
        assertTrue(help.contains("clusters"));
        assertTrue(help.contains("mute"));
        assertTrue(help.contains("mutes"));
    }

    @Test
    void unknownActionIsRejected() {
        StringWriter err = new StringWriter();
        CommandLine commandLine = new CommandLine(new ReportCommand());
        commandLine.setErr(new PrintWriter(err));
        int code = commandLine.execute("nope");
        assertEquals(2, code);
        assertTrue(err.toString().toLowerCase().contains("unknown"));
    }
}
