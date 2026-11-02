package com.shaft.commandline.command;

import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.testsupport.InProcessMcp;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportCommandTest {

    private final McpConnector connector = InProcessMcp.connector();

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
        assertTrue(help.contains("heal"));
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

    @Test
    void openAliasDispatchesReportOpen() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("open");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("called report_open"));
    }

    @Test
    void historyAliasDispatchesReportHistory() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("history", "historyPath=target/history.jsonl");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("called report_history"));
    }

    @Test
    void flakeAliasDispatchesReportFlake() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("flake");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("called report_flake"));
    }

    @Test
    void tagsAliasDispatchesReportSmartTags() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("tags");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("called report_smart_tags"));
    }

    @Test
    void healAliasDispatchesReportHeal() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("heal");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("called report_heal"));
    }

    @Test
    void muteAliasDispatchesReportMuteWithAction() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("mute", "signature=sig-1");
        assertEquals(0, exit);
        String body = out.toString();
        assertTrue(body.contains("called report_mute"));
        assertTrue(body.contains("mute") || body.contains("action"));
    }
}
