package com.shaft.commandline.command;

import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.testsupport.InProcessMcp;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
        assertTrue(help.contains("generate"));
        assertTrue(help.contains("--results"));
        assertTrue(help.contains("--single-file"));
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
    void generateWithoutResultsDoesNotCallMcp() {
        StringWriter err = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setErr(new PrintWriter(err, true))
                .execute("generate");
        assertEquals(2, exit);
        assertTrue(err.toString().contains("--results"));
        assertFalse(err.toString().contains("called report_"));
    }

    @Test
    void generateMissingDirectoryUsesAllureManagerAndSkipsMcp() {
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        Path missing = Path.of("target").resolve("missing-allure-results-7837");
        String previous = System.getProperty("allure.singleFile");
        try {
            int exit = new CommandLine(new ReportCommand(connector))
                    .setOut(new PrintWriter(out, true))
                    .setErr(new PrintWriter(err, true))
                    .execute("generate", "--results", missing.toString(), "--single-file");
            assertEquals(1, exit);
            assertTrue(err.toString().contains("does not exist"));
            assertFalse(out.toString().contains("called report_"));
            assertEquals("true", System.getProperty("allure.singleFile"));
        } finally {
            if (previous == null) {
                System.clearProperty("allure.singleFile");
            } else {
                System.setProperty("allure.singleFile", previous);
            }
        }
    }

    @Test
    void generateSkipsBlankPairsAndAcceptsResultsDirectoryKey() {
        StringWriter err = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setErr(new PrintWriter(err, true))
                .execute("generate", "not-a-pair", "other=value", "results=",
                        "resultsDirectory=target/blank-skipped-allure-results-7837");
        assertEquals(1, exit);
        assertTrue(err.toString().contains("does not exist"));
        assertFalse(err.toString().contains("called report_"));
    }

    @Test
    void generateAcceptsResultsKeyWithoutCallingMcp() {
        StringWriter err = new StringWriter();
        int exit = new CommandLine(new ReportCommand(connector))
                .setErr(new PrintWriter(err, true))
                .execute("generate", "results=target/also-missing-allure-results-7837");
        assertEquals(1, exit);
        assertTrue(err.toString().contains("does not exist"));
        assertFalse(err.toString().contains("called report_"));
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
