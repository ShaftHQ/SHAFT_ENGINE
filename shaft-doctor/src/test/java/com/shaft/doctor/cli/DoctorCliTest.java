package com.shaft.doctor.cli;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Doctor command parsing and the branches that stop before Git or a provider call.
 */
class DoctorCliTest {
    @TempDir
    Path temp;

    @Test
    void rejectsMissingUnknownAndIncompleteCommands() {
        assertEquals(1, run(null));
        assertEquals(1, run(new String[] {}));
        assertTrue(error(new String[] {"inspect"}).contains("Usage:"));
        assertTrue(error(new String[] {"analyze", "input"}).contains("Unexpected Doctor argument."));
        assertTrue(error(new String[] {"analyze", "--input"}).contains("requires a value"));
        assertTrue(error(new String[] {"analyze", "--allowed-root", temp.toString()}).contains("--input is required"));
        assertTrue(error(new String[] {
                "analyze", "--input", temp.toString(), "--ai-cache"
        }).contains("--ai-cache requires --ai"));
        assertTrue(error(new String[] {
                "analyze", "--input", temp.toString(), "--allowed-root", temp.toString(), "--minimum-results", "0"
        }).contains("positive integer"));
        assertTrue(error(new String[] {
                "analyze", "--input", temp.toString(), "--allowed-root", temp.toString(), "--max-item-bytes", "nope"
        }).contains("positive integer"));
    }

    @Test
    void analyzeWithAiReportsConsentFallbackWithoutCallingAProvider() throws IOException {
        Path input = Files.createDirectories(temp.resolve("input"));
        Files.writeString(input.resolve("cli-result.json"), """
                {
                  "uuid": "cli-ai",
                  "historyId": "cli-ai",
                  "name": "test-cli-ai",
                  "fullName": "example.Test.test-cli-ai",
                  "status": "failed",
                  "start": 1,
                  "stop": 2,
                  "statusDetails": {"message": "TimeoutException: condition failed to be met", "trace": "trace"},
                  "labels": [
                    {"name": "testClass", "value": "example.Test"},
                    {"name": "testMethod", "value": "test-cli-ai"}
                  ]
                }
                """, StandardCharsets.UTF_8);
        Path output = temp.resolve("out");
        String previousCategories = SHAFT.Properties.pilot.allowedEvidenceCategories();
        boolean local = SHAFT.Properties.pilot.localConsent();
        boolean onPrem = SHAFT.Properties.pilot.onPremConsent();
        boolean remote = SHAFT.Properties.pilot.remoteConsent();
        try {
            SHAFT.Properties.pilot.set()
                    .allowedEvidenceCategories("")
                    .localConsent(false)
                    .onPremConsent(false)
                    .remoteConsent(false);
            StringWriter cliOutput = new StringWriter();
            StringWriter cliError = new StringWriter();
            int exit = DoctorCli.run(new String[] {
                    "ANALYZE",
                    "--input", input.toString(),
                    "--allowed-root", temp.toString(),
                    "--output-dir", output.toString(),
                    "--include-screenshots",
                    "--include-page-snapshots",
                    "--minimum-results", "1",
                    "--max-item-bytes", "4096",
                    "--max-bundle-bytes", "1000000",
                    "--ai",
                    "--ai-cache"
            }, new PrintWriter(cliOutput, true), new PrintWriter(cliError, true));

            assertEquals(0, exit, cliError.toString());
            assertTrue(cliOutput.toString().contains("\"primaryCause\":\"TIMING_SYNCHRONIZATION\""), cliOutput.toString());
            assertTrue(cliOutput.toString().contains("\"advisoryStatus\":\"FALLBACK\""), cliOutput.toString());
            assertTrue(cliOutput.toString().contains("\"providerStatus\":\"CONSENT_REQUIRED\""), cliOutput.toString());
            assertTrue(cliOutput.toString().contains("consent"), cliOutput.toString());
            assertTrue(Files.isRegularFile(output.resolve("doctor-evidence.json")));
        } finally {
            SHAFT.Properties.pilot.set()
                    .allowedEvidenceCategories(previousCategories)
                    .localConsent(local)
                    .onPremConsent(onPrem)
                    .remoteConsent(remote);
        }
    }

    @Test
    void proposeFixStopsOnBadInputBeforeCreatingAWorktree() throws IOException {
        assertTrue(error(new String[] {"propose-fix"}).contains("--repair-input is required"));
        Path missing = temp.resolve("missing-repair.json");
        assertTrue(error(new String[] {"propose-fix", "--repair-input", missing.toString()})
                .contains("local JSON input could not be read"));
        Path invalid = temp.resolve("invalid.json");
        Files.writeString(invalid, "{", StandardCharsets.UTF_8);
        assertTrue(error(new String[] {"propose-fix", "--repair-input", invalid.toString()})
                .contains("SHAFT Doctor command failed"));

        Path repair = temp.resolve("repair.json");
        Files.writeString(repair, """
                {"patches":[],"validationCommands":[]}
                """, StandardCharsets.UTF_8);
        String failure = error(new String[] {
                "propose-fix",
                "--repository", temp.toString(),
                "--base-sha", "0123456789abcdef0123456789abcdef01234567",
                "--diagnosis", temp.resolve("diagnosis.json").toString(),
                "--issue", "coverage",
                "--allowed-path", "src/Main.java",
                "--evidence-bundle", temp.resolve("bundle.json").toString(),
                "--approve-network-validation",
                "--output-dir", temp.resolve("repairs").toString(),
                "--max-patch-bytes", "128",
                "--command-timeout-seconds", "2",
                "--repair-input", repair.toString()
        });
        assertTrue(failure.contains("structured repair patch"));
        assertFalse(Files.exists(temp.resolve("repairs")));
        assertTrue(error(new String[] {
                "propose-fix",
                "--repository", temp.toString(),
                "--base-sha", "0123456789abcdef0123456789abcdef01234567",
                "--diagnosis", temp.resolve("diagnosis.json").toString(),
                "--issue", "coverage",
                "--allowed-path", "src/Main.java",
                "--repair-input", repair.toString(),
                "--max-patch-bytes", "0"
        }).contains("positive integer"));
    }

    @Test
    void proposeFixAiReturnsTheConsentFallbackAndDoesNotPublishWithoutApproval() throws IOException {
        Path diagnosis = temp.resolve("diagnosis.json");
        Files.writeString(diagnosis, """
                {
                  "schemaVersion": "1.1",
                  "primaryCause": "UNKNOWN",
                  "confidence": "LOW",
                  "summary": "synthetic",
                  "rationale": "unit"
                }
                """, StandardCharsets.UTF_8);
        Path repair = temp.resolve("repair.json");
        Files.writeString(repair, """
                {"patches":[],"validationCommands":[]}
                """, StandardCharsets.UTF_8);
        String previousCategories = SHAFT.Properties.pilot.allowedEvidenceCategories();
        try {
            SHAFT.Properties.pilot.set().allowedEvidenceCategories("").localConsent(false)
                    .onPremConsent(false).remoteConsent(false);
            StringWriter cliOutput = new StringWriter();
            StringWriter cliError = new StringWriter();
            int exit = DoctorCli.run(new String[] {
                    "propose-fix",
                    "--repository", temp.toString(),
                    "--base-sha", "0123456789abcdef0123456789abcdef01234567",
                    "--diagnosis", diagnosis.toString(),
                    "--issue", "coverage",
                    "--repair-input", repair.toString(),
                    "--ai"
            }, new PrintWriter(cliOutput, true), new PrintWriter(cliError, true));
            assertEquals(0, exit, cliError.toString());
            assertTrue(cliOutput.toString().contains("CONSENT_REQUIRED"));
            assertTrue(cliOutput.toString().contains("consent"));
        } finally {
            SHAFT.Properties.pilot.set().allowedEvidenceCategories(previousCategories);
        }

        String published = error(new String[] {
                "publish-draft-pr",
                "--manifest", temp.resolve("missing-manifest.json").toString(),
                "--approval-token", "token",
                "--title", "draft"
        });
        assertTrue(published.contains("Explicit publication approval is required."));
        assertTrue(error(new String[] {"publish-draft-pr", "--approval-token", "token"})
                .contains("--manifest is required"));
    }

    private static int run(String[] args) {
        return DoctorCli.run(args, new PrintWriter(new StringWriter(), true), new PrintWriter(new StringWriter(), true));
    }

    private static String error(String[] args) {
        StringWriter error = new StringWriter();
        int exit = DoctorCli.run(args, new PrintWriter(new StringWriter(), true), new PrintWriter(error, true));
        assertEquals(1, exit, error.toString());
        return error.toString();
    }
}
