package com.shaft.mcp;

import com.shaft.doctor.cli.DoctorCli;
import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Both branches of the stdio listener guard in {@link ShaftMcpApplication#main}.
 * Doctor returns 0, so main returns instead of calling {@code System.exit}.
 */
class ShaftMcpApplicationLogbackListenerTest {
    private static final String LISTENER = "logback.statusListenerClass";
    private static final String NOP = "ch.qos.logback.core.status.NopStatusListener";
    private static final String SENTINEL = "sentinel.NotNopStatusListener";

    @TempDir
    Path temp;

    @Test
    void mainSetsNopStatusListenerOnlyWhenUnset() throws IOException {
        String previousListener = System.getProperty(LISTENER);
        boolean localConsent = SHAFT.Properties.pilot.localConsent();
        boolean onPremConsent = SHAFT.Properties.pilot.onPremConsent();
        boolean remoteConsent = SHAFT.Properties.pilot.remoteConsent();
        String categories = SHAFT.Properties.pilot.allowedEvidenceCategories();
        try {
            SHAFT.Properties.pilot.set().allowedEvidenceCategories("").localConsent(false)
                    .onPremConsent(false).remoteConsent(false);
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
            String[] args = {
                    "doctor",
                    "propose-fix",
                    "--repository", temp.toString(),
                    "--base-sha", "0123456789abcdef0123456789abcdef01234567",
                    "--diagnosis", diagnosis.toString(),
                    "--issue", "coverage",
                    "--repair-input", repair.toString(),
                    "--ai"
            };
            StringWriter cliError = new StringWriter();
            int exit = DoctorCli.run(
                    Arrays.copyOfRange(args, 1, args.length),
                    new PrintWriter(new StringWriter(), true),
                    new PrintWriter(cliError, true));
            assertEquals(0, exit, cliError.toString());

            System.clearProperty(LISTENER);
            ShaftMcpApplication.main(args);
            assertEquals(NOP, System.getProperty(LISTENER));

            System.setProperty(LISTENER, SENTINEL);
            ShaftMcpApplication.main(args);
            assertEquals(SENTINEL, System.getProperty(LISTENER));
        } finally {
            if (previousListener == null) {
                System.clearProperty(LISTENER);
            } else {
                System.setProperty(LISTENER, previousListener);
            }
            SHAFT.Properties.pilot.set().allowedEvidenceCategories(categories)
                    .localConsent(localConsent).onPremConsent(onPremConsent).remoteConsent(remoteConsent);
        }
    }
}
