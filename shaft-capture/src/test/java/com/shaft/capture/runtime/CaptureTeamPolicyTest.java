package com.shaft.capture.runtime;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureTeamPolicyTest {
    @Test
    void loadsCheckedInTeamPolicyAndReRootsOutputPaths(@TempDir Path workspace) throws Exception {
        Path policyFile = workspace.resolve(CaptureTeamPolicy.POLICY_PATH);
        Files.createDirectories(policyFile.getParent());
        Files.writeString(policyFile, """
                {"headless": true, "outputDirectory": "recordings/team", "browser": "Edge"}
                """);

        CaptureTeamPolicy policy = CaptureTeamPolicy.load(workspace);

        assertAll(
                () -> assertTrue(policy.present()),
                () -> assertEquals(Optional.of(true), policy.headless()),
                () -> assertEquals("Edge", policy.browser()),
                // An explicit request keeps its file name but moves into the team directory...
                () -> assertEquals("recordings/team/my-flow.json",
                        policy.applyOutputDirectory("somewhere/else/my-flow.json", "default.json")),
                // ...and a blank request gets the caller's default name in the team directory.
                () -> assertEquals("recordings/team/default.json",
                        policy.applyOutputDirectory("", "default.json")));
    }

    @Test
    void absentOrMalformedPolicyFallsBackToEmptyDefaults(@TempDir Path workspace) throws Exception {
        CaptureTeamPolicy absent = CaptureTeamPolicy.load(workspace);

        Path policyFile = workspace.resolve(CaptureTeamPolicy.POLICY_PATH);
        Files.createDirectories(policyFile.getParent());
        Files.writeString(policyFile, "{not json");
        CaptureTeamPolicy malformed = CaptureTeamPolicy.load(workspace);

        assertAll(
                () -> assertFalse(absent.present()),
                () -> assertFalse(malformed.present()),
                // No policy directory means the requested path passes through untouched.
                () -> assertEquals("recordings/x.json",
                        absent.applyOutputDirectory("recordings/x.json", "default.json")));
    }
}
