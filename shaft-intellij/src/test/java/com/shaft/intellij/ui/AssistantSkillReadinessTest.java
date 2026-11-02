package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantSkillReadinessTest {

    @Test
    void codexRequiresBothProjectLocalShaftSkills(@TempDir Path projectRoot) throws Exception {
        AssistantSkillReadiness.Result missing = AssistantSkillReadiness.inspect(projectRoot.toString(), "CODEX");

        Path skills = projectRoot.resolve(".agents/skills");
        Files.createDirectories(skills.resolve("shaft-developer"));
        Files.writeString(skills.resolve("shaft-developer/SKILL.md"), "name: shaft-developer");
        AssistantSkillReadiness.Result partial = AssistantSkillReadiness.inspect(projectRoot.toString(), "CODEX");

        Files.createDirectories(skills.resolve("shaft-recording-codegen"));
        Files.writeString(skills.resolve("shaft-recording-codegen/SKILL.md"), "name: shaft-recording-codegen");
        AssistantSkillReadiness.Result ready = AssistantSkillReadiness.inspect(projectRoot.toString(), "CODEX");

        assertAll(
                () -> assertEquals(2, missing.missingSkills().size()),
                () -> assertEquals(java.util.List.of("shaft-recording-codegen"), partial.missingSkills()),
                () -> assertFalse(partial.ready()),
                () -> assertTrue(ready.ready()),
                () -> assertEquals("codex", AssistantSkillReadiness.repairLoop("CODEX")),
                () -> assertEquals("claude", AssistantSkillReadiness.repairLoop("CLAUDE")),
                () -> assertEquals("vscode", AssistantSkillReadiness.repairLoop("COPILOT")));
    }

    @Test
    void copilotReadinessMatchesTheVscodeRepairDestination(@TempDir Path projectRoot) throws Exception {
        Path skills = projectRoot.resolve(".github/instructions");
        for (String skill : java.util.List.of("shaft-developer", "shaft-recording-codegen")) {
            Files.createDirectories(skills.resolve(skill));
            Files.writeString(skills.resolve(skill).resolve("SKILL.md"), "name: " + skill);
        }

        assertTrue(AssistantSkillReadiness.inspect(projectRoot.toString(), "COPILOT").ready());
    }
}
