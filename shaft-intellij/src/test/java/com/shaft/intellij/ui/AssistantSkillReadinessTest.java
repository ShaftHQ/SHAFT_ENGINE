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
    void codexRequiresTheShaftDeveloperEntrypoint(@TempDir Path projectRoot) throws Exception {
        AssistantSkillReadiness.Result missing = AssistantSkillReadiness.inspect(projectRoot.toString(), "CODEX");

        Path skills = projectRoot.resolve(".agents/skills");
        Files.createDirectories(skills.resolve("shaft-developer"));
        Files.writeString(skills.resolve("shaft-developer/SKILL.md"), "name: shaft-developer");
        AssistantSkillReadiness.Result ready = AssistantSkillReadiness.inspect(projectRoot.toString(), "CODEX");

        assertAll(
                () -> assertEquals(java.util.List.of("shaft-developer"), missing.missingSkills()),
                () -> assertFalse(missing.ready()),
                () -> assertTrue(ready.ready()),
                () -> assertEquals("codex", AssistantSkillReadiness.repairLoop("CODEX")),
                () -> assertEquals("claude", AssistantSkillReadiness.repairLoop("CLAUDE")),
                () -> assertEquals("copilot", AssistantSkillReadiness.repairLoop("COPILOT")),
                () -> assertEquals("gemini", AssistantSkillReadiness.repairLoop("GEMINI")));
    }

    @Test
    void copilotReadinessUsesTheCanonicalAgentsSkillRoot(@TempDir Path projectRoot) throws Exception {
        Path skills = projectRoot.resolve(".agents/skills/shaft-developer");
        Files.createDirectories(skills);
        Files.writeString(skills.resolve("SKILL.md"), "name: shaft-developer");

        assertTrue(AssistantSkillReadiness.inspect(projectRoot.toString(), "COPILOT").ready());
        assertFalse(AssistantSkillReadiness.inspect(projectRoot.toString(), "OPENCODE").ready());
    }
}
