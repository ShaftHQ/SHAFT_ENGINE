package com.shaft.intellij.ui;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;

/** Verifies the project-local SHAFT skills that local agent runs depend on. */
final class AssistantSkillReadiness {
    private static final List<String> REQUIRED_SKILLS = List.of("shaft-developer");

    private AssistantSkillReadiness() {
    }

    static Result inspect(String projectRoot, String family) {
        if (projectRoot == null || projectRoot.isBlank()) {
            return new Result(List.of());
        }
        Path skillsRoot = Path.of(projectRoot).resolve(skillsDirectory(family));
        List<String> missing = REQUIRED_SKILLS.stream()
                .filter(skill -> !Files.isRegularFile(skillsRoot.resolve(skill).resolve("SKILL.md")))
                .toList();
        return new Result(missing);
    }

    static String repairLoop(String family) {
        return switch (normalize(family)) {
            case "CLAUDE", "CLAUDE_CODE" -> "claude";
            case "COPILOT", "COPILOT_CLI" -> "copilot";
            case "OPENCODE" -> "opencode";
            case "GEMINI" -> "gemini";
            case "CURSOR" -> "cursor";
            case "GROK" -> "grok";
            case "ANTIGRAVITY" -> "antigravity";
            case "VSCODE" -> "vscode";
            default -> "codex";
        };
    }

    private static Path skillsDirectory(String family) {
        return switch (normalize(family)) {
            case "CLAUDE", "CLAUDE_CODE" -> Path.of(".claude", "skills");
            case "OPENCODE" -> Path.of(".opencode", "skills");
            default -> Path.of(".agents", "skills");
        };
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
    }

    record Result(List<String> missingSkills) {
        boolean ready() {
            return missingSkills.isEmpty();
        }

        String status() {
            return ready() ? "Ready · SHAFT skills loaded" : "Missing · " + String.join(", ", missingSkills);
        }
    }
}
