package com.shaft.intellij.ui.firstrun;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.settings.ShaftSettingsState;

import java.nio.file.Files;
import java.nio.file.Path;

/** Detected project facts for wizard step 1. */
public final class ProjectFacts {
    private ProjectFacts() {
    }

    public static String describe(Project project, ShaftSettingsState.Settings settings) {
        String base = project == null ? "" : project.getBasePath();
        boolean blank = base == null || base.isBlank();
        boolean shaft = !blank && ShaftProjectDetector.isShaftProject(project);
        String buildKey = blank ? "wizard.facts.build.none" : buildKey(Path.of(base));
        boolean mcp = mcpPresent(settings, blank ? null : Path.of(base));
        String jdk = System.getProperty("java.version", "");
        String os = System.getProperty("os.name", "");
        String text = String.join("\n",
                WizardMessages.get(shaft ? "wizard.facts.shaft.yes" : "wizard.facts.shaft.no"),
                WizardMessages.get(buildKey),
                WizardMessages.format("wizard.facts.jdk", jdk),
                WizardMessages.format("wizard.facts.os", os),
                WizardMessages.get(mcp ? "wizard.facts.mcp.yes" : "wizard.facts.mcp.no"));
        return "<html>" + text.replace("\n", "<br>") + "</html>";
    }

    private static String buildKey(Path root) {
        boolean maven = Files.isRegularFile(root.resolve("pom.xml"));
        boolean gradle = Files.isRegularFile(root.resolve("build.gradle"))
                || Files.isRegularFile(root.resolve("build.gradle.kts"))
                || Files.isRegularFile(root.resolve("gradlew"))
                || Files.isRegularFile(root.resolve("gradlew.bat"));
        if (maven && gradle) {
            return "wizard.facts.build.both";
        }
        if (maven) {
            return "wizard.facts.build.maven";
        }
        if (gradle) {
            return "wizard.facts.build.gradle";
        }
        return "wizard.facts.build.none";
    }

    private static boolean mcpPresent(ShaftSettingsState.Settings settings, Path root) {
        if (settings != null && settings.mcpCommand != null && !settings.mcpCommand.isBlank()) {
            return true;
        }
        if (root == null) {
            return false;
        }
        return Files.isRegularFile(root.resolve("mcp.json")) || Files.isRegularFile(root.resolve(".mcp.json"));
    }
}
