package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the portable-setup prerequisite detection: every tool the setup flow depends on is either
 * detected on PATH or paired with a simple per-OS terminal command the user can paste, so a fresh
 * machine can be provisioned entirely from the setup screen.
 */
class SetupPrerequisitesTest {
    private static final Predicate<String> NOTHING_INSTALLED = command -> false;
    private static final Predicate<String> EVERYTHING_INSTALLED = command -> true;

    @Test
    void allToolsPresentYieldsNoMissingPrerequisitesAndNoNodeRow() {
        List<SetupPrerequisites.Prerequisite> detected =
                SetupPrerequisites.detect("CLAUDE", EVERYTHING_INSTALLED, true, false);

        assertAll(
                () -> assertTrue(detected.stream().allMatch(SetupPrerequisites.Prerequisite::present)),
                () -> assertTrue(detected.stream().noneMatch(item -> item.name().startsWith("Node.js")),
                        "Node.js is only surfaced when the agent CLI is missing and npm is needed"),
                () -> assertEquals(Set.of("Python 3", "Java (JDK)", "Maven", "Claude Code CLI"),
                        Set.copyOf(detected.stream().map(SetupPrerequisites.Prerequisite::name).toList())));
    }

    @Test
    void missingAgentCliWithoutNodeSurfacesNodeAsARequiredPrerequisite() {
        Predicate<String> onlyPythonAndJava = command -> Set.of("py", "python", "java", "mvn").contains(command);

        List<SetupPrerequisites.Prerequisite> detected =
                SetupPrerequisites.detect("CLAUDE", onlyPythonAndJava, true, false);

        SetupPrerequisites.Prerequisite node = detected.stream()
                .filter(item -> item.name().startsWith("Node.js"))
                .findFirst().orElseThrow();
        SetupPrerequisites.Prerequisite agent = detected.stream()
                .filter(item -> item.name().equals("Claude Code CLI"))
                .findFirst().orElseThrow();
        assertAll(
                () -> assertTrue(node.required()),
                () -> assertTrue(node.installCommand().contains("winget"), node.installCommand()),
                () -> assertFalse(agent.present()),
                () -> assertEquals("npm install -g @anthropic-ai/claude-code", agent.installCommand()));
    }

    @Test
    void geminiCloudFamilyNeedsNoAgentCli() {
        List<SetupPrerequisites.Prerequisite> detected =
                SetupPrerequisites.detect("GEMINI", NOTHING_INSTALLED, true, false);

        assertAll(
                () -> assertTrue(detected.stream().noneMatch(item -> item.name().contains("CLI")),
                        "Gemini runs through SHAFT MCP's cloud provider route: " + detected),
                () -> assertTrue(detected.stream().anyMatch(item -> item.name().equals("Python 3"))));
    }

    @Test
    void installCommandsArePerOperatingSystem() {
        List<SetupPrerequisites.Prerequisite> windows =
                SetupPrerequisites.detect("CODEX", NOTHING_INSTALLED, true, false);
        List<SetupPrerequisites.Prerequisite> mac =
                SetupPrerequisites.detect("CODEX", NOTHING_INSTALLED, false, true);
        List<SetupPrerequisites.Prerequisite> linux =
                SetupPrerequisites.detect("CODEX", NOTHING_INSTALLED, false, false);

        assertAll(
                () -> assertTrue(python(windows).installCommand().contains("winget install"),
                        python(windows).installCommand()),
                () -> assertTrue(python(mac).installCommand().contains("brew install"),
                        python(mac).installCommand()),
                () -> assertTrue(python(linux).installCommand().contains("apt-get install"),
                        python(linux).installCommand()));
    }

    @Test
    void javaAndMavenAreAdvisoryBecauseTheInstallerBootstrapsJavaItself() {
        List<SetupPrerequisites.Prerequisite> detected =
                SetupPrerequisites.detect("CODEX", NOTHING_INSTALLED, true, false);

        assertAll(
                () -> assertFalse(byName(detected, "Java (JDK)").required()),
                () -> assertFalse(byName(detected, "Maven").required()),
                () -> assertTrue(byName(detected, "Python 3").required()));
    }

    @Test
    void shaftEngineWarmupCommandPinsARealVersionInsteadOfTheLatestMetaVersion() {
        String command = SetupPrerequisites.shaftEngineWarmupCommand();

        assertAll(
                () -> assertTrue(
                        command.startsWith("mvn -B dependency:get \"-Dartifact=io.github.shafthq:SHAFT_ENGINE:"),
                        command),
                () -> assertFalse(command.contains(":LATEST"), command),
                // PowerShell splits an unquoted -Dartifact=io.github... token at the first dot, so
                // the -D argument must stay double-quoted end to end (issue #3426 A1).
                () -> assertTrue(command.endsWith("\""), command));
    }

    @Test
    void latestEngineVersionPrefersMavenCentralThenBundledPluginVersionThenReleaseMetaVersion() {
        assertAll(
                () -> assertEquals("10.3.20260710",
                        SetupPrerequisites.latestEngineVersion(() -> "10.3.20260710", () -> "10.3.20260101")),
                () -> assertEquals("10.3.20260101",
                        SetupPrerequisites.latestEngineVersion(() -> null, () -> "10.3.20260101")),
                () -> assertEquals("10.3.20260101",
                        SetupPrerequisites.latestEngineVersion(() -> "", () -> "10.3.20260101")),
                () -> assertEquals("RELEASE",
                        SetupPrerequisites.latestEngineVersion(() -> null, () -> "${pluginVersion}")),
                () -> assertEquals("RELEASE",
                        SetupPrerequisites.latestEngineVersion(() -> "not-a-version", () -> "dev")));
    }

    @Test
    void releaseVersionIsExtractedFromMavenRepositoryMetadata() {
        String metadata = """
                <metadata>
                  <groupId>io.github.shafthq</groupId>
                  <artifactId>SHAFT_ENGINE</artifactId>
                  <versioning>
                    <latest>10.3.20260710</latest>
                    <release>10.3.20260710</release>
                  </versioning>
                </metadata>
                """;

        assertAll(
                () -> assertEquals("10.3.20260710", SetupPrerequisites.releaseVersionFromMetadata(metadata)),
                () -> assertEquals("", SetupPrerequisites.releaseVersionFromMetadata("<metadata/>")),
                () -> assertEquals("", SetupPrerequisites.releaseVersionFromMetadata("")));
    }

    private static SetupPrerequisites.Prerequisite python(List<SetupPrerequisites.Prerequisite> detected) {
        return byName(detected, "Python 3");
    }

    private static SetupPrerequisites.Prerequisite byName(
            List<SetupPrerequisites.Prerequisite> detected, String name) {
        return detected.stream().filter(item -> item.name().equals(name)).findFirst().orElseThrow();
    }
}
