package com.shaft.intellij.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.function.Predicate;

/**
 * Detects the tools the SHAFT MCP setup flow depends on and produces the per-OS terminal commands
 * that install a missing one, so the setup screen can run fully portable on a fresh machine: every
 * prerequisite is either detected as present or paired with a simple command the user can paste
 * into their terminal. Java is bootstrapped automatically by the installer when absent, so it is
 * advisory rather than blocking; Python is required to run the installer script itself.
 */
final class SetupPrerequisites {
    /**
     * One detected prerequisite: display name, whether it was found on PATH, whether setup can
     * proceed without it, and the terminal command that installs it on this OS.
     */
    record Prerequisite(String name, boolean present, boolean required, String installCommand) {
    }

    private SetupPrerequisites() {
        throw new IllegalStateException("Utility class");
    }

    static List<Prerequisite> detect(String family) {
        return detect(family, AssistantLocalAgentRunner::isCommandAvailable, isWindows(), isMac());
    }

    /**
     * Package-private overload used by tests to stub PATH detection and the OS.
     */
    static List<Prerequisite> detect(String family, Predicate<String> commandAvailable, boolean windows, boolean mac) {
        List<Prerequisite> prerequisites = new ArrayList<>();
        boolean python = commandAvailable.test(windows ? "py" : "python3") || commandAvailable.test("python");
        prerequisites.add(new Prerequisite("Python 3", python, true, pythonInstallCommand(windows, mac)));
        prerequisites.add(new Prerequisite("Java (JDK)", commandAvailable.test("java"), false,
                javaInstallCommand(windows, mac)));
        prerequisites.add(new Prerequisite("Maven", commandAvailable.test("mvn"), false,
                mavenInstallCommand(windows, mac)));
        String agentExecutable = agentExecutableFor(family);
        if (agentExecutable != null) {
            boolean agentPresent = commandAvailable.test(agentExecutable);
            if (!agentPresent && !commandAvailable.test("node")) {
                prerequisites.add(new Prerequisite("Node.js (required to install " + agentDisplayNameFor(family) + ")",
                        false, true, nodeInstallCommand(windows, mac)));
            }
            prerequisites.add(new Prerequisite(agentDisplayNameFor(family), agentPresent, true,
                    agentInstallCommandFor(family)));
        }
        return List.copyOf(prerequisites);
    }

    static String agentExecutableFor(String family) {
        return switch (normalize(family)) {
            case "CLAUDE" -> "claude";
            case "COPILOT" -> "copilot";
            case "GEMINI" -> null;
            default -> "codex";
        };
    }

    static String agentDisplayNameFor(String family) {
        return switch (normalize(family)) {
            case "CLAUDE" -> "Claude Code CLI";
            case "COPILOT" -> "GitHub Copilot CLI";
            default -> "Codex CLI";
        };
    }

    static String agentInstallCommandFor(String family) {
        return switch (normalize(family)) {
            case "CLAUDE" -> "npm install -g @anthropic-ai/claude-code";
            case "COPILOT" -> "npm install -g @github/copilot";
            default -> "npm install -g @openai/codex";
        };
    }

    /**
     * Pre-downloads SHAFT Engine and its transitive dependencies into the local Maven repository so
     * every future SHAFT project on this machine builds without re-downloading them.
     */
    static String shaftEngineWarmupCommand() {
        return "mvn -B dependency:get -Dartifact=io.github.shafthq:SHAFT_ENGINE:LATEST";
    }

    private static String pythonInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id Python.Python.3.12";
        }
        return mac ? "brew install python@3.12" : "sudo apt-get install -y python3";
    }

    private static String javaInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id EclipseAdoptium.Temurin.25.JDK";
        }
        return mac ? "brew install --cask temurin@25" : "sudo apt-get install -y openjdk-25-jdk";
    }

    private static String mavenInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id Apache.Maven";
        }
        return mac ? "brew install maven" : "sudo apt-get install -y maven";
    }

    private static String nodeInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id OpenJS.NodeJS.LTS";
        }
        return mac ? "brew install node" : "sudo apt-get install -y nodejs npm";
    }

    private static String normalize(String value) {
        return (value == null || value.isBlank() ? "" : value.trim())
                .toUpperCase(Locale.ROOT)
                .replace('-', '_')
                .replace(' ', '_');
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }

    private static boolean isMac() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("mac");
    }
}
