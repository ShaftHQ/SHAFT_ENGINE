package com.shaft.intellij.ui.firstrun;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/** JDK, Maven 3.9+, and Git presence for the prerequisites step. */
public final class PrerequisitePlan {
    /**
     * Host tool snapshot. {@code mavenVersion} is blank when Maven is missing.
     *
     * @param jdkPresent whether a JDK is available
     * @param mavenVersion detected Maven version, or blank
     * @param gitPresent whether Git is available
     */
    public record Snapshot(boolean jdkPresent, String mavenVersion, boolean gitPresent) {
        public boolean mavenMeets() {
            return mavenAtLeast(mavenVersion, 3, 9);
        }

        public boolean allPresent() {
            return jdkPresent && mavenMeets() && gitPresent;
        }

        public String readyLine() {
            return WizardMessages.format("wizard.ready", String.join(", ", readyNames()));
        }

        public List<String> readyNames() {
            List<String> names = new ArrayList<>();
            if (jdkPresent) {
                names.add(WizardMessages.get("wizard.tool.jdk"));
            }
            if (mavenMeets()) {
                names.add(WizardMessages.get("wizard.tool.maven"));
            }
            if (gitPresent) {
                names.add(WizardMessages.get("wizard.tool.git"));
            }
            return names;
        }

        public List<String> missingNames() {
            List<String> names = new ArrayList<>();
            if (!jdkPresent) {
                names.add(WizardMessages.get("wizard.tool.jdk"));
            }
            if (!mavenMeets()) {
                names.add(WizardMessages.get("wizard.tool.maven"));
            }
            if (!gitPresent) {
                names.add(WizardMessages.get("wizard.tool.git"));
            }
            return names;
        }
    }

    private PrerequisitePlan() {
    }

    static boolean mavenAtLeast(String version, int major, int minor) {
        int[] parts = versionParts(version);
        if (parts[0] != major) {
            return parts[0] > major;
        }
        return parts[1] >= minor;
    }

    static String installCommand(String toolName) {
        boolean windows = os("win");
        boolean mac = os("mac");
        if (WizardMessages.get("wizard.tool.jdk").equals(toolName)) {
            if (windows) {
                return "winget install -e --id EclipseAdoptium.Temurin.25.JDK";
            }
            return mac ? "brew install --cask temurin@25" : "sudo apt-get install -y openjdk-25-jdk";
        }
        if (WizardMessages.get("wizard.tool.maven").equals(toolName)) {
            if (windows) {
                return "winget install -e --id Apache.Maven";
            }
            return mac ? "brew install maven" : "sudo apt-get install -y maven";
        }
        if (windows) {
            return "winget install -e --id Git.Git";
        }
        return mac ? "brew install git" : "sudo apt-get install -y git";
    }

    private static int[] versionParts(String version) {
        int[] parts = new int[] {0, 0};
        if (version == null || version.isBlank()) {
            return parts;
        }
        String[] tokens = version.split("[^0-9]+");
        int index = 0;
        for (String token : tokens) {
            if (token.isEmpty()) {
                continue;
            }
            parts[index] = Integer.parseInt(token);
            index++;
            if (index == 2) {
                break;
            }
        }
        return parts;
    }

    private static boolean os(String token) {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains(token);
    }
}
