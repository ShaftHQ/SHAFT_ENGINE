package com.shaft.intellij.ui;

import com.shaft.intellij.ui.firstrun.PrerequisitePlan;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.concurrent.TimeUnit;

/** Cached JDK / Maven / Git snapshot for the wizard prerequisites step. */
public final class HostToolVersion {
    private static final Pattern MAVEN_VERSION = Pattern.compile("Apache Maven\\s+(\\d+\\.\\d+(?:\\.\\d+)?)");
    private static volatile PrerequisitePlan.Snapshot cached;

    private HostToolVersion() {
    }

    public static PrerequisitePlan.Snapshot snapshot() {
        PrerequisitePlan.Snapshot current = cached;
        if (current != null) {
            return current;
        }
        synchronized (HostToolVersion.class) {
            if (cached == null) {
                cached = detect();
            }
            return cached;
        }
    }

    private static PrerequisitePlan.Snapshot detect() {
        boolean jdk = Runtime.version().feature() >= 17;
        return new PrerequisitePlan.Snapshot(jdk, mavenVersion(), commandSucceeds("git", "--version"));
    }

    private static String mavenVersion() {
        String output = commandOutput("mvn", "-version");
        Matcher matcher = MAVEN_VERSION.matcher(output);
        return matcher.find() ? matcher.group(1) : "";
    }

    private static boolean commandSucceeds(String... command) {
        return !commandOutput(command).isBlank();
    }

    private static String commandOutput(String... command) {
        Process process = null;
        try {
            process = new ProcessBuilder(command).redirectErrorStream(true).start();
            boolean finished = process.waitFor(8, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return "";
            }
            if (process.exitValue() != 0) {
                return "";
            }
            try (InputStream stream = process.getInputStream()) {
                return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            }
        } catch (Exception exception) {
            if (process != null) {
                process.destroyForcibly();
            }
            return "";
        }
    }
}
