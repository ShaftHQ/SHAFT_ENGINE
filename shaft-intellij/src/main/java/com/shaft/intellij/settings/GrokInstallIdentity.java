package com.shaft.intellij.settings;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * The {@code grok} executable is shared by Grok CLI and Grok Build. Grok Build's
 * help text identifies itself as "Grok Build"; anything else stays Grok CLI.
 */
public final class GrokInstallIdentity {
    public static final String GROK_BUILD = "Grok Build";
    public static final String GROK_CLI = "Grok CLI";

    private static volatile Supplier<String> helpText = GrokInstallIdentity::readHelp;

    private GrokInstallIdentity() {
        throw new IllegalStateException("Utility class");
    }

    public static String labelFromHelp(String help) {
        return help != null && help.contains("Grok Build") ? GROK_BUILD : GROK_CLI;
    }

    public static String detectedLabel() {
        try {
            return labelFromHelp(helpText.get());
        } catch (RuntimeException ignored) {
            return GROK_CLI;
        }
    }

    /** Test seam. Pass null to restore the live {@code grok --help} probe. */
    public static void useHelp(Supplier<String> supplier) {
        helpText = supplier == null ? GrokInstallIdentity::readHelp : supplier;
    }

    static String readHelp() {
        try {
            Process process = new ProcessBuilder("grok", "--help").redirectErrorStream(true).start();
            if (!process.waitFor(3, TimeUnit.SECONDS)) {
                process.destroyForcibly();
                return "";
            }
            try (InputStream input = process.getInputStream()) {
                return new String(input.readAllBytes(), StandardCharsets.UTF_8);
            }
        } catch (Exception ignored) {
            return "";
        }
    }
}
