package com.shaft.intellij.settings;

import com.shaft.intellij.mcp.ShaftPluginExecutor;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import javax.swing.SwingUtilities;

/**
 * The {@code grok} executable is shared by Grok CLI and Grok Build. Grok Build's
 * help text identifies itself as "Grok Build"; anything else stays Grok CLI.
 * <p>
 * The probe runs {@code grok --help} once and caches the label (issue #6634): combo renderers call
 * {@link #detectedLabel()} on every repaint, so an uncached probe spawned a process on the EDT each
 * time. On the EDT an unprobed call returns {@link #GROK_CLI} and probes in the background.</p>
 */
public final class GrokInstallIdentity {
    public static final String GROK_BUILD = "Grok Build";
    public static final String GROK_CLI = "Grok CLI";

    private static volatile Supplier<String> helpText = GrokInstallIdentity::readHelp;
    private static volatile String cachedLabel;
    private static volatile boolean probing;
    /** Bumped by {@link #useHelp}; a probe started under an older generation must not cache its label. */
    private static int generation; // guarded by GrokInstallIdentity.class

    private GrokInstallIdentity() {
        throw new IllegalStateException("Utility class");
    }

    public static String labelFromHelp(String help) {
        return help != null && help.contains("Grok Build") ? GROK_BUILD : GROK_CLI;
    }

    public static String detectedLabel() {
        String label = cachedLabel;
        if (label != null) {
            return label;
        }
        if (SwingUtilities.isEventDispatchThread()) {
            probeInBackground();
            return GROK_CLI;
        }
        return probe();
    }

    private static synchronized void probeInBackground() {
        if (!probing && cachedLabel == null) {
            probing = true;
            CompletableFuture.runAsync(GrokInstallIdentity::probe, ShaftPluginExecutor.getInstance().executor());
        }
    }

    private static String probe() {
        Supplier<String> source;
        int startedGeneration;
        synchronized (GrokInstallIdentity.class) {
            source = helpText;
            startedGeneration = generation;
        }
        String label;
        try {
            label = labelFromHelp(source.get());
        } catch (RuntimeException ignored) {
            label = GROK_CLI;
        }
        synchronized (GrokInstallIdentity.class) {
            if (startedGeneration == generation) {
                cachedLabel = label;
            }
            probing = false;
        }
        return label;
    }

    /** Test seam. Pass null to restore the live {@code grok --help} probe. */
    public static synchronized void useHelp(Supplier<String> supplier) {
        helpText = supplier == null ? GrokInstallIdentity::readHelp : supplier;
        generation++;
        cachedLabel = null;
    }

    private static void deleteQuietly(File file) {
        if (file != null) {
            try {
                Files.deleteIfExists(file.toPath());
            } catch (java.io.IOException ignored) {
                // A leftover temp file is harmless.
            }
        }
    }

    /** Output goes to a temp file so a full pipe can never stall {@code waitFor}. */
    static String readHelp() {
        File output = null;
        try {
            output = File.createTempFile("shaft-grok-help", ".txt");
            Process process = new ProcessBuilder("grok", "--help")
                    .redirectErrorStream(true).redirectOutput(output).start();
            if (!process.waitFor(3, TimeUnit.SECONDS)) {
                process.destroyForcibly();
                return "";
            }
            return Files.readString(output.toPath(), StandardCharsets.UTF_8);
        } catch (Exception ignored) {
            return "";
        } finally {
            deleteQuietly(output);
        }
    }
}
