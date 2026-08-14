package com.shaft.gui.playwright.internal;

import com.shaft.infrastructure.PlaywrightRuntime;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupProfile;

import java.util.Map;
import java.util.Objects;
import java.nio.file.Path;
import java.util.function.Function;

/** Applies managed Playwright browser ownership only to the child driver process. */
final class PlaywrightManagedRuntime {
    private PlaywrightManagedRuntime() {
        throw new IllegalStateException("Utility class");
    }

    static Map<String, String> environment(SetupOptions options, String connectionMode, String channel,
                                           Map<String, String> processEnvironment) {
        return environment(options, connectionMode, channel, processEnvironment,
                PlaywrightRuntime::resolveBrowserRoot);
    }

    static Map<String, String> environment(SetupOptions options, String connectionMode, String channel,
                                           Map<String, String> processEnvironment,
                                           Function<SetupOptions, Path> browserRootResolver) {
        SetupOptions selected = Objects.requireNonNull(options, "options");
        String mode = Objects.requireNonNull(connectionMode, "connectionMode").trim();
        String selectedChannel = Objects.requireNonNull(channel, "channel").trim();
        Map<String, String> inherited = Objects.requireNonNull(processEnvironment, "processEnvironment");
        Function<SetupOptions, Path> resolver = Objects.requireNonNull(browserRootResolver, "browserRootResolver");
        if (!(mode.isEmpty() || "local".equalsIgnoreCase(mode)) || !selectedChannel.isEmpty()
                || !inherited.getOrDefault("PLAYWRIGHT_BROWSERS_PATH", "").isBlank()
                || selected.profile() != SetupProfile.PLAYWRIGHT
                || selected.effectiveMode() != SetupMode.MANAGED) {
            return Map.of();
        }
        return Map.of("PLAYWRIGHT_BROWSERS_PATH",
                resolver.apply(selected).toAbsolutePath().normalize().toString());
    }
}
