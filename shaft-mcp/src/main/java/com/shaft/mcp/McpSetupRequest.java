package com.shaft.mcp;

import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupSelection;
import com.shaft.infrastructure.ShaftCachePaths;

import java.net.URI;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Locale;

/** Machine-readable setup policy accepted by the MCP adapter. */
public record McpSetupRequest(String profile, String mode, String cacheRoot, String dataRoot,
                              Boolean offline, Boolean autoStart, Boolean preferSystemTools,
                              Boolean reuseOwnedProcesses, String startupTimeout, String shutdownTimeout,
                              String remoteEndpoint, List<String> components) {
    public McpSetupRequest {
        components = components == null ? List.of() : List.copyOf(components);
    }

    SetupProfile setupProfile() {
        if (profile == null || profile.isBlank()) {
            throw new IllegalArgumentException("profile must not be blank.");
        }
        try {
            return SetupProfile.valueOf(profile.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException invalid) {
            throw new IllegalArgumentException("Unsupported setup profile: " + profile, invalid);
        }
    }

    SetupSelection selection() {
        SetupProfile selectedProfile = setupProfile();
        if (selectedProfile != SetupProfile.OCR && !components.isEmpty()) {
            throw new IllegalArgumentException("components are currently supported only for profile OCR.");
        }
        return new SetupSelection(components);
    }

    SetupOptions options() {
        SetupProfile selectedProfile = setupProfile();
        SetupOptions defaults = SetupOptions.defaults(selectedProfile, paths());
        SetupMode selectedMode = mode == null || mode.isBlank()
                ? SetupMode.EXTERNAL : parseMode(mode);
        SetupOptions options = defaults.withMode(selectedMode)
                .withOffline(offline == null ? defaults.offline() : offline)
                .withAutoStart(autoStart == null ? defaults.autoStart() : autoStart)
                .withPreferSystemTools(preferSystemTools == null
                        ? defaults.preferSystemTools() : preferSystemTools)
                .withReuseOwnedProcesses(reuseOwnedProcesses == null
                        ? defaults.reuseOwnedProcesses() : reuseOwnedProcesses)
                .withTimeouts(parseDuration(startupTimeout, defaults.startupTimeout(), "startupTimeout"),
                        parseDuration(shutdownTimeout, defaults.shutdownTimeout(), "shutdownTimeout"));
        if (remoteEndpoint != null && !remoteEndpoint.isBlank()) {
            options = options.withRemoteEndpoint(URI.create(remoteEndpoint.trim()));
        }
        return options;
    }

    private ShaftCachePaths paths() {
        boolean cacheBlank = cacheRoot == null || cacheRoot.isBlank();
        boolean dataBlank = dataRoot == null || dataRoot.isBlank();
        if (cacheBlank && dataBlank) return ShaftCachePaths.current();
        if (cacheBlank || dataBlank) {
            throw new IllegalArgumentException("cacheRoot and dataRoot must be supplied together.");
        }
        Path cache = Path.of(cacheRoot).normalize();
        Path data = Path.of(dataRoot).normalize();
        if (!cache.isAbsolute() || !data.isAbsolute()) {
            throw new IllegalArgumentException("cacheRoot and dataRoot must be absolute.");
        }
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static SetupMode parseMode(String value) {
        try {
            return SetupMode.valueOf(value.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException invalid) {
            throw new IllegalArgumentException("Unsupported setup mode: " + value, invalid);
        }
    }

    private static Duration parseDuration(String value, Duration fallback, String name) {
        if (value == null || value.isBlank()) return fallback;
        try {
            Duration parsed = Duration.parse(value.trim());
            if (parsed.isZero() || parsed.isNegative()) throw new IllegalArgumentException();
            return parsed;
        } catch (RuntimeException invalid) {
            throw new IllegalArgumentException(name + " must be a positive ISO-8601 duration.", invalid);
        }
    }
}
