package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.time.Duration;

/** Explicit CLI/API access to lease-safe Android runtime stop and bounded logs. */
public final class AndroidRuntimeManager {
    private static final long MAX_LOG_BYTES = 2L * 1024 * 1024;

    private AndroidRuntimeManager() { }

    public static boolean stop(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                               AndroidSetupRequest request, Duration timeout) throws IOException {
        return service(paths, platform, architecture, request).stop(timeout);
    }

    public static String logs(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                              AndroidSetupRequest request) throws IOException {
        AndroidLifecycleService service = service(paths, platform, architecture, request);
        return read("emulator", service.emulatorLog()) + read("appium", service.appiumLog());
    }

    private static AndroidLifecycleService service(ShaftCachePaths paths, SetupPlatform platform,
                                                   SetupArchitecture architecture, AndroidSetupRequest request) {
        AndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths, platform,
                architecture, request, true);
        return new AndroidLifecycleService(paths, platform, architecture, request, operations,
                new SystemAndroidRuntimeController(), new SystemAndroidRuntimeHealth(paths, platform, architecture));
    }

    private static String read(String role, Path path) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) return "";
        long size = Files.size(path);
        if (size > MAX_LOG_BYTES) throw new IOException(role + " log exceeds the 2 MiB safety limit: " + path);
        return "== " + role + " ==" + System.lineSeparator()
                + Files.readString(path, StandardCharsets.UTF_8) + System.lineSeparator();
    }
}
