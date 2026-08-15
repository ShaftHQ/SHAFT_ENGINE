package com.shaft.infrastructure;

import java.io.IOException;
import java.time.Duration;

/** Explicit CLI/API access to lease-safe Android runtime stop and bounded logs. */
public final class AndroidRuntimeManager {
    private AndroidRuntimeManager() { }

    public static boolean stop(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                               AndroidSetupRequest request, Duration timeout) throws IOException {
        return service(paths, platform, architecture, request).stop(timeout);
    }

    public static String logs(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                              AndroidSetupRequest request) throws IOException {
        return service(paths, platform, architecture, request).logs();
    }

    private static AndroidLifecycleService service(ShaftCachePaths paths, SetupPlatform platform,
                                                   SetupArchitecture architecture, AndroidSetupRequest request) {
        AndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths, platform,
                architecture, request, true);
        return new AndroidLifecycleService(paths, platform, architecture, request, operations,
                new SystemAndroidRuntimeController(), new SystemAndroidRuntimeHealth(paths, platform, architecture));
    }

}
