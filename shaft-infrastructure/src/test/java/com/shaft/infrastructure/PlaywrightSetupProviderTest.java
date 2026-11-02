package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightSetupProviderTest {
    @Test
    void profileIsPlaywrightAndExternalStatusNeverTouchesManagedTools(@TempDir Path temp) {
        PlaywrightSetupProvider provider = new PlaywrightSetupProvider();
        assertEquals(SetupProfile.PLAYWRIGHT, provider.profile());

        ShaftCachePaths paths = paths(temp);
        SetupOptions options = SetupOptions.defaults(SetupProfile.PLAYWRIGHT, paths);

        SetupPlan plan = provider.plan(options, SetupPlatform.WINDOWS, SetupArchitecture.X64);
        assertEquals(SetupMode.EXTERNAL, plan.mode());
        assertEquals(SetupProfile.PLAYWRIGHT, plan.profile());

        SetupReport report = provider.status(options, SetupPlatform.WINDOWS, SetupArchitecture.X64);
        assertEquals(SetupReadiness.MISSING, report.readiness());
        assertTrue(report.targets().stream().allMatch(target -> target.readiness() == SetupReadiness.MISSING));
        assertTrue(report.targets().stream().allMatch(target ->
                target.detail().contains("diagnostic-only")));
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
