package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ShaftCachePathsTest {
    private final Path home = Path.of(System.getProperty("user.home")).toAbsolutePath().resolve("shaft-test");

    @Test
    void separatesPurgeableCacheFromDurableStateOnEveryPlatform() {
        ShaftCachePaths windows = ShaftCachePaths.resolve(SetupPlatform.WINDOWS, home, null, null, null);
        assertEquals(home.resolve("AppData/Local/ShaftHQ/SHAFT/cache"), windows.cacheRoot());
        assertEquals(home.resolve("AppData/Local/ShaftHQ/SHAFT/receipts"), windows.receipts());

        ShaftCachePaths mac = ShaftCachePaths.resolve(SetupPlatform.MACOS, home, null, null, null);
        assertEquals(home.resolve("Library/Caches/ShaftHQ/SHAFT"), mac.cacheRoot());
        assertEquals(home.resolve("Library/Application Support/ShaftHQ/SHAFT/receipts"), mac.receipts());

        ShaftCachePaths linux = ShaftCachePaths.resolve(SetupPlatform.LINUX, home, null,
                home.resolve("xdg-cache"), home.resolve("xdg-data"));
        assertEquals(home.resolve("xdg-cache/shafthq/shaft"), linux.cacheRoot());
        assertEquals(home.resolve("xdg-data/shafthq/shaft/receipts"), linux.receipts());
    }

    @Test
    void rejectsRelativeAndRootEscapingLayouts() {
        assertThrows(IllegalArgumentException.class,
                () -> ShaftCachePaths.resolve(SetupPlatform.LINUX, Path.of("relative"), null, null, null));
        assertThrows(IllegalArgumentException.class, () -> new ShaftCachePaths(home, home,
                home.resolve("../escape"), home.resolve("tools"), home.resolve("state"), home.resolve("receipts")));
        assertEquals(null, ShaftCachePaths.environmentPathValue("relative-cache"));
        assertEquals(home, ShaftCachePaths.environmentPathValue(home.toString()));
    }

    @Test
    void platformSelectionIsExplicitAndHandlesDarwinBeforeWindows() {
        assertEquals(SetupPlatform.WINDOWS, SetupPlatform.fromOsName("Windows 11"));
        assertEquals(SetupPlatform.MACOS, SetupPlatform.fromOsName("Mac OS X"));
        assertEquals(SetupPlatform.MACOS, SetupPlatform.fromOsName("Darwin"));
        assertEquals(SetupPlatform.LINUX, SetupPlatform.fromOsName("Linux"));
        assertThrows(IllegalArgumentException.class, () -> SetupPlatform.fromOsName("Plan 9"));
        assertEquals(SetupPlatform.fromOsName(System.getProperty("os.name")), SetupPlatform.current());
        assertEquals(3, SetupPlatform.values().length);
        assertEquals(5, SetupReadiness.values().length);
        ShaftCachePaths current = ShaftCachePaths.current();
        assertEquals(current.cacheRoot().resolve("artifacts"), current.cache());
    }
}
