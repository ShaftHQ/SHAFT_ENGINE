package com.shaft.infrastructure;

import java.nio.file.Path;
import java.util.Objects;

/** OS-native user-scoped cache and durable-data locations; resolution performs no mutation. */
public record ShaftCachePaths(Path cacheRoot, Path dataRoot, Path downloads, Path tools,
                              Path state, Path receipts) {
    public ShaftCachePaths {
        cacheRoot = absolute(cacheRoot, "cacheRoot");
        dataRoot = absolute(dataRoot, "dataRoot");
        downloads = child(cacheRoot, downloads, "downloads");
        tools = child(dataRoot, tools, "tools");
        state = child(dataRoot, state, "state");
        receipts = child(dataRoot, receipts, "receipts");
    }

    public Path cache() { return cacheRoot.resolve("artifacts"); }

    public static ShaftCachePaths current() {
        Path home = Path.of(System.getProperty("user.home")).toAbsolutePath();
        return resolve(SetupPlatform.current(), home, environmentPath("LOCALAPPDATA"),
                environmentPath("XDG_CACHE_HOME"), environmentPath("XDG_DATA_HOME"));
    }

    public static ShaftCachePaths resolve(SetupPlatform platform, Path userHome, Path windowsLocalAppData,
                                          Path xdgCacheHome, Path xdgDataHome) {
        Objects.requireNonNull(platform, "platform");
        Path home = absolute(userHome, "userHome");
        Path cacheRoot;
        Path dataRoot;
        switch (platform) {
            case WINDOWS -> {
                dataRoot = (windowsLocalAppData == null ? home.resolve("AppData/Local")
                        : absolute(windowsLocalAppData, "windowsLocalAppData")).resolve("ShaftHQ/SHAFT");
                cacheRoot = dataRoot.resolve("cache");
            }
            case MACOS -> {
                cacheRoot = home.resolve("Library/Caches/ShaftHQ/SHAFT");
                dataRoot = home.resolve("Library/Application Support/ShaftHQ/SHAFT");
            }
            case LINUX -> {
                cacheRoot = (xdgCacheHome == null ? home.resolve(".cache") : absolute(xdgCacheHome, "xdgCacheHome"))
                        .resolve("shafthq/shaft");
                dataRoot = (xdgDataHome == null ? home.resolve(".local/share") : absolute(xdgDataHome, "xdgDataHome"))
                        .resolve("shafthq/shaft");
            }
            default -> throw new IllegalStateException("Unhandled platform: " + platform);
        }
        return new ShaftCachePaths(cacheRoot, dataRoot, cacheRoot.resolve("downloads"),
                dataRoot.resolve("tools"), dataRoot.resolve("state"), dataRoot.resolve("receipts"));
    }

    private static Path environmentPath(String name) {
        return environmentPathValue(System.getenv(name));
    }

    static Path environmentPathValue(String value) {
        if (value == null || value.isBlank()) return null;
        Path path = Path.of(value).normalize();
        return path.isAbsolute() ? path : null;
    }

    private static Path absolute(Path path, String name) {
        Path value = Objects.requireNonNull(path, name).normalize();
        if (!value.isAbsolute()) throw new IllegalArgumentException(name + " must be absolute.");
        return value;
    }

    private static Path child(Path root, Path path, String name) {
        Path value = absolute(path, name);
        if (!value.startsWith(root)) throw new IllegalArgumentException(name + " must stay inside its SHAFT root.");
        return value;
    }
}
