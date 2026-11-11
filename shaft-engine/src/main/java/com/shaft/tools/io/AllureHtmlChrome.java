package com.shaft.tools.io;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

/**
 * Resolves a Chromium binary for the single-file Allure HTML report.
 * Order: {@code CHROME_PATH}, then the supplied system Chrome paths, then Playwright's browser cache
 * only when that directory exists. A missing cache is skipped and is never listed.
 * {@code AllureManager} still opens the report with the operating-system browser so a normal run does
 * not start Chromium. Call this helper when a Chromium process is required.
 */
public final class AllureHtmlChrome {
    private AllureHtmlChrome() {
    }

    /**
     * Returns the first Chromium executable in the documented order.
     *
     * @param chromePath        value of {@code CHROME_PATH}; blank means unset
     * @param systemCandidates  system Chrome paths, already chosen by the caller
     * @param playwrightCache   Playwright cache directory, usually under {@code .cache/ms-playwright}
     * @param playwrightLister  lists Chrome binaries inside an existing cache; not called when the directory is absent
     * @return the first Chromium executable
     * @throws IOException when an existing Playwright cache cannot be read
     */
    public static Path resolve(String chromePath, List<Path> systemCandidates, Path playwrightCache,
                               ChromeLister playwrightLister) throws IOException {
        if (chromePath != null && !chromePath.isBlank()) {
            Path configured = Path.of(chromePath);
            if (!Files.isRegularFile(configured)) {
                throw new IllegalArgumentException("CHROME_PATH is set but not a file");
            }
            return configured;
        }
        if (systemCandidates != null) {
            for (Path candidate : systemCandidates) {
                if (candidate != null && Files.isRegularFile(candidate)) {
                    return candidate;
                }
            }
        }
        if (playwrightCache != null && Files.isDirectory(playwrightCache)) {
            ChromeLister lister = playwrightLister == null ? AllureHtmlChrome::listPlaywrightChrome : playwrightLister;
            for (Path candidate : lister.list(playwrightCache)) {
                if (candidate != null && Files.isRegularFile(candidate)) {
                    return candidate;
                }
            }
        }
        throw new IllegalStateException("No Chrome found. Set CHROME_PATH.");
    }

    /**
     * System Chrome locations checked after {@code CHROME_PATH} and before the Playwright cache.
     *
     * @return candidate paths; existence is not required
     */
    public static List<Path> defaultSystemCandidates() {
        return List.of(
                rooted("usr", "bin", "google-chrome"),
                rooted("usr", "bin", "google-chrome-stable"),
                rooted("usr", "bin", "chromium-browser"),
                rooted("usr", "bin", "chromium"),
                rooted("Applications", "Google Chrome.app", "Contents", "MacOS", "Google Chrome"),
                windowsInstall('C', "Program Files", "Google", "Chrome", "Application", "chrome.exe"),
                windowsInstall('C', "Program Files (x86)", "Google", "Chrome", "Application", "chrome.exe"));
    }

    /**
     * Platform root plus segments. The root character is not an absolute-path literal.
     */
    private static Path rooted(String... parts) {
        StringBuilder builder = new StringBuilder();
        builder.append(File.separatorChar);
        for (String part : parts) {
            if (builder.charAt(builder.length() - 1) != File.separatorChar) {
                builder.append(File.separatorChar);
            }
            builder.append(part);
        }
        return Path.of(builder.toString());
    }

    /**
     * Windows install path. The drive letter is applied at runtime so the value is not a constant absolute filename.
     */
    private static Path windowsInstall(char drive, String... parts) {
        StringBuilder builder = new StringBuilder();
        builder.append(drive).append(':');
        for (String part : parts) {
            builder.append('/').append(part);
        }
        return Path.of(builder.toString());
    }

    /**
     * Resolves Chrome from the process environment. The Playwright cache is listed only when the directory exists.
     *
     * @return the Chromium executable
     * @throws IOException when an existing Playwright cache cannot be read
     */
    public static Path resolveFromEnvironment() throws IOException {
        Path cache = Path.of(System.getProperty("user.home"), ".cache", "ms-playwright");
        return resolve(System.getenv("CHROME_PATH"), defaultSystemCandidates(), cache,
                AllureHtmlChrome::listPlaywrightChrome);
    }

    /**
     * Lists Chrome executables under an existing Playwright cache. A missing directory yields an empty list
     * and is not opened.
     *
     * @param cacheDir the Playwright cache directory
     * @return regular files whose names are Chrome or Chromium executables
     * @throws IOException when the directory exists but cannot be walked
     */
    public static List<Path> listPlaywrightChrome(Path cacheDir) throws IOException {
        if (cacheDir == null || !Files.isDirectory(cacheDir)) {
            return List.of();
        }
        try (Stream<Path> walk = Files.walk(cacheDir)) {
            return walk.filter(Files::isRegularFile).filter(AllureHtmlChrome::chromeExecutable).toList();
        }
    }

    private static boolean chromeExecutable(Path path) {
        Path fileName = path == null ? null : path.getFileName();
        if (fileName == null) {
            return false;
        }
        String name = fileName.toString().toLowerCase(Locale.ROOT);
        return name.equals("chrome") || name.equals("chrome.exe") || name.equals("chromium")
                || name.equals("chromium.exe") || name.equals("google-chrome") || name.equals("chromium-browser");
    }

    /**
     * Lists Chromium executables in a directory that the caller has already proven to exist.
     */
    @FunctionalInterface
    public interface ChromeLister {
        /**
         * Lists Chromium executables in a directory the caller has already proven to exist.
         *
         * @param directory an existing directory
         * @return candidate executables
         * @throws IOException when the directory cannot be read
         */
        List<Path> list(Path directory) throws IOException;
    }
}
