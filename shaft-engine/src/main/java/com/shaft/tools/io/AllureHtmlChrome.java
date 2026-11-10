package com.shaft.tools.io;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
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
        List<Path> candidates = new ArrayList<>();
        candidates.add(Path.of("/usr/bin/google-chrome"));
        candidates.add(Path.of("/usr/bin/google-chrome-stable"));
        candidates.add(Path.of("/usr/bin/chromium-browser"));
        candidates.add(Path.of("/usr/bin/chromium"));
        candidates.add(Path.of("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"));
        candidates.add(Path.of("C:/Program Files/Google/Chrome/Application/chrome.exe"));
        candidates.add(Path.of("C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"));
        return List.copyOf(candidates);
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
        String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
        return name.equals("chrome") || name.equals("chrome.exe") || name.equals("chromium")
                || name.equals("chromium.exe") || name.equals("google-chrome") || name.equals("chromium-browser");
    }

    /**
     * Lists Chromium executables in a directory that the caller has already proven to exist.
     */
    @FunctionalInterface
    public interface ChromeLister {
        /**
         * @param directory an existing directory
         * @return candidate executables
         * @throws IOException when the directory cannot be read
         */
        List<Path> list(Path directory) throws IOException;
    }
}
