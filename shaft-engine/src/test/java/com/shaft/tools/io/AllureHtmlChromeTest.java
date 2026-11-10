package com.shaft.tools.io;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

/**
 * Chromium for the single-file Allure HTML must not scan a missing Playwright cache (#7899).
 */
public class AllureHtmlChromeTest {

    @Test
    public void missingPlaywrightCacheUsesSystemChromeAndDoesNotThrowEnoent() throws IOException {
        Path root = Files.createTempDirectory("allure-chrome");
        Path systemChrome = root.resolve("google-chrome");
        Files.writeString(systemChrome, "chrome");
        Path missingCache = root.resolve("ms-playwright");
        Assert.assertFalse(Files.exists(missingCache));

        Path resolved = AllureHtmlChrome.resolve(null, List.of(systemChrome), missingCache, directory -> {
            throw new NoSuchFileException(directory.toString());
        });

        Assert.assertEquals(resolved, systemChrome);
    }

    @Test
    public void chromePathWinsWhenTheFileExists() throws IOException {
        Path root = Files.createTempDirectory("allure-chrome-env");
        Path configured = root.resolve("configured-chrome");
        Path systemChrome = root.resolve("google-chrome");
        Files.writeString(configured, "chrome");
        Files.writeString(systemChrome, "chrome");

        Path resolved = AllureHtmlChrome.resolve(configured.toString(), List.of(systemChrome),
                root.resolve("absent-cache"), directory -> {
                    throw new NoSuchFileException(directory.toString());
                });

        Assert.assertEquals(resolved, configured);
    }

    @Test
    public void chromePathThatIsNotAFileFailsBeforeSystemChrome() throws IOException {
        Path root = Files.createTempDirectory("allure-chrome-bad-env");
        Path systemChrome = root.resolve("google-chrome");
        Files.writeString(systemChrome, "chrome");
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> AllureHtmlChrome.resolve(root.resolve("missing").toString(), List.of(systemChrome),
                        root.resolve("absent-cache"), directory -> List.of()));
        Assert.assertTrue(failure.getMessage().contains("CHROME_PATH"), failure.getMessage());
    }

    @Test
    public void defaultSystemCandidatesIncludeTheLinuxChromePath() {
        Assert.assertTrue(AllureHtmlChrome.defaultSystemCandidates().contains(Path.of("/usr/bin/google-chrome")));
    }

    @Test
    public void listPlaywrightChromeReadsAnExistingCacheAndSkipsAMissingOne() throws IOException {
        Assert.assertTrue(AllureHtmlChrome.listPlaywrightChrome(Path.of("missing-playwright-cache")).isEmpty());
        Path cache = Files.createTempDirectory("pw-cache");
        Path chrome = cache.resolve("chromium-1/chrome-linux/chrome");
        Files.createDirectories(chrome.getParent());
        Files.writeString(chrome, "chrome");
        Files.writeString(cache.resolve("readme.txt"), "skip");
        Assert.assertEquals(AllureHtmlChrome.listPlaywrightChrome(cache), List.of(chrome));
    }

    @Test
    public void resolveFromEnvironmentReturnsAFileOrExplainsTheMiss() throws IOException {
        try {
            Path resolved = AllureHtmlChrome.resolveFromEnvironment();
            Assert.assertTrue(Files.isRegularFile(resolved));
        } catch (IllegalStateException | IllegalArgumentException failure) {
            Assert.assertFalse(failure.getMessage().isBlank(), failure.getMessage());
        }
    }

    @Test
    public void noConfiguredPathSystemFileOrCacheNamesChromePath() throws IOException {
        Path root = Files.createTempDirectory("allure-chrome-miss");
        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                () -> AllureHtmlChrome.resolve(null, List.of(root.resolve("missing-chrome")),
                        root.resolve("ms-playwright"), directory -> List.of()));
        Assert.assertTrue(failure.getMessage().contains("CHROME_PATH"), failure.getMessage());
    }

    @Test
    public void playwrightCacheSuppliesTheChromeFileTheListerReturns() throws IOException {
        Path root = Files.createTempDirectory("allure-chrome-listed");
        Path cache = root.resolve("ms-playwright");
        Files.createDirectories(cache);
        Path chrome = cache.resolve("chromium-1/chrome-linux/chrome");
        Files.createDirectories(chrome.getParent());
        Files.writeString(chrome, "chrome");
        Path resolved = AllureHtmlChrome.resolve(" ", List.of(root.resolve("missing-chrome")), cache,
                directory -> List.of(chrome));
        Assert.assertEquals(resolved, chrome);
    }

    @Test
    public void nullInputsAndEveryChromeFileNameStillResolve() throws Exception {
        Constructor<AllureHtmlChrome> constructor = AllureHtmlChrome.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
        Assert.assertTrue(AllureHtmlChrome.listPlaywrightChrome(null).isEmpty());

        IllegalStateException missing = Assert.expectThrows(IllegalStateException.class,
                () -> AllureHtmlChrome.resolve(null, null, null, null));
        Assert.assertTrue(missing.getMessage().contains("CHROME_PATH"), missing.getMessage());

        Path root = Files.createTempDirectory("allure-chrome-names");
        Path cache = root.resolve("ms-playwright");
        Files.createDirectories(cache);
        List<String> names = List.of(
                "chrome", "chrome.exe", "chromium", "chromium.exe", "google-chrome", "chromium-browser");
        for (String name : names) {
            Files.writeString(cache.resolve(name), "chrome");
        }
        Files.writeString(cache.resolve("readme.txt"), "skip");
        List<String> found = AllureHtmlChrome.listPlaywrightChrome(cache).stream()
                .map(path -> path.getFileName().toString())
                .toList();
        Assert.assertTrue(found.containsAll(names), found.toString());

        Path resolved = AllureHtmlChrome.resolve(null, null, cache, null);
        Assert.assertTrue(names.contains(resolved.getFileName().toString()), resolved.toString());

        Path chrome = cache.resolve("chrome");
        Path notAFile = root.resolve("missing-chrome");
        Assert.assertEquals(AllureHtmlChrome.resolve(" ", Arrays.asList(null, notAFile), cache,
                directory -> Arrays.asList(null, notAFile, chrome)), chrome);

        IllegalStateException empty = Assert.expectThrows(IllegalStateException.class,
                () -> AllureHtmlChrome.resolve(null, List.of(), cache, directory -> List.of()));
        Assert.assertTrue(empty.getMessage().contains("CHROME_PATH"), empty.getMessage());
    }

    @Test
    public void listPlaywrightChromeAcceptsChromeExe() throws IOException {
        Path cache = Files.createTempDirectory("pw-cache-exe");
        Path chrome = cache.resolve("chromium-1/chrome-win/chrome.exe");
        Files.createDirectories(chrome.getParent());
        Files.writeString(chrome, "chrome");
        Assert.assertEquals(AllureHtmlChrome.listPlaywrightChrome(cache), List.of(chrome));
    }
}
