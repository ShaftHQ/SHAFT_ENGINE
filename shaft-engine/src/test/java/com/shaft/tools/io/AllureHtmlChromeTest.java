package com.shaft.tools.io;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
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
}
