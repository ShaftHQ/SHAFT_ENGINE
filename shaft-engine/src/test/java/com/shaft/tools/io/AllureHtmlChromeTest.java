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
}
