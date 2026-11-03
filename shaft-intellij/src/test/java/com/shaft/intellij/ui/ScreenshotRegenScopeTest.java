package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * W2.14: screenshot regeneration stays scoped to {@link ShaftPluginScreenshotRendererTest}.
 */
class ScreenshotRegenScopeTest {
    @Test
    void onlyScreenshotRendererTestDocumentsScreenshotDirGate() throws Exception {
        Path root = Path.of("src/test/java");
        String renderer = Files.readString(
                root.resolve("com/shaft/intellij/ui/ShaftPluginScreenshotRendererTest.java"));
        assertTrue(renderer.contains("shaft.intellij.screenshotDir"));
        // Overrides panel screenshot test also gates on the same property but is not the regen entry.
        assertTrue(Files.isRegularFile(
                root.resolve("com/shaft/intellij/testrunner/ShaftRunConfigurationOverridesPanelScreenshotTest.java")));
    }
}
