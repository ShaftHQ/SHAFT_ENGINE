package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantModelCatalogTest {
    @Test
    void cloudCatalogListsModelsForEveryProviderWithGeminiFallback() {
        assertAll(
                () -> assertEquals("gemini-3.5-flash", AssistantModelCatalog.defaultCloudModel("gemini")),
                () -> assertTrue(AssistantModelCatalog.cloudModels("anthropic")
                        .containsAll(List.of("claude-fable-5", "claude-opus-4-8", "claude-sonnet-5"))),
                () -> assertFalse(AssistantModelCatalog.cloudModels("openai").isEmpty()),
                () -> assertFalse(AssistantModelCatalog.cloudModels("github").isEmpty()),
                () -> assertEquals(AssistantModelCatalog.cloudModels("gemini"),
                        AssistantModelCatalog.cloudModels("unknown-provider")));
    }

    @Test
    void localCatalogCoversEveryAssistantFamily() {
        assertAll(
                () -> assertTrue(AssistantModelCatalog.localModels("CLAUDE").contains("claude-sonnet-5")),
                () -> assertFalse(AssistantModelCatalog.localModels("CODEX").isEmpty()),
                () -> assertFalse(AssistantModelCatalog.localModels("COPILOT").isEmpty()),
                () -> assertEquals(AssistantModelCatalog.localModels("CODEX"),
                        AssistantModelCatalog.localModels("unknown-family")));
    }

    @Test
    void effortLevelsStartWithDefaultAndOnlyExplicitLevelsAreForwarded() {
        assertAll(
                () -> assertEquals(AssistantModelCatalog.DEFAULT_EFFORT,
                        AssistantModelCatalog.effortLevels().get(0)),
                () -> assertTrue(AssistantModelCatalog.effortLevels()
                        .containsAll(List.of("LOW", "MEDIUM", "HIGH"))),
                () -> assertTrue(AssistantModelCatalog.isExplicitEffort("high")),
                () -> assertTrue(AssistantModelCatalog.isExplicitEffort("Medium")),
                () -> assertFalse(AssistantModelCatalog.isExplicitEffort(AssistantModelCatalog.DEFAULT_EFFORT)),
                () -> assertFalse(AssistantModelCatalog.isExplicitEffort("")),
                () -> assertFalse(AssistantModelCatalog.isExplicitEffort(null)),
                () -> assertFalse(AssistantModelCatalog.isExplicitEffort("EXTREME")));
    }
}
