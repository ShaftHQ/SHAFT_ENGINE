package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolTemplatesTest {
    @Test
    void categoriesKeepCopilotLikeAssistantToolsSecondary() {
        List<String> labels = ToolTemplates.categories().stream()
                .map(ToolCategory::label)
                .toList();

        assertEquals(List.of("Recorder", "Playback", "Doctor", "Healer", "Inspector", "Projects", "MCP", "Guide"),
                labels);
    }

    @Test
    void projectsExposeCreatePreviewAndApplyTemplates() {
        Map<String, ToolTemplate> templates = ToolTemplates.projects().stream()
                .collect(Collectors.toMap(ToolTemplate::label, template -> template));

        assertEquals("shaft_project_create", templates.get("Create SHAFT Project").toolName());
        assertTrue(templates.get("Create SHAFT Project").arguments().contains("\"runner\": \"TestNG\""));
        assertEquals("shaft_project_upgrade", templates.get("Preview Current Project Upgrade").toolName());
        assertTrue(templates.get("Preview Current Project Upgrade").arguments().contains("\"dryRun\": true"));
        assertEquals("shaft_project_upgrade", templates.get("Apply Current Project Upgrade").toolName());
        assertTrue(templates.get("Apply Current Project Upgrade").arguments().contains("\"approve\": true"));
    }

    @Test
    void projectChangingTemplatesRequireConfirmation() {
        Map<String, ToolTemplate> templates = ToolTemplates.projects().stream()
                .collect(Collectors.toMap(ToolTemplate::label, template -> template));

        assertFalse(templates.get("Preview Current Project Upgrade").confirmationRequired());
        assertTrue(templates.get("Apply Current Project Upgrade").confirmationRequired());
        assertFalse(templates.get("Apply Current Project Upgrade").description().isBlank());
    }
}
