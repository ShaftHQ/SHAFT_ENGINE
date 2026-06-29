package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolTemplatesTest {
    private static final String MANIFEST_RELATIVE_PATH = "shaft-mcp/src/test/resources/fixtures/mcp-tool-manifest.json";
    private static final Set<String> RISK_WHITELIST = Set.of("Preview Current Project Upgrade:shaft_project_upgrade");
    private static final Set<String> HIGH_RISK_MUTATION_TOOL_NAMES = Set.of(
            "browser_open_intent",
            "browser_take_screenshot",
            "playwright_browser_take_screenshot",
            "playwright_record_start",
            "playwright_record_status",
            "playwright_record_stop",
            "playwright_capture_generate_replay");

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

    @Test
    void workflowTemplatesExposePlaywrightAndInspectorCoverage() {
        Set<String> templateTools = allTemplates().stream()
                .map(ToolTemplate::toolName)
                .collect(Collectors.toSet());

        assertTrue(templateTools.contains("playwright_record_start"));
        assertTrue(templateTools.contains("playwright_record_status"));
        assertTrue(templateTools.contains("playwright_record_stop"));
        assertTrue(templateTools.contains("playwright_recording_code_blocks"));
        assertTrue(templateTools.contains("playwright_capture_code_blocks"));
        assertTrue(templateTools.contains("playwright_capture_generate_replay"));
        assertTrue(templateTools.contains("browser_open_intent"));
        assertTrue(templateTools.contains("browser_get_page_dom"));
        assertTrue(templateTools.contains("browser_take_screenshot"));
        assertTrue(templateTools.contains("playwright_browser_get_page_dom"));
        assertTrue(templateTools.contains("playwright_browser_take_screenshot"));
        assertTrue(templateTools.contains("trace_read"));
        assertTrue(templateTools.contains("trace_summarize"));
    }

    @Test
    void confirmationRequiredTemplatesHaveToolDescriptions() {
        List<String> missingDescription = allTemplates().stream()
                .filter(ToolTemplate::confirmationRequired)
                .filter(template -> template.description().isBlank())
                .map(template -> template.label() + " (" + template.toolName() + ")")
                .toList();

        assertTrue(missingDescription.isEmpty(), "Confirmation templates missing descriptions: " + missingDescription);
    }

    @Test
    void toolTemplatesExistInMcpManifest() throws IOException {
        Set<String> manifestToolNames = manifestToolNames();
        Set<String> templateToolNames = allTemplates().stream()
                .map(ToolTemplate::toolName)
                .collect(Collectors.toCollection(TreeSet::new));

        Set<String> missing = templateToolNames.stream()
                .filter(name -> !manifestToolNames.contains(name))
                .collect(Collectors.toSet());
        assertTrue(missing.isEmpty(), "Templates missing from MCP manifest: " + missing);
    }

    @Test
    void riskyMutationTemplatesRequireConfirmationOrWhitelist() {
        List<String> violations = allTemplates().stream()
                .filter(ToolTemplatesTest::isMutationRisk)
                .filter(template -> !template.confirmationRequired())
                .filter(template -> !isWhitelisted(template))
                .map(template -> template.label() + " (" + template.toolName() + ")")
                .toList();

        assertTrue(violations.isEmpty(), "Missing confirmation for: " + violations);
    }

    private static boolean isMutationRisk(ToolTemplate template) {
        JsonObject arguments = JsonParser.parseString(template.arguments()).getAsJsonObject();
        return booleanFlagEnabled(arguments, "approve")
                || booleanFlagEnabled(arguments, "sourcePatchConsent")
                || booleanFlagSet(arguments, "dryRun", false)
                || HIGH_RISK_MUTATION_TOOL_NAMES.contains(template.toolName())
                || template.toolName().contains("healer")
                || template.toolName().contains("project_upgrade")
                || template.label().contains("Healer")
                || template.label().contains("Upgrade");
    }

    private static boolean booleanFlagEnabled(JsonObject arguments, String key) {
        return arguments.has(key)
                && arguments.get(key).isJsonPrimitive()
                && arguments.get(key).getAsJsonPrimitive().isBoolean()
                && arguments.get(key).getAsBoolean();
    }

    private static boolean booleanFlagSet(JsonObject arguments, String key, boolean expected) {
        return arguments.has(key)
                && arguments.get(key).isJsonPrimitive()
                && arguments.get(key).getAsJsonPrimitive().isBoolean()
                && arguments.get(key).getAsBoolean() == expected;
    }

    private static boolean isWhitelisted(ToolTemplate template) {
        return RISK_WHITELIST.contains(template.label() + ":" + template.toolName());
    }

    private static Set<String> manifestToolNames() throws IOException {
        JsonObject root = JsonParser.parseString(Files.readString(resolveManifestPath())).getAsJsonObject();
        return root.getAsJsonArray("tools").asList().stream()
                .map(node -> node.getAsJsonObject().get("name").getAsString())
                .collect(Collectors.toCollection(TreeSet::new));
    }

    private static Path resolveManifestPath() {
        Path cwd = Path.of(System.getProperty("user.dir")).toAbsolutePath().normalize();
        List<Path> candidates = List.of(
                cwd.resolve(MANIFEST_RELATIVE_PATH),
                cwd.getParent().resolve(MANIFEST_RELATIVE_PATH),
                cwd.resolveSibling("shaft-mcp").resolve("src/test/resources/fixtures/mcp-tool-manifest.json"),
                Path.of(System.getProperty("user.dir"), "shaft-mcp", "src", "test", "resources", "fixtures",
                        "mcp-tool-manifest.json"));
        for (Path candidate : candidates) {
            if (Files.exists(candidate)) {
                return candidate;
            }
        }
        return cwd.resolve(MANIFEST_RELATIVE_PATH);
    }

    private static List<ToolTemplate> allTemplates() {
        return ToolTemplates.categories().stream()
                .flatMap(category -> category.templates().stream())
                .toList();
    }
}
