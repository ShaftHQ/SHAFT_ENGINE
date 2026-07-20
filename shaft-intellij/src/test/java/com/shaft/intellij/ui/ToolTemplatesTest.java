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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolTemplatesTest {
    private static final String MANIFEST_RELATIVE_PATH = "shaft-mcp/src/test/resources/fixtures/mcp-tool-manifest.json";
    private static final Set<String> RISK_WHITELIST = Set.of("Preview Current Project Upgrade:shaft_project_upgrade");
    private static final Set<String> HIGH_RISK_MUTATION_TOOL_NAMES = Set.of(
            "browser_open_intent",
            "browser_take_screenshot",
            "capture_backend_comparison",
            "capture_record_at_target_code_blocks");

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

        assertTrue(templates.get("Create SHAFT Project").confirmationRequired());
        assertFalse(templates.get("Create SHAFT Project").description().isBlank());
        assertFalse(templates.get("Preview Current Project Upgrade").confirmationRequired());
        assertTrue(templates.get("Apply Current Project Upgrade").confirmationRequired());
        assertFalse(templates.get("Apply Current Project Upgrade").description().isBlank());
    }

    @Test
    void workflowTemplatesExposePlaywrightAndInspectorCoverage() {
        // Tool sweep W1 (89-tool catalog): Playwright no longer has its own tool names -- it is
        // reached through the same capture_*/browser_* tools as WebDriver and Mobile, dispatching
        // on whichever engine is active (or an explicit backend:"playwright" argument). Coverage
        // for Playwright is therefore proven by the presence of a "backend":"playwright" template,
        // not by a separate playwright_* tool name.
        List<ToolTemplate> templates = allTemplates();
        Set<String> templateTools = templates.stream()
                .map(ToolTemplate::toolName)
                .collect(Collectors.toSet());
        boolean hasPlaywrightBackendTemplate = templates.stream()
                .anyMatch(template -> template.arguments().contains("\"backend\": \"playwright\""));

        assertTrue(hasPlaywrightBackendTemplate);
        assertTrue(templateTools.contains("capture_start"));
        assertTrue(templateTools.contains("capture_status"));
        assertTrue(templateTools.contains("capture_stop"));
        assertTrue(templateTools.contains("capture_code_blocks"));
        assertTrue(templateTools.contains("capture_generate_replay"));
        assertTrue(templateTools.contains("capture_target_candidates"));
        assertTrue(templateTools.contains("capture_record_at_target_code_blocks"));
        assertTrue(templateTools.contains("capture_backend_comparison"));
        assertTrue(templateTools.contains("capture_evidence_pack"));
        assertTrue(templateTools.contains("browser_open_intent"));
        assertTrue(templateTools.contains("browser_get_page_dom"));
        assertTrue(templateTools.contains("browser_take_screenshot"));
        assertTrue(templateTools.contains("trace_read"));
        assertTrue(templateTools.contains("trace_summarize"));
        assertTrue(templateTools.contains("shaft_coding_partner_plan"));
    }

    @Test
    void recordAtTargetTemplateExplainsReviewPreviewAndVerifyFlow() {
        ToolTemplate template = templateByName("capture_record_at_target_code_blocks");

        assertNotNull(template);
        assertEquals("Record Into Java Target", template.label());
        assertTrue(template.confirmationRequired());
        assertTrue(template.arguments().contains("\"targetSourcePath\""));
        assertTrue(template.arguments().contains("\"insertAfter\""));
        assertTrue(template.description().contains("review"));
        assertTrue(template.description().contains("patch preview"));
        assertTrue(template.description().contains("verify"));
    }

    @Test
    void recorderEnhancementTemplatesKeepSafeDefaults() {
        ToolTemplate targets = templateByName("capture_target_candidates");
        ToolTemplate comparison = templateByName("capture_backend_comparison");
        ToolTemplate evidence = templateByName("capture_evidence_pack");

        assertNotNull(targets);
        assertNotNull(comparison);
        assertNotNull(evidence);
        assertFalse(targets.confirmationRequired());
        assertTrue(targets.arguments().contains("\"repositoryPath\": \".\""));
        assertTrue(comparison.confirmationRequired());
        assertTrue(comparison.arguments().contains("\"overwrite\": false"));
        assertFalse(evidence.confirmationRequired());
        assertTrue(evidence.arguments().contains("\"screenshotPaths\": []"));
    }

    @Test
    void codingPartnerTemplateKeepsPreviewOnlyDefaults() {
        ToolTemplate template = templateByName("shaft_coding_partner_plan");

        assertNotNull(template);
        assertEquals("Plan Coding Partner Work", template.label());
        assertFalse(template.confirmationRequired());
        assertTrue(template.arguments().contains("\"repositoryPath\": \".\""));
        assertTrue(template.arguments().contains("\"currentSourcePath\": \"\""));
        assertTrue(template.arguments().contains("\"selectedText\": \"\""));
        assertTrue(template.arguments().contains("\"artifactPaths\": []"));
        assertTrue(template.description().contains("preview"));
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

    @Test
    void parseToolsListSupportsAlternativeContextKeysAndStringValues() {
        String toolsList = """
                {
                  "tools": [
                    {
                      "name": "browser_take_screenshot",
                      "description": "Take a browser screenshot",
                      "contexts": ["browser", "browser-action"]
                    },
                    {
                      "name": "mobile_tap",
                      "description": "Tap an element in a mobile app",
                      "context": "mobile,mobile-element-touch-action"
                    },
                    {
                      "name": "assist_only_tool",
                      "description": "Tool from explicit context",
                      "contextTypes": ["assistant", "diagnostics"]
                    }
                  ]
                }
                """;
        List<DiscoveredTool> tools = ToolCatalog.parseToolsList(toolsList);
        Map<String, DiscoveredTool> indexed = new java.util.LinkedHashMap<>();
        for (DiscoveredTool tool : tools) {
            indexed.put(tool.name(), tool);
        }

        assertEquals(Set.of("all", "browser", "browser-action"), indexed.get("browser_take_screenshot").contextTypes());
        assertEquals(Set.of("all", "mobile", "mobile-element-touch-action"),
                indexed.get("mobile_tap").contextTypes());
        assertEquals(Set.of("all", "assistant", "diagnostics"), indexed.get("assist_only_tool").contextTypes());
    }

    @Test
    void curatedTemplatesInferSearchContextTypes() {
        ToolTemplate browserOpenIntent = templateByName("browser_open_intent");
        assertNotNull(browserOpenIntent);
        assertTrue(browserOpenIntent.contextTypes().contains("browser"));
        assertTrue(browserOpenIntent.contextTypes().contains("browser-session-management"));

        ToolTemplate mobileGetElement = templateByName("mobile_get_accessibility_tree");
        ToolTemplate mobileContexts = templateByName("mobile_get_contexts");
        ToolTemplate mobileSwitchContext = templateByName("mobile_switch_context");
        ToolTemplate mobileScreenshot = templateByLabel("Take Mobile Screenshot");
        assertNotNull(mobileGetElement);
        assertNotNull(mobileContexts);
        assertNotNull(mobileSwitchContext);
        assertNotNull(mobileScreenshot);
        assertTrue(mobileGetElement.contextTypes().contains("mobile"));
        assertTrue(mobileContexts.arguments().contains("8000"));
        assertTrue(mobileSwitchContext.arguments().contains("contextName"));
        assertTrue(mobileScreenshot.arguments().contains("includeBase64"));

        ToolTemplate traceRead = templateByName("trace_read");
        assertNotNull(traceRead);
        assertTrue(traceRead.contextTypes().contains("traces"));
    }

    @Test
    void parseToolsListResultEnvelopeSupportsMcpStylePayload() {
        String toolsList = """
                {
                  "jsonrpc": "2.0",
                  "result": {
                    "tools": [
                      {
                        "name": "capture_start",
                        "description": "Start or resume a capture session"
                      },
                      {
                        "name": "new_dynamic_tool",
                        "description": "Tool not in curated templates"
                      }
                    ]
                  }
                }
                """;

        assertEquals(List.of(
                        new DiscoveredTool("capture_start", "Start or resume a capture session"),
                        new DiscoveredTool("new_dynamic_tool", "Tool not in curated templates")),
                ToolCatalog.parseToolsList(toolsList));
    }

    @Test
    void mergeDiscoveredToolsUpdatesDescriptionsAndAddsFallbackTemplates() {
        String toolsList = """
                {
                  "tools": [
                    {
                      "name": "capture_start",
                      "description": "Start or resume a capture session"
                    },
                    {
                      "name": "new_dynamic_tool",
                      "description": "Tool not in curated templates"
                    }
                  ]
                }
                """;

        Map<String, ToolTemplate> mergedByTool = ToolTemplates.categories(toolsList).stream()
                .flatMap(category -> category.templates().stream())
                .collect(Collectors.toMap(ToolTemplate::toolName, template -> template, (first, second) -> first));

        assertEquals("Start or resume a capture session", mergedByTool.get("capture_start").description());
        assertEquals("new_dynamic_tool", mergedByTool.get("new_dynamic_tool").label());
        assertEquals("{}", mergedByTool.get("new_dynamic_tool").arguments());
        assertEquals("Tool not in curated templates", mergedByTool.get("new_dynamic_tool").description());
    }

    @Test
    void guideCommandInvocationResolvesAgainstProbedStdioToolList() {
        String probedToolsList = """
                {
                  "tools": [
                    {
                      "name": "shaft_guide_search",
                      "description": "searches the live official SHAFT user guide"
                    },
                    {
                      "name": "test_automation_scenarios",
                      "description": "returns SHAFT MCP usage scenarios"
                    },
                    {
                      "name": "shaft_coding_partner_plan",
                      "description": "plans a repository-aware SHAFT IntelliJ coding-partner workflow"
                    },
                    {
                      "name": "test_code_guardrails_check",
                      "description": "checks generated SHAFT test code for lexical anti-patterns"
                    }
                  ]
                }
                """;

        AssistantCommand.Invocation guideInvocation = AssistantCommand.fromPrompt(
                "/guide page objects locators", "CODEX", "AGENT", "C:/work/project", "", false);
        assertEquals("shaft_guide_search", guideInvocation.toolName());

        Map<String, ToolTemplate> mergedByTool = ToolTemplates.categories(probedToolsList).stream()
                .flatMap(category -> category.templates().stream())
                .collect(Collectors.toMap(ToolTemplate::toolName, template -> template, (first, second) -> first));

        ToolTemplate resolved = mergedByTool.get(guideInvocation.toolName());
        assertNotNull(resolved, "/guide invocation tool " + guideInvocation.toolName()
                + " did not resolve against the probed stdio tools/list");
        assertEquals("searches the live official SHAFT user guide", resolved.description());
    }

    @Test
    void mergeDiscoveredToolsFallsBackToMcpCategoryWhenEnvelopeStyle() {
        String toolsList = """
                {
                  "jsonrpc": "2.0",
                  "result": {
                    "tools": [
                      {
                        "name": "new_toolkit_tool",
                        "description": "Tool from result envelope"
                      }
                    ]
                  }
                }
                """;

        Map<String, ToolTemplate> mergedByTool = ToolTemplates.categories(toolsList).stream()
                .filter(category -> category.label().equals("MCP"))
                .flatMap(category -> category.templates().stream())
                .collect(Collectors.toMap(ToolTemplate::toolName, template -> template, (first, second) -> first));

        assertTrue(mergedByTool.containsKey("new_toolkit_tool"));
        assertEquals("{}", mergedByTool.get("new_toolkit_tool").arguments());
        assertEquals("Tool from result envelope", mergedByTool.get("new_toolkit_tool").description());
    }

    private static boolean isMutationRisk(ToolTemplate template) {
        JsonObject arguments = JsonParser.parseString(template.arguments()).getAsJsonObject();
        return booleanFlagEnabled(arguments, "approve")
                || booleanFlagEnabled(arguments, "sourcePatchConsent")
                || booleanFlagSet(arguments, "dryRun", false)
                || HIGH_RISK_MUTATION_TOOL_NAMES.contains(template.toolName())
                || template.toolName().contains("healer")
                || template.toolName().contains("project_create")
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

    private static ToolTemplate templateByName(String toolName) {
        return allTemplates().stream()
                .filter(template -> toolName.equals(template.toolName()))
                .findFirst()
                .orElse(null);
    }

    private static ToolTemplate templateByLabel(String label) {
        return allTemplates().stream()
                .filter(template -> label.equals(template.label()))
                .findFirst()
                .orElse(null);
    }
}
