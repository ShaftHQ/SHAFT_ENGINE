package com.shaft.mcp;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.Test;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest(properties = "spring.ai.mcp.server.enabled=false")
class ShaftMcpApplicationTests {
    private static final Pattern GENERIC_PARAMETER_NAME = Pattern.compile("\"arg\\d+\"");

    @Autowired
    private ApplicationContext context;

	@Test
    void contextRegistersExpectedLeanMcpToolApi() throws Exception {
        Object bean = context.getBean("shaftTools");
        assertTrue(bean instanceof List<?>);
        List<?> callbacks = (List<?>) bean;

        Set<String> toolNames = callbacks.stream()
                .map(ToolCallback.class::cast)
                .map(callback -> callback.getToolDefinition().name())
                .collect(Collectors.toSet());
        Set<String> expected = expectedTools();
        assertEquals(expected, toolNames);
        callbacks.stream()
                .map(ToolCallback.class::cast)
                .forEach(callback -> {
                    String schema = callback.getToolDefinition().inputSchema();
                    assertFalse(GENERIC_PARAMETER_NAME.matcher(schema).find(),
                            callback.getToolDefinition().name() + " exposes generic parameter names: " + schema);
                });
        assertTrue(toolNames.contains("doctor_analyze_failed_allure"));
        assertTrue(toolNames.contains("healer_run_failed_test"));
        assertTrue(toolNames.contains("capture_generate_replay"));
        assertTrue(toolNames.contains("capture_checkpoint"));
        assertTrue(toolNames.contains("browser_get_page_dom"));
        assertTrue(toolNames.contains("browser_take_screenshot"));
        assertTrue(toolNames.contains("playwright_initialize"));
        assertTrue(toolNames.contains("playwright_browser_get_page_dom"));
        assertTrue(toolNames.contains("playwright_element_click"));
        assertTrue(toolNames.contains("playwright_recording_code_blocks"));
        assertTrue(toolNames.contains("playwright_capture_code_blocks"));
        assertTrue(toolNames.contains("playwright_doctor_analyze_failed_allure"));
        assertTrue(toolNames.contains("playwright_healer_run_failed_test"));
        assertTrue(toolNames.contains("mobile_initialize_web_emulation"));
        assertTrue(toolNames.contains("mobile_initialize_native"));
        assertTrue(toolNames.contains("mobile_get_contexts"));
        assertTrue(toolNames.contains("mobile_get_accessibility_tree"));
        assertTrue(toolNames.contains("mobile_take_screenshot"));
        assertTrue(toolNames.contains("mobile_tap"));
        assertTrue(toolNames.contains("mobile_type"));
        assertTrue(toolNames.contains("mobile_record_start"));
        assertTrue(toolNames.contains("mobile_recording_code_blocks"));
        assertTrue(toolNames.contains("mobile_replay_recording"));
        assertTrue(toolNames.contains("natural_act"));
        assertTrue(toolNames.contains("shaft_guide_search"));
        assertTrue(toolNames.contains("test_automation_scenarios"));
        assertTrue(toolNames.contains("test_code_guardrails_check"));
        assertFalse(toolNames.contains("doctor_publish_draft_pr"));
        assertFalse(toolNames.contains("browser_get_page_source"));
        assertFalse(toolNames.contains("browser_get_cookie"));
        assertFalse(toolNames.contains("element_click_semantic"));
        assertFalse(toolNames.contains("element_type_semantic"));
        assertFalse(toolNames.contains("element_click_ai"));
    }

    @Test
    void captureLaunchPrefixBuildsRunnableCaptureCommand() throws Exception {
        Method method = ShaftMcpApplication.class.getDeclaredMethod("captureLaunchPrefix");
        method.setAccessible(true);

        @SuppressWarnings("unchecked")
        List<String> prefix = (List<String>) method.invoke(null);

        assertTrue(prefix.size() >= 3);
        assertTrue(prefix.contains("capture")
                || prefix.contains(com.shaft.capture.cli.CaptureCli.class.getName()));
    }

    @Test
    void absoluteClassPathNormalizesRelativeEntries() throws Exception {
        Method method = ShaftMcpApplication.class.getDeclaredMethod("absoluteClassPath", String.class);
        method.setAccessible(true);

        String normalized = (String) method.invoke(null, "target/classes" + File.pathSeparator + ".");

        for (String entry : normalized.split(java.util.regex.Pattern.quote(File.pathSeparator))) {
            assertTrue(Path.of(entry).isAbsolute(), entry);
        }
    }

    private static Set<String> expectedTools() throws Exception {
        JsonNode root = new ObjectMapper().readTree(Files.readString(Path.of(
                "src/test/resources/fixtures/mcp-tool-manifest.json")));
        assertEquals("1.0", root.path("schemaVersion").asText());
        Set<String> tools = new java.util.TreeSet<>();
        Set<String> duplicates = new java.util.TreeSet<>();
        for (JsonNode tool : root.path("tools")) {
            String name = tool.path("name").asText();
            assertFalse(name.isBlank(), "MCP tool manifest contains a tool without a name");
            if (!tools.add(name)) {
                duplicates.add(name);
            }
            for (String metadata : List.of("mutation", "sensitive", "deprecated")) {
                assertTrue(tool.path(metadata).isBoolean(),
                        "MCP tool manifest tool " + name + " must define boolean " + metadata);
            }
        }
        assertTrue(duplicates.isEmpty(), "MCP tool manifest contains duplicate tools: " + duplicates);
        return tools;
    }
}
