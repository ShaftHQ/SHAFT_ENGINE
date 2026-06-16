package com.shaft.mcp;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.boot.test.context.SpringBootTest;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest(properties = "spring.ai.mcp.server.enabled=false")
class ShaftMcpApplicationTests {
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
        assertTrue(toolNames.contains("doctor_analyze_failed_allure"));
        assertTrue(toolNames.contains("capture_generate_replay"));
        assertTrue(toolNames.contains("capture_checkpoint"));
        assertTrue(toolNames.contains("browser_get_page_dom"));
        assertTrue(toolNames.contains("browser_take_screenshot"));
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
        assertTrue(toolNames.contains("element_click_semantic"));
        assertTrue(toolNames.contains("natural_act"));
        assertTrue(!toolNames.contains("doctor_publish_draft_pr"));
        assertTrue(!toolNames.contains("browser_get_page_source"));
        assertTrue(!toolNames.contains("browser_get_cookie"));
        assertTrue(!toolNames.contains("element_click_ai"));
    }

    private static Set<String> expectedTools() throws Exception {
        var root = new ObjectMapper().readTree(Files.readString(Path.of(
                "src/test/resources/fixtures/mcp-tool-manifest.json")));
        return java.util.stream.StreamSupport.stream(root.path("tools").spliterator(), false)
                .map(node -> node.path("name").asText())
                .collect(Collectors.toCollection(java.util.TreeSet::new));
    }
}
