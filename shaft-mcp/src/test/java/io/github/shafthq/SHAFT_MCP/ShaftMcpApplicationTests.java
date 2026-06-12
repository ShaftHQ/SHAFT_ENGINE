package io.github.shafthq.SHAFT_MCP;

import org.junit.jupiter.api.Test;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest(properties = "spring.ai.mcp.server.enabled=false")
class ShaftMcpApplicationTests {

    private static final Set<String> REPRESENTATIVE_TOOLS = Set.of(
            "driver_initialize",
            "browser_navigate",
            "browser_get_current_url",
            "element_click",
            "capture_start",
            "capture_status",
            "capture_stop",
            "capture_generate",
            "doctor_analyze"
    );

    @Autowired
    private ApplicationContext context;

	@Test
	void contextRegistersExistingMcpToolApi() {
        Object bean = context.getBean("shaftTools");
        assertTrue(bean instanceof List<?>);
        List<?> callbacks = (List<?>) bean;
        assertEquals(45, callbacks.size());

        Set<String> toolNames = callbacks.stream()
                .map(ToolCallback.class::cast)
                .map(callback -> callback.getToolDefinition().name())
                .collect(Collectors.toSet());
        assertTrue(toolNames.containsAll(REPRESENTATIVE_TOOLS));
    }
}
