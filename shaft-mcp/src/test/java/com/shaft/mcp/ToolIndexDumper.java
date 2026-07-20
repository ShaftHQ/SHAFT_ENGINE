package com.shaft.mcp;

import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.spec.McpSchema;
import org.springframework.ai.support.ToolCallbacks;
import org.springframework.ai.tool.ToolCallback;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Java-side mechanical tool-index dump (design doc Decision 4 / amendment A5): reads the ACTUAL
 * registered {@code ToolCallback}/{@code @McpTool} schemas straight off the live Spring context --
 * never a regex scan over source -- so the dump cannot lie about what schema Spring AI truly
 * serves (the exact defect a source-text scanner would have: it would miss policy-driven defaults
 * and any schema-generation quirk).
 */
final class ToolIndexDumper {

    private ToolIndexDumper() {
    }

    /**
     * Dumps every {@code @Tool}-annotated method on {@code serviceBeans} (via
     * {@link ToolCallbacks#from(Object)}, exactly as {@code ShaftMcpApplication.shaftTools()}
     * builds the live tool list) plus every {@code @McpTool}-annotated method reachable through
     * {@code annotationScannedToolSpecs} (the "toolSpecs" bean), tagging each with the simple name
     * of the service bean it came from.
     *
     * @param serviceBeans                the live shaft-mcp service beans (same set
     *                                    {@code ShaftMcpApplication.shaftTools()} takes)
     * @param annotationScannedToolSpecs  the live "toolSpecs" bean (McpTool-annotated tools)
     * @return every tool, sorted by name for a deterministic diff
     */
    static List<ToolIndexEntry> dump(
            List<Object> serviceBeans,
            List<McpServerFeatures.SyncToolSpecification> annotationScannedToolSpecs) {
        List<ToolIndexEntry> entries = new ArrayList<>();
        Map<String, String> mcpToolServiceByName = new HashMap<>();

        for (Object bean : serviceBeans) {
            String serviceName = bean.getClass().getSimpleName();
            for (ToolCallback callback : ToolCallbacks.from(bean)) {
                var definition = callback.getToolDefinition();
                entries.add(new ToolIndexEntry(
                        definition.name(),
                        serviceName,
                        definition.description(),
                        ToolIndexSchema.paramsFromJson(definition.inputSchema())));
            }
            for (Method method : bean.getClass().getMethods()) {
                var mcpTool = method.getAnnotation(org.springframework.ai.mcp.annotation.McpTool.class);
                if (mcpTool != null) {
                    mcpToolServiceByName.put(mcpTool.name(), serviceName);
                }
            }
        }

        for (McpServerFeatures.SyncToolSpecification spec : annotationScannedToolSpecs) {
            McpSchema.Tool tool = spec.tool();
            String serviceName = mcpToolServiceByName.getOrDefault(tool.name(), "UNKNOWN");
            entries.add(new ToolIndexEntry(
                    tool.name(),
                    serviceName,
                    tool.description(),
                    ToolIndexSchema.paramsFromMap(tool.inputSchema())));
        }

        entries.sort(Comparator.comparing(ToolIndexEntry::name));
        return entries;
    }
}
