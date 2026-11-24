package com.shaft.mcp;

import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.spec.McpSchema;
import org.jspecify.annotations.Nullable;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.mcp.annotation.McpTool;
import org.springframework.ai.support.ToolCallbacks;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.DefaultToolDefinition;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.ai.tool.metadata.ToolMetadata;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * Selects which MCP service beans are exposed and strips the redundant JSON Schema
 * dialect URL from each exposed input schema. The default selector is every service.
 */
final class McpToolsets {
    /** Compact tools/list UTF-8 size before issue #8152, measured with all 122 tools. */
    static final int PRE_8152_COMPACT_TOOLS_LIST_BYTES = 79_741;

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final List<ServiceGroup> GROUPS = List.of(
            new ServiceGroup("engine", EngineService.class, List.of("engine")),
            new ServiceGroup("browser", BrowserService.class, List.of("browser")),
            new ServiceGroup("element", ElementService.class, List.of("element")),
            new ServiceGroup("mobile", MobileService.class, List.of("mobile")),
            new ServiceGroup("capture", CaptureService.class, List.of("capture")),
            new ServiceGroup("doctor", DoctorService.class, List.of("doctor")),
            new ServiceGroup("trace", TraceService.class, List.of("trace")),
            new ServiceGroup("healer", HealerService.class, List.of("healer", "heal")),
            new ServiceGroup("guide", GuideService.class, List.of("guide")),
            new ServiceGroup("project", ShaftProjectService.class, List.of("project")),
            new ServiceGroup("test-automation", TestAutomationService.class, List.of("test-automation", "report")),
            new ServiceGroup("coding-partner", CodingPartnerService.class, List.of("coding-partner")),
            new ServiceGroup("autobot", AutobotService.class, List.of("autobot")),
            new ServiceGroup("planner", PlannerService.class, List.of("planner")),
            new ServiceGroup("infrastructure", InfrastructureMcpService.class, List.of("infrastructure", "setup")),
            new ServiceGroup("design", DesignService.class, List.of("design")));
    private static final Map<String, String> ALIASES = aliases();
    private static final Map<String, String> MCP_TOOL_SERVICES = mcpToolServices();
    private static final String VALID_GROUPS = String.join(", ", new TreeMap<>(ALIASES).keySet());

    private McpToolsets() {
    }

    /**
     * Spring property first. When that value is missing or blank, use the raw
     * {@code SHAFT_MCP_TOOLSETS} environment variable so a caller outside Boot binding
     * still honors the opt-in filter.
     */
    static String resolve(@Nullable String springProperty) {
        return resolve(springProperty, System.getenv("SHAFT_MCP_TOOLSETS"));
    }

    static String resolve(@Nullable String springProperty, @Nullable String envValue) {
        if (springProperty != null && !springProperty.isBlank()) {
            return springProperty;
        }
        return envValue == null ? "" : envValue;
    }

    static Map<String, List<ToolCallback>> groupServices(
            EngineService engineService,
            BrowserService browserService,
            ElementService elementService,
            MobileService mobileService,
            CaptureService captureService,
            DoctorService doctorService,
            TraceService traceService,
            HealerService healerService,
            GuideService guideService,
            ShaftProjectService shaftProjectService,
            TestAutomationService testAutomationService,
            CodingPartnerService codingPartnerService,
            AutobotService autobotService,
            PlannerService plannerService,
            InfrastructureMcpService infrastructureMcpService,
            DesignService designService) {
        Map<String, List<ToolCallback>> grouped = new LinkedHashMap<>();
        grouped.put("engine", callbacksOf(engineService));
        grouped.put("browser", callbacksOf(browserService));
        grouped.put("element", callbacksOf(elementService));
        grouped.put("mobile", callbacksOf(mobileService));
        grouped.put("capture", callbacksOf(captureService));
        grouped.put("doctor", callbacksOf(doctorService));
        grouped.put("trace", callbacksOf(traceService));
        grouped.put("healer", callbacksOf(healerService));
        grouped.put("guide", callbacksOf(guideService));
        grouped.put("project", callbacksOf(shaftProjectService));
        grouped.put("test-automation", callbacksOf(testAutomationService));
        grouped.put("coding-partner", callbacksOf(codingPartnerService));
        grouped.put("autobot", callbacksOf(autobotService));
        grouped.put("planner", callbacksOf(plannerService));
        grouped.put("infrastructure", callbacksOf(infrastructureMcpService));
        grouped.put("design", callbacksOf(designService));
        return grouped;
    }

    static List<ToolCallback> exposeCallbacks(
            @Nullable String springProperty, Map<String, List<ToolCallback>> grouped) {
        List<ToolCallback> stripped = new ArrayList<>();
        for (ToolCallback callback : filter(resolve(springProperty), grouped)) {
            stripped.add(new StrippedCallback(callback));
        }
        return List.copyOf(stripped);
    }

    static <T> List<T> filter(@Nullable String selector, Map<String, List<T>> toolsByService) {
        Set<String> selected = selectedServiceKeys(selector);
        List<T> result = new ArrayList<>();
        for (ServiceGroup group : GROUPS) {
            if (!selected.contains(group.key())) {
                continue;
            }
            List<T> tools = toolsByService.get(group.key());
            if (tools != null) {
                result.addAll(tools);
            }
        }
        return List.copyOf(result);
    }

    static Object exposeToolSpecs(@Nullable String springProperty, List<?> specs) {
        List<McpServerFeatures.SyncToolSpecification> typed = new ArrayList<>();
        for (Object spec : specs) {
            if (!(spec instanceof McpServerFeatures.SyncToolSpecification sync)) {
                return specs;
            }
            typed.add(sync);
        }
        return filterToolSpecs(resolve(springProperty), typed);
    }

    static List<ToolView> views(List<ToolCallback> callbacks, List<?> specs) {
        List<ToolView> views = new ArrayList<>();
        for (ToolCallback callback : callbacks) {
            ToolDefinition definition = callback.getToolDefinition();
            views.add(new ToolView(definition.name(), definition.description(), definition.inputSchema()));
        }
        for (Object spec : specs) {
            if (spec instanceof McpServerFeatures.SyncToolSpecification sync) {
                McpSchema.Tool tool = sync.tool();
                views.add(new ToolView(tool.name(), tool.description(), MAPPER.writeValueAsString(tool.inputSchema())));
            }
        }
        return views;
    }

    static int compactToolsListBytes(List<ToolView> tools) {
        ObjectNode root = MAPPER.createObjectNode();
        ArrayNode array = root.putArray("tools");
        List<ToolView> sorted = new ArrayList<>(tools);
        sorted.sort(Comparator.comparing(ToolView::name));
        for (ToolView tool : sorted) {
            ObjectNode entry = array.addObject();
            entry.put("name", tool.name());
            entry.put("description", tool.description() == null ? "" : tool.description());
            entry.set("inputSchema", readSchema(tool.inputSchema()));
        }
        return MAPPER.writeValueAsString(root).getBytes(StandardCharsets.UTF_8).length;
    }

    static String stripSchemaJson(String schemaJson) {
        JsonNode node = MAPPER.readTree(schemaJson);
        if (node instanceof ObjectNode object) {
            object.remove("$schema");
            return MAPPER.writeValueAsString(object);
        }
        return schemaJson;
    }

    static String validGroupNames() {
        return VALID_GROUPS;
    }

    private static List<McpServerFeatures.SyncToolSpecification> filterToolSpecs(
            @Nullable String selector, List<McpServerFeatures.SyncToolSpecification> specs) {
        Set<String> selected = selectedServiceKeys(selector);
        boolean everyService = selected.containsAll(serviceKeys());
        List<McpServerFeatures.SyncToolSpecification> exposed = new ArrayList<>();
        for (McpServerFeatures.SyncToolSpecification spec : specs) {
            String service = MCP_TOOL_SERVICES.get(spec.tool().name());
            if (service == null) {
                if (everyService) {
                    exposed.add(stripSchema(spec));
                }
                continue;
            }
            if (selected.contains(service)) {
                exposed.add(stripSchema(spec));
            }
        }
        return List.copyOf(exposed);
    }

    private static Set<String> selectedServiceKeys(@Nullable String selector) {
        if (selector == null || selector.isBlank() || "all".equalsIgnoreCase(selector.trim())) {
            return serviceKeys();
        }
        LinkedHashSet<String> selected = new LinkedHashSet<>();
        List<String> unknown = new ArrayList<>();
        for (String part : selector.split(",", -1)) {
            String entry = part.trim().toLowerCase(Locale.ROOT);
            if (entry.isEmpty()) {
                continue;
            }
            String key = ALIASES.get(entry);
            if (key == null) {
                unknown.add(entry);
            } else {
                selected.add(key);
            }
        }
        if (!unknown.isEmpty()) {
            throw new IllegalArgumentException("Unknown SHAFT_MCP_TOOLSETS group(s): "
                    + String.join(", ", unknown) + ". Valid groups: " + VALID_GROUPS);
        }
        if (selected.isEmpty()) {
            return serviceKeys();
        }
        return selected;
    }

    private static Set<String> serviceKeys() {
        LinkedHashSet<String> keys = new LinkedHashSet<>();
        for (ServiceGroup group : GROUPS) {
            keys.add(group.key());
        }
        return keys;
    }

    private static Map<String, String> aliases() {
        Map<String, String> aliases = new LinkedHashMap<>();
        for (ServiceGroup group : GROUPS) {
            for (String alias : group.aliases()) {
                aliases.put(alias, group.key());
            }
        }
        return Map.copyOf(aliases);
    }

    private static Map<String, String> mcpToolServices() {
        Map<String, String> services = new LinkedHashMap<>();
        for (ServiceGroup group : GROUPS) {
            for (Method method : group.type().getDeclaredMethods()) {
                McpTool mcpTool = method.getAnnotation(McpTool.class);
                if (mcpTool != null && !mcpTool.name().isBlank()) {
                    services.put(mcpTool.name(), group.key());
                }
            }
        }
        return Map.copyOf(services);
    }

    private static List<ToolCallback> callbacksOf(Object service) {
        return List.of(ToolCallbacks.from(service));
    }

    private static McpServerFeatures.SyncToolSpecification stripSchema(
            McpServerFeatures.SyncToolSpecification spec) {
        McpSchema.Tool tool = spec.tool();
        Map<String, Object> schema = new LinkedHashMap<>(tool.inputSchema());
        schema.remove("$schema");
        McpSchema.Tool.Builder builder = McpSchema.Tool.builder(tool.name(), schema);
        applyToolMetadata(builder, tool);
        return McpServerFeatures.SyncToolSpecification.builder()
                .tool(builder.build())
                .callHandler(spec.callHandler())
                .build();
    }

    private static void applyToolMetadata(McpSchema.Tool.Builder builder, McpSchema.Tool tool) {
        if (tool.title() != null) {
            builder.title(tool.title());
        }
        if (tool.description() != null) {
            builder.description(tool.description());
        }
        if (tool.outputSchema() != null) {
            builder.outputSchema(tool.outputSchema());
        }
        if (tool.annotations() != null) {
            builder.annotations(tool.annotations());
        }
        if (tool.meta() != null) {
            builder.meta(tool.meta());
        }
        if (tool.icons() != null) {
            builder.icons(tool.icons());
        }
    }

    private static JsonNode readSchema(String schemaJson) {
        JsonNode node = MAPPER.readTree(stripSchemaJson(schemaJson));
        return node == null ? MAPPER.createObjectNode() : node;
    }

    record ToolView(String name, @Nullable String description, String inputSchema) {
    }

    private record ServiceGroup(String key, Class<?> type, List<String> aliases) {
    }

    private static final class StrippedCallback implements ToolCallback {
        private final ToolCallback delegate;
        private final ToolDefinition definition;

        private StrippedCallback(ToolCallback delegate) {
            this.delegate = delegate;
            ToolDefinition original = delegate.getToolDefinition();
            this.definition = DefaultToolDefinition.builder()
                    .name(original.name())
                    .description(original.description())
                    .inputSchema(stripSchemaJson(original.inputSchema()))
                    .build();
        }

        @Override
        public ToolDefinition getToolDefinition() {
            return definition;
        }

        @Override
        public ToolMetadata getToolMetadata() {
            return delegate.getToolMetadata();
        }

        @Override
        public String call(String toolInput) {
            return delegate.call(toolInput);
        }

        @Override
        public String call(String toolInput, @Nullable ToolContext toolContext) {
            return delegate.call(toolInput, toolContext);
        }
    }
}
