package com.shaft.mcp;

import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.spec.McpSchema;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.JsonNode;
import org.junit.jupiter.api.Test;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest(properties = "spring.ai.mcp.server.enabled=false")
class ShaftMcpApplicationTests {
    private static final Pattern GENERIC_PARAMETER_NAME = Pattern.compile("\"arg\\d+\"");
    private static final Set<String> GUIDANCE_AND_CODEGEN_TOOL_NAMES = Set.of(
            "shaft_guide_search",
            "test_automation_scenarios",
            "shaft_coding_partner_plan",
            "test_code_guardrails_check");

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
                .collect(Collectors.toCollection(HashSet::new));
        callbacks.stream()
                .map(ToolCallback.class::cast)
                .forEach(callback -> {
                    String schema = callback.getToolDefinition().inputSchema();
                    assertFalse(GENERIC_PARAMETER_NAME.matcher(schema).find(),
                            callback.getToolDefinition().name() + " exposes generic parameter names: " + schema);
                });

        // Tools that need a live MCP exchange (for example, to stream progress notifications) are
        // exposed via @McpTool annotation scanning instead of the plain @Tool/ToolCallback path
        // above; they register on a separate "toolSpecs" bean that the real McpSyncServer also
        // consumes (McpServerAutoConfiguration merges both lists), so the full external tool
        // catalog is the union of the two.
        for (McpServerFeatures.SyncToolSpecification toolSpecification : annotationScannedToolSpecs()) {
            McpSchema.Tool tool = toolSpecification.tool();
            assertTrue(toolNames.add(tool.name()),
                    tool.name() + " is registered both as a ToolCallback and an annotation-scanned MCP tool");
            assertNoGenericParameterNames(tool);
        }

        Set<String> expected = expectedTools();
        assertEquals(expected, toolNames);
        assertEquals(89, toolNames.size(), "the tool architecture sweep's frozen catalog is exactly 89 tools "
                + "(design doc Decision 2); update the manifest fixture deliberately, never this literal");
        assertTrue(toolNames.contains("doctor_analyze_failed_allure"));
        assertTrue(toolNames.contains("trace_latest"));
        assertTrue(toolNames.contains("trace_read"));
        assertTrue(toolNames.contains("trace_summarize"));
        assertTrue(toolNames.contains("doctor_analyze_trace"));
        assertTrue(toolNames.contains("healer_run_failed_test"));
        assertTrue(toolNames.contains("capture_generate_replay"));
        assertTrue(toolNames.contains("capture_checkpoint"));
        assertTrue(toolNames.contains("browser_get_page_dom"));
        assertTrue(toolNames.contains("browser_open_intent"));
        assertTrue(toolNames.contains("browser_take_screenshot"));
        assertTrue(toolNames.contains("driver_initialize"));
        assertTrue(toolNames.contains("driver_quit"));
        assertTrue(toolNames.contains("element_click"));
        assertTrue(toolNames.contains("element_type"));
        assertTrue(toolNames.contains("element_upload_file"));
        assertTrue(toolNames.contains("browser_delete_cookies"));
        assertTrue(toolNames.contains("mobile_swipe"));
        assertTrue(toolNames.contains("mobile_get_contexts"));
        assertTrue(toolNames.contains("mobile_get_accessibility_tree"));
        assertTrue(toolNames.contains("mobile_inspector_record_start"));
        assertTrue(toolNames.contains("mobile_inspector_record_status"));
        assertTrue(toolNames.contains("mobile_inspector_record_stop"));
        assertTrue(toolNames.contains("capture_api_start"));
        assertTrue(toolNames.contains("capture_api_status"));
        assertTrue(toolNames.contains("capture_api_stop"));
        assertTrue(toolNames.contains("capture_api_transactions"));
        assertTrue(toolNames.contains("shaft_guide_search"));
        assertTrue(toolNames.contains("shaft_project_create"));
        assertTrue(toolNames.contains("shaft_project_upgrade"));
        assertTrue(toolNames.contains("test_automation_scenarios"));
        assertTrue(toolNames.contains("test_code_guardrails_check"));
        assertTrue(toolNames.contains("autobot_local_agent_clients"));
        assertTrue(toolNames.contains("autobot_local_agent_run"));
        assertTrue(toolNames.contains("autobot_provider_status"));
        assertTrue(toolNames.contains("shaft_coding_partner_diff"));
        assertTrue(toolNames.contains("verify_run_focused"));
        assertFalse(toolNames.contains("doctor_publish_draft_pr"));
        assertFalse(toolNames.contains("browser_get_page_source"));
        assertFalse(toolNames.contains("browser_get_cookie"));
        assertFalse(toolNames.contains("element_click_semantic"));
        assertFalse(toolNames.contains("element_type_semantic"));
        assertFalse(toolNames.contains("element_click_ai"));

        // Deleted outright (owner mandate, no re-export) -- design doc Decision 2 "Deleted outright".
        assertFalse(toolNames.contains("natural_act"));
        assertFalse(toolNames.contains("mobile_natural_act"));
        // Absorbed into unified element_*/browser_*/capture_*/doctor_*/healer_*/mobile_swipe tools --
        // no playwright_*-prefixed tool survives, and no per-engine or JS/append click/type variant does.
        assertTrue(toolNames.stream().noneMatch(name -> name.startsWith("playwright_")),
                "no playwright_-prefixed tool may survive (design doc Decision 2): " + toolNames);
        assertFalse(toolNames.contains("element_click_js"));
        assertFalse(toolNames.contains("element_double_click"));
        assertFalse(toolNames.contains("element_click_and_hold"));
        assertFalse(toolNames.contains("element_append_text"));
        assertFalse(toolNames.contains("element_set_value_js"));
        assertFalse(toolNames.contains("element_drag_and_drop_by_offset"));
        assertFalse(toolNames.contains("element_drop_file_to_upload"));
        assertFalse(toolNames.contains("browser_maximize_window"));
        assertFalse(toolNames.contains("browser_fullscreen_window"));
        assertFalse(toolNames.contains("browser_delete_cookie"));
        assertFalse(toolNames.contains("browser_delete_all_cookies"));
        assertFalse(toolNames.contains("browser_network_request"));
        assertFalse(toolNames.contains("capture_start_codegen"));
        assertFalse(toolNames.contains("mobile_initialize_native"));
        assertFalse(toolNames.contains("mobile_initialize_web_emulation"));
        assertFalse(toolNames.contains("mobile_tap"));
        assertFalse(toolNames.contains("mobile_double_tap"));
        assertFalse(toolNames.contains("mobile_long_tap"));
        assertFalse(toolNames.contains("mobile_type"));
        assertFalse(toolNames.contains("mobile_clear"));
        assertFalse(toolNames.contains("mobile_take_screenshot"));
        assertFalse(toolNames.contains("mobile_swipe_by_offset"));
        assertFalse(toolNames.contains("mobile_swipe_coordinates"));
        assertFalse(toolNames.contains("mobile_swipe_element_into_view"));
        assertFalse(toolNames.contains("mobile_swipe_text_into_view"));
        assertFalse(toolNames.contains("mobile_record_start"));
        assertFalse(toolNames.contains("mobile_record_status"));
        assertFalse(toolNames.contains("mobile_record_stop"));
        assertFalse(toolNames.contains("mobile_step_delete"));
        assertFalse(toolNames.contains("mobile_step_reorder"));
        assertFalse(toolNames.contains("mobile_recording_code_blocks"));
        assertFalse(toolNames.contains("mobile_record_at_target_code_blocks"));
        assertFalse(toolNames.contains("mobile_replay_recording"));
        assertFalse(toolNames.contains("mobile_api_record_start"));
        assertFalse(toolNames.contains("mobile_api_record_status"));
        assertFalse(toolNames.contains("mobile_api_record_stop"));
        assertFalse(toolNames.contains("mobile_api_record_transactions"));
        assertFalse(toolNames.contains("mobile_inspector_record_prepare"));
        assertFalse(toolNames.contains("mobile_inspector_record_control"));
    }

    @Test
    void shaftToolsBeanAlwaysRegistersGuidanceAndCodegenTools() {
        Object bean = context.getBean("shaftTools");
        assertTrue(bean instanceof List<?>);
        List<?> callbacks = (List<?>) bean;

        Set<String> toolNames = callbacks.stream()
                .map(ToolCallback.class::cast)
                .map(callback -> callback.getToolDefinition().name())
                .collect(Collectors.toSet());

        Set<String> missing = GUIDANCE_AND_CODEGEN_TOOL_NAMES.stream()
                .filter(name -> !toolNames.contains(name))
                .collect(Collectors.toSet());
        assertTrue(missing.isEmpty(),
                "shaftTools bean is missing guidance/codegen tools independent of launch directory: " + missing);
    }

    @Test
    void captureLaunchPrefixBuildsRunnableCaptureCommand() throws Exception {
        Method method = ShaftMcpApplication.class.getDeclaredMethod("captureLaunchPrefix");
        method.setAccessible(true);

        @SuppressWarnings("unchecked")
        List<String> prefix = (List<String>) method.invoke(null);

        assertTrue(prefix.size() >= 2);
        String lastArgument = prefix.get(prefix.size() - 1);
        if (lastArgument.startsWith("@")) {
            String argsFileContent = Files.readString(Path.of(lastArgument.substring(1)));
            assertTrue(argsFileContent.contains(com.shaft.capture.cli.CaptureCli.class.getName()));
        } else {
            assertTrue(prefix.contains("capture")
                    || prefix.contains(com.shaft.capture.cli.CaptureCli.class.getName()));
        }
    }

    @Test
    void captureLaunchPrefixArgFileFallbackKeepsCommandLineShortOnLargeClasspaths() throws Exception {
        Method method = ShaftMcpApplication.class.getDeclaredMethod(
                "captureLaunchPrefix", String[].class, String.class);
        method.setAccessible(true);

        String largeClassPath = java.util.stream.IntStream.range(0, 400)
                .mapToObj(index -> "target/dependency/library-" + index + ".jar")
                .collect(Collectors.joining(File.pathSeparator));

        // The launch inputs are explicit because this JVM's own launch shape is unreliable:
        // Surefire forks tests through a manifest-only booter jar (-jar). A plain multi-entry
        // classpath launch (dev/IDE runs) must hit the @argfile fallback -- the same one Windows
        // CreateProcess rejects at ~32K chars when it is passed inline instead, per
        // gotcha.windows-jvm-launchers-must-pass-large-classpaths-via-a-java-argfile.
        @SuppressWarnings("unchecked")
        List<String> prefix = (List<String>) method.invoke(null, new String[0], largeClassPath);

        String lastArgument = prefix.get(prefix.size() - 1);
        assertTrue(lastArgument.startsWith("@"), "expected an @argfile launch, got: " + prefix);
        int totalCommandLineLength = String.join(" ", prefix).length();
        assertTrue(totalCommandLineLength < 1_000,
                "relaunch command line should stay short regardless of classpath size: " + totalCommandLineLength);

        String argsFileContent = Files.readString(Path.of(lastArgument.substring(1)));
        assertTrue(argsFileContent.contains("-cp"));
        assertTrue(argsFileContent.contains(com.shaft.capture.cli.CaptureCli.class.getName()));
    }

    /**
     * When the current process was itself launched with {@code -jar some.jar}, the relaunch prefix
     * reuses that exact jar instead of falling through to the classpath-based branches -- this is
     * the branch a real {@code -jar}-launched JVM (a packaged shaft-mcp release) takes, which
     * {@link #captureLaunchPrefixBuildsRunnableCaptureCommand} cannot reliably exercise since
     * {@code ProcessHandle.info().arguments()} often comes back empty on this OS/JDK.
     */
    @Test
    void captureLaunchPrefixReusesTheDashJarArgumentWhenThisProcessWasJarLaunched() throws Exception {
        Method method = ShaftMcpApplication.class.getDeclaredMethod(
                "captureLaunchPrefix", String[].class, String.class);
        method.setAccessible(true);

        String[] processArguments = {"-Xmx512m", "-jar", "shaft-mcp-launcher.jar", "--server"};

        @SuppressWarnings("unchecked")
        List<String> prefix = (List<String>) method.invoke(null, processArguments, "ignored-classpath.jar");

        assertEquals(List.of(prefix.get(0), "-jar", "shaft-mcp-launcher.jar", "capture"), prefix);
    }

    /**
     * When this JVM's own classpath is a single {@code .jar} file (the shape Surefire's
     * manifest-only booter jar can also produce), the relaunch prefix reuses that jar directly
     * instead of falling through to the {@code @argfile} branch.
     */
    @Test
    void captureLaunchPrefixReusesASingleJarClasspathEntry(@org.junit.jupiter.api.io.TempDir Path tempDir)
            throws Exception {
        Method method = ShaftMcpApplication.class.getDeclaredMethod(
                "captureLaunchPrefix", String[].class, String.class);
        method.setAccessible(true);

        Path jar = Files.createFile(tempDir.resolve("single-entry.jar"));

        @SuppressWarnings("unchecked")
        List<String> prefix = (List<String>) method.invoke(null, new String[0], jar.toString());

        assertEquals(List.of(prefix.get(0), "-jar", jar.toString(), "capture"), prefix);
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

    /**
     * Tools annotated {@code @McpTool} (for example, {@code capture_generate_replay}, which needs
     * a live {@code McpSyncServerExchange} to stream progress notifications) register on the
     * "toolSpecs" bean built by Spring AI's annotation-scanning MCP auto-configuration, not on the
     * "shaftTools" {@code List<ToolCallback>} bean. That auto-configuration only requires
     * {@code spring.ai.mcp.server.annotation-scanner.enabled} (defaults true) and is unaffected by
     * this test class's {@code spring.ai.mcp.server.enabled=false}, so the bean is present here too.
     */
    @SuppressWarnings("unchecked")
    private List<McpServerFeatures.SyncToolSpecification> annotationScannedToolSpecs() {
        return (List<McpServerFeatures.SyncToolSpecification>) context.getBean("toolSpecs", List.class);
    }

    private static void assertNoGenericParameterNames(McpSchema.Tool tool) {
        Object properties = tool.inputSchema().get("properties");
        if (!(properties instanceof Map<?, ?> propertiesMap)) {
            return;
        }
        Set<String> generic = propertiesMap.keySet().stream()
                .map(Object::toString)
                .filter(name -> name.matches("arg\\d+"))
                .collect(Collectors.toSet());
        assertTrue(generic.isEmpty(), tool.name() + " exposes generic parameter names: " + generic);
    }

    /**
     * Issue #3692 acceptance: every onboarding quick-action call must succeed with no fields at all.
     * {@code capture_start} previously served a schema that marked {@code sessionGoal} (and, until
     * this fix, {@code headless}) as required, so the IntelliJ onboarding "Record a sample flow"
     * action failed with "required property 'sessionGoal' not found" the moment the caller omitted
     * it. This asserts the live-served schema -- not just the source annotations -- has an empty
     * {@code required} array, matching the "no sessionGoal and no other fields" acceptance bar.
     */
    @Test
    void captureStartSchemaHasNoRequiredFieldsSoOnboardingQuickActionCallsSucceed() throws Exception {
        JsonNode schema = toolInputSchema("capture_start");

        List<String> requiredNames = new java.util.ArrayList<>();
        for (JsonNode node : schema.path("required")) {
            requiredNames.add(node.asText());
        }

        assertTrue(requiredNames.isEmpty(),
                "capture_start schema still requires: " + requiredNames
                        + " -- a bare capture_start call with no arguments must succeed schema validation");
        assertTrue(schema.path("properties").has("sessionGoal"));
        assertTrue(schema.path("properties").has("headless"));
    }

    /**
     * Issue #3692 item 2: the codegen tools' {@code sessionPath} must not be a required schema
     * field once the server-side "default to the latest recording" fallback exists ({@link
     * com.shaft.mcp.CaptureService#resolveSessionPath}) -- otherwise a schema-validating client
     * still refuses to send the call before the fallback ever runs.
     */
    @Test
    @SuppressWarnings("unchecked")
    void codegenToolsSessionPathIsNotRequiredSoOmittingItDefersToTheLatestRecording() throws Exception {
        assertFalse(sessionPathRequired(toolInputSchema("capture_code_blocks")),
                "capture_code_blocks still requires sessionPath");
        assertFalse(sessionPathRequired(toolInputSchema("capture_record_at_target_code_blocks")),
                "capture_record_at_target_code_blocks still requires sessionPath");

        McpSchema.Tool captureGenerateReplay = annotationScannedToolSpecs().stream()
                .filter(spec -> spec.tool().name().equals("capture_generate_replay"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("capture_generate_replay is not annotation-scanned"))
                .tool();
        List<String> required = (List<String>) captureGenerateReplay.inputSchema().get("required");
        assertFalse(required.contains("sessionPath"),
                "capture_generate_replay (the @McpTool-annotated overload) still requires sessionPath: " + required);
    }

    @SuppressWarnings("unchecked")
    private JsonNode toolInputSchema(String toolName) throws Exception {
        Object bean = context.getBean("shaftTools");
        List<?> callbacks = (List<?>) bean;
        ToolCallback callback = callbacks.stream()
                .map(ToolCallback.class::cast)
                .filter(candidate -> candidate.getToolDefinition().name().equals(toolName))
                .findFirst()
                .orElseThrow(() -> new AssertionError(toolName + " is not registered on the shaftTools bean"));
        return new ObjectMapper().readTree(callback.getToolDefinition().inputSchema());
    }

    private static boolean sessionPathRequired(JsonNode schema) {
        for (JsonNode required : schema.path("required")) {
            if ("sessionPath".equals(required.asText())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Issue #3899 regression: {@code driver_initialize}'s mobile fields (former
     * {@code mobile_initialize_web_emulation}/{@code mobile_initialize_native} parameters) moved
     * under a nested {@code mobileOptions} object when commit 22bc611ec7 absorbed those tools
     * (design doc amendment A9). The IntelliJ plugin's {@code GuidedWorkflowPanel.mobileWebEmulation()}
     * kept sending them flat at the top level, which the live-served schema rejects
     * ("property 'targetUrl' is not defined in the schema and the schema does not allow additional
     * properties", ...): this asserts the served schema shape directly so that drift is caught by a
     * fast in-repo test instead of only the nightly live E2E run.
     */
    @Test
    void driverInitializeSchemaNestsMobileFieldsUnderMobileOptionsNotAtTheTopLevel() throws Exception {
        JsonNode schema = toolInputSchema("driver_initialize");
        JsonNode properties = schema.path("properties");

        assertTrue(properties.has("mobileOptions"),
                "driver_initialize schema must expose a nested mobileOptions object: " + schema);
        assertFalse(properties.has("targetUrl"),
                "targetUrl must live under mobileOptions, not top-level: " + schema);
        assertFalse(properties.has("browser") && properties.path("browser").path("type").asText().equals("string")
                        && properties.has("deviceName"),
                "web-emulation fields must live under mobileOptions, not top-level: " + schema);

        JsonNode mobileOptionsProperties = properties.path("mobileOptions").path("properties");
        assertTrue(mobileOptionsProperties.has("targetUrl"), "mobileOptions must expose targetUrl: " + schema);
        assertTrue(mobileOptionsProperties.has("deviceName"), "mobileOptions must expose deviceName: " + schema);
        assertTrue(mobileOptionsProperties.has("headless"), "mobileOptions must expose headless: " + schema);
    }

    /**
     * Issue #3899: confirms the wire-format casing {@code driver_initialize}'s {@code engine}
     * selector actually validates against, so plugin/tool callers don't have to guess between the
     * Java enum constant name ({@code MOBILE_WEB}) and the lowercase form used in prose elsewhere
     * (for example {@link ActiveEngine}'s own javadoc says {@code engine=mobile_web}).
     */
    @Test
    void driverInitializeEngineEnumWireCasingMatchesJavaConstantNames() throws Exception {
        JsonNode schema = toolInputSchema("driver_initialize");
        JsonNode engineEnum = schema.path("properties").path("engine").path("enum");

        List<String> allowed = new java.util.ArrayList<>();
        engineEnum.forEach(node -> allowed.add(node.asText()));

        assertEquals(List.of("NONE", "WEB", "MOBILE_NATIVE", "MOBILE_WEB", "PLAYWRIGHT"), allowed,
                "driver_initialize's engine enum wire values: " + schema);
    }

    /**
     * Issue #3908 live-repro finding: {@code McpMobileInitOptions}'s javadoc says "Every field is
     * optional" and every field is genuinely optional at the Java level (all boxed/nullable, each
     * defaulted internally by {@code MobileService#initializeWebEmulation}/{@code #initializeNative}),
     * but the live-served schema marked all 9 native-only fields required -- confirmed live by 3/3
     * "Guided Workflows Live E2E" workflow_dispatch runs (29808732349, 29808737368, 29808742128)
     * failing {@code mobileRecorderRecordsEmulatedActionsAndGeneratesShaftCode} with "JSON schema
     * validation errors: [/mobileOptions: required property 'app' not found, ...]" the moment a
     * MOBILE_WEB caller (which never sends the native-only fields) reached real schema validation --
     * masked until #3906 fixed the plugin's flat-vs-nested shape bug that failed validation earlier
     * for an unrelated reason. {@code McpMobileInitOptions} is the only record type used as a nested
     * {@code @ToolParam} request object anywhere in shaft-mcp; the sibling pattern
     * ({@code CaptureCodegenStartRequest}) is a plain mutable class, which Spring AI's schema
     * generator treats as all-optional by default -- records don't get that default.
     */
    @Test
    void driverInitializeMobileOptionsSchemaHasNoRequiredFieldsSoAnyEngineCanOmitTheOthers() throws Exception {
        JsonNode schema = toolInputSchema("driver_initialize");
        JsonNode mobileOptionsRequired = schema.path("properties").path("mobileOptions").path("required");

        List<String> requiredNames = new java.util.ArrayList<>();
        mobileOptionsRequired.forEach(node -> requiredNames.add(node.asText()));

        assertTrue(requiredNames.isEmpty(),
                "mobileOptions schema still requires: " + requiredNames
                        + " -- a MOBILE_WEB caller that never sends native-only fields (or vice versa) "
                        + "must still pass schema validation: " + schema);
    }
}
