package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAnalysisResult;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.Remediation;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.EvidenceReference;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * Builds local MCP remediation actions and safe copy-paste code blocks from Doctor evidence.
 */
final class McpDoctorRemediationService {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final int MAX_SOURCE_FILES = 5;
    private static final int MAX_SOURCE_BYTES = 20_000;
    private static final Pattern SECRET_LIKE = Pattern.compile(
            "(?i)(authorization\\s*[:=]|bearer\\s+[a-z0-9._\\-]{8,}|api[_-]?key\\s*[:=]|sk-[a-z0-9]{8,})");

    private final Function<AiRequest, AiResponse> executor;

    /**
     * Creates a remediation service using the configured Pilot provider boundary.
     */
    McpDoctorRemediationService() {
        this(new AiExecutionService()::execute);
    }

    McpDoctorRemediationService(Function<AiRequest, AiResponse> executor) {
        this.executor = executor;
    }

    /**
     * Builds deterministic and optional provider remediation.
     *
     * @param result deterministic Doctor analysis result
     * @param repositoryRoot optional repository root
     * @param allowedSourcePaths optional approved source paths
     * @param useAi whether provider fallback is requested
     * @param approvalPolicy explicit provider approval
     * @param driverVariableName Java driver variable name used in snippets
     * @return complete MCP report
     */
    McpAnalysisReport build(
            DoctorAnalysisResult result,
            Path repositoryRoot,
            List<String> allowedSourcePaths,
            boolean useAi,
            ApprovalPolicy approvalPolicy,
            String driverVariableName) {
        List<McpCodeBlock> blocks = deterministicBlocks(result.diagnosis(), driverVariableName);
        List<McpActionRecord> actions = actions(result.diagnosis(), blocks);
        ProviderBlocks providerBlocks = useAi
                ? providerBlocks(result, repositoryRoot, allowedSourcePaths, approvalPolicy, blocks)
                : new ProviderBlocks(List.of(), McpProviderFallback.disabled());
        List<McpCodeBlock> mergedBlocks = new ArrayList<>(blocks);
        mergedBlocks.addAll(providerBlocks.blocks());
        List<McpActionRecord> mergedActions = new ArrayList<>(actions);
        providerBlocks.blocks().forEach(block -> mergedActions.add(new McpActionRecord(
                "provider-" + block.id(),
                block.title(),
                "PROVIDER_ADVISORY",
                "Review the provider advisory block before applying it.",
                block.evidenceIds(),
                List.of(block.id()),
                McpActionRecord.Status.PROVIDER_ADVISORY)));
        return new McpAnalysisReport(
                McpAnalysisReport.CURRENT_SCHEMA_VERSION,
                providerBlocks.fallback().used()
                        ? McpAnalysisReport.Status.WITH_PROVIDER_ADVISORY
                        : McpAnalysisReport.Status.DETERMINISTIC,
                result.bundle().bundleId(),
                result.diagnosis().primaryCause(),
                result.diagnosis().confidence(),
                result.diagnosis().summary(),
                result.diagnosis(),
                mergedActions,
                mergedBlocks,
                providerBlocks.fallback(),
                result.bundlePath(),
                result.jsonReportPath(),
                result.markdownReportPath(),
                warnings(result, providerBlocks.fallback()));
    }

    /**
     * Builds deterministic remediation for an already loaded diagnosis.
     *
     * @param diagnosis diagnosis
     * @param driverVariableName Java driver variable name
     * @return deterministic code blocks
     */
    List<McpCodeBlock> deterministicBlocks(Diagnosis diagnosis, String driverVariableName) {
        String driver = javaIdentifierOrDefault(driverVariableName, "driver");
        List<String> evidenceIds = evidenceIds(diagnosis);
        return switch (diagnosis.primaryCause()) {
            case LOCATOR -> List.of(locatorBlock(evidenceIds), waitBlock(driver, evidenceIds, false));
            case TIMING_SYNCHRONIZATION -> List.of(waitBlock(driver, evidenceIds, true),
                    assertionBlock(driver, evidenceIds, false));
            case DATA -> List.of(dataBlock(evidenceIds));
            case ENVIRONMENT_CONFIGURATION -> List.of(setupBlock(evidenceIds));
            case PRODUCT, TEST -> List.of(assertionBlock(driver, evidenceIds, true));
            case INFRASTRUCTURE, UNKNOWN -> List.of(investigationBlock(diagnosis, evidenceIds));
        };
    }

    private List<McpActionRecord> actions(Diagnosis diagnosis, List<McpCodeBlock> blocks) {
        List<McpActionRecord> records = new ArrayList<>();
        List<String> blockIds = blocks.stream().map(McpCodeBlock::id).toList();
        for (Remediation remediation : diagnosis.remediations()) {
            records.add(new McpActionRecord(
                    remediation.id(),
                    remediation.title(),
                    diagnosis.primaryCause().name(),
                    remediation.action(),
                    remediation.evidenceIds(),
                    blockIds,
                    blockIds.isEmpty()
                            ? McpActionRecord.Status.NEEDS_EVIDENCE
                            : McpActionRecord.Status.SUGGESTED));
        }
        if (records.isEmpty()) {
            records.add(new McpActionRecord(
                    "inspect-evidence",
                    "Inspect cited evidence",
                    diagnosis.primaryCause().name(),
                    diagnosis.summary(),
                    evidenceIds(diagnosis),
                    blockIds,
                    McpActionRecord.Status.NEEDS_EVIDENCE));
        }
        return List.copyOf(records);
    }

    private ProviderBlocks providerBlocks(
            DoctorAnalysisResult result,
            Path repositoryRoot,
            List<String> allowedSourcePaths,
            ApprovalPolicy approvalPolicy,
            List<McpCodeBlock> deterministicBlocks) {
        List<String> warnings = new ArrayList<>();
        ObjectNode deterministicFallback = JSON.createObjectNode();
        deterministicFallback.put("schemaVersion", "1.0");
        deterministicFallback.set("codeBlocks", JSON.valueToTree(deterministicBlocks));
        AiRequest.Builder builder = AiRequest.builder("shaft-mcp-doctor-remediation", providerSchema())
                .text(providerPrompt(result.diagnosis()))
                .timeout(Duration.ofSeconds(30))
                .budget(new AiBudget(12_000, 2_000, BigDecimal.ZERO))
                .approvalPolicy(approvalPolicy == null ? ApprovalPolicy.denyAll() : approvalPolicy)
                .deterministicFallback(deterministicFallback);
        result.bundle().evidence().stream()
                .filter(item -> item.category() == com.shaft.doctor.model.EvidenceCategory.EXCEPTION_CHAIN
                        || item.category() == com.shaft.doctor.model.EvidenceCategory.ALLURE_RESULT
                        || item.category() == com.shaft.doctor.model.EvidenceCategory.SHAFT_LOG)
                .limit(12)
                .forEach(item -> builder.evidence(new EvidenceReference(
                        item.id(),
                        EvidenceCategory.TEXT,
                        item.mediaType(),
                        safeContent(item.content()))));
        sourceEvidence(repositoryRoot, allowedSourcePaths, warnings).forEach(builder::evidence);

        AiResponse response;
        try {
            response = executor.apply(builder.build());
        } catch (RuntimeException exception) {
            return new ProviderBlocks(List.of(), new McpProviderFallback(
                    true, false, AiResponseStatus.ERROR, "", "",
                    "AI fallback failed before returning a response.", warnings));
        }
        if (response == null || !response.successful()) {
            McpProviderFallback fallback = new McpProviderFallback(
                    true, false,
                    response == null ? AiResponseStatus.ERROR : response.status(),
                    response == null ? "" : response.provider(),
                    response == null ? "" : response.model(),
                    response == null ? "AI fallback returned no response." : response.fallbackReason(),
                    warnings);
            return new ProviderBlocks(List.of(agentDelegationBlock(result.diagnosis())), fallback);
        }
        List<McpCodeBlock> accepted = acceptedProviderBlocks(response.structuredPayload(), result.bundle(), warnings);
        return new ProviderBlocks(accepted, new McpProviderFallback(
                true,
                !accepted.isEmpty(),
                accepted.isEmpty() ? AiResponseStatus.INVALID_RESPONSE : response.status(),
                response.provider(),
                response.model(),
                accepted.isEmpty() ? "AI fallback returned no accepted cited code blocks." : "",
                warnings));
    }

    private static McpCodeBlock agentDelegationBlock(Diagnosis diagnosis) {
        return new McpCodeBlock(
                "agent-doctor-advisory",
                "Agent LLM repair handoff",
                McpCodeBlock.Kind.PROVIDER_ADVISORY,
                "text",
                List.of(),
                """
                        The configured SHAFT AI provider was unavailable, but this MCP call is an implicit agent
                        approval boundary. Use the calling agent's own LLM with the deterministic diagnosis and cited
                        evidence already returned by this tool. Propose only repository-local edits supported by cited
                        evidence, preserve existing test patterns, and keep the patch minimal.

                        Diagnosis: %s
                        """.formatted(diagnosis.summary()),
                "AI agent: inspect cited evidence and repository files, then propose the smallest supported fix.",
                true,
                evidenceIds(diagnosis),
                List.of());
    }

    private static List<EvidenceReference> sourceEvidence(
            Path repositoryRoot,
            List<String> allowedSourcePaths,
            List<String> warnings) {
        if (repositoryRoot == null || allowedSourcePaths == null || allowedSourcePaths.isEmpty()) {
            return List.of();
        }
        List<EvidenceReference> result = new ArrayList<>();
        long retained = 0;
        for (String value : allowedSourcePaths) {
            if (result.size() >= MAX_SOURCE_FILES || retained >= MAX_SOURCE_BYTES) {
                break;
            }
            Path path = repositoryRoot.resolve(value).normalize();
            try {
                byte[] bytes = Files.readAllBytes(path);
                if (bytes.length > MAX_SOURCE_BYTES - retained) {
                    warnings.add("Approved source file was omitted because it exceeded the source evidence budget.");
                    continue;
                }
                String content = new String(bytes, StandardCharsets.UTF_8);
                if (SECRET_LIKE.matcher(content).find()) {
                    warnings.add("Approved source file was omitted because it contained secret-like material.");
                    continue;
                }
                result.add(new EvidenceReference(
                        value.replace('\\', '/'),
                        EvidenceCategory.SOURCE,
                        mediaType(value),
                        content));
                retained += bytes.length;
            } catch (IOException exception) {
                warnings.add("Approved source file was omitted because it could not be read.");
            }
        }
        return List.copyOf(result);
    }

    private static List<McpCodeBlock> acceptedProviderBlocks(
            JsonNode payload,
            EvidenceBundle bundle,
            List<String> warnings) {
        Set<String> evidenceIds = bundle.evidence().stream()
                .map(EvidenceItem::id)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        List<McpCodeBlock> result = new ArrayList<>();
        JsonNode blocks = payload.path("codeBlocks");
        if (!blocks.isArray() || blocks.size() > 8) {
            warnings.add("AI fallback payload did not include a bounded codeBlocks array.");
            return List.of();
        }
        int index = 1;
        for (JsonNode block : blocks) {
            List<String> cited = textList(block.path("evidenceIds"));
            if (cited.isEmpty() || !evidenceIds.containsAll(cited)) {
                warnings.add("AI fallback block was rejected because it did not cite submitted evidence.");
                continue;
            }
            String code = block.path("code").asText("");
            if (code.isBlank() || code.length() > 8_000 || SECRET_LIKE.matcher(code).find()) {
                warnings.add("AI fallback block was rejected because its code was empty, too large, or secret-like.");
                continue;
            }
            McpCodeBlock.Kind kind = parseKind(block.path("kind").asText(""));
            result.add(new McpCodeBlock(
                    "ai-" + index++,
                    block.path("title").asText("Provider advisory"),
                    kind,
                    "java",
                    textList(block.path("imports")),
                    code,
                    block.path("placement").asText("Review before pasting into the target class."),
                    block.path("copyPasteReady").asBoolean(false),
                    cited,
                    textList(block.path("warnings"))));
        }
        return List.copyOf(result);
    }

    private static ObjectNode providerSchema() {
        ObjectNode schema = JSON.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");
        properties.putObject("schemaVersion").put("type", "string");
        ObjectNode codeBlocks = properties.putObject("codeBlocks");
        codeBlocks.put("type", "array");
        codeBlocks.put("maxItems", 8);
        ObjectNode item = codeBlocks.putObject("items");
        item.put("type", "object");
        ArrayNode required = item.putArray("required");
        required.add("title");
        required.add("kind");
        required.add("code");
        required.add("placement");
        required.add("evidenceIds");
        ObjectNode itemProperties = item.putObject("properties");
        itemProperties.putObject("title").put("type", "string");
        itemProperties.putObject("kind").put("type", "string");
        itemProperties.putObject("code").put("type", "string");
        itemProperties.putObject("placement").put("type", "string");
        itemProperties.putObject("copyPasteReady").put("type", "boolean");
        itemProperties.putObject("imports").put("type", "array");
        itemProperties.putObject("evidenceIds").put("type", "array");
        itemProperties.putObject("warnings").put("type", "array");
        return schema;
    }

    private static String providerPrompt(Diagnosis diagnosis) {
        try {
            return """
                    Produce review-only Java snippets for a SHAFT test failure.
                    Use only submitted evidence IDs. Do not invent locators, paths, classes, commands, or assertions.
                    Prefer copy-pasteable snippets for an existing class. Use placeholders only when evidence is insufficient.
                    Return JSON matching the requested schema; no Markdown fences.

                    Deterministic diagnosis:
                    %s
                    """.formatted(JSON.writeValueAsString(diagnosis));
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor diagnosis could not be serialized.", exception);
        }
    }

    private static McpCodeBlock locatorBlock(List<String> evidenceIds) {
        return new McpCodeBlock(
                "locator-review",
                "Review and replace the failing locator",
                McpCodeBlock.Kind.LOCATOR,
                "java",
                List.of("org.openqa.selenium.By", "com.shaft.driver.SHAFT"),
                """
                        // Replace the placeholder with evidence-backed attributes from the current page.
                        private static final By TARGET_ELEMENT = SHAFT.GUI.Locator.hasAnyTagName()
                                .containsText("REPLACE_WITH_STABLE_VISIBLE_TEXT")
                                .build();
                        """,
                "Paste as a field in the page object or test class, then replace the placeholder.",
                false,
                evidenceIds,
                List.of("Doctor found locator evidence, but the MCP tool does not invent a new locator."));
    }

    private static McpCodeBlock waitBlock(String driver, List<String> evidenceIds, boolean ready) {
        return new McpCodeBlock(
                "explicit-wait",
                "Add an evidence-backed explicit wait",
                McpCodeBlock.Kind.WAIT,
                "java",
                List.of("org.openqa.selenium.By", "org.openqa.selenium.support.ui.ExpectedConditions"),
                """
                        By target = TARGET_ELEMENT;
                        %s.element().waitUntil(ExpectedConditions.elementToBeClickable(target), true);
                        """.formatted(driver),
                "Paste inside the test method before interacting with the target element.",
                ready,
                evidenceIds,
                ready ? List.of() : List.of("Replace TARGET_ELEMENT with the intended locator."));
    }

    private static McpCodeBlock assertionBlock(String driver, List<String> evidenceIds, boolean ready) {
        return new McpCodeBlock(
                "state-assertion",
                "Assert the expected state before continuing",
                McpCodeBlock.Kind.ASSERTION,
                "java",
                List.of("org.openqa.selenium.By"),
                """
                        By target = TARGET_ELEMENT;
                        %s.assertThat().element(target).isVisible().perform();
                        """.formatted(driver),
                "Paste inside the test method near the failing step.",
                ready,
                evidenceIds,
                ready ? List.of() : List.of("Replace TARGET_ELEMENT with the intended locator."));
    }

    private static McpCodeBlock dataBlock(List<String> evidenceIds) {
        return new McpCodeBlock(
                "required-test-data",
                "Read required data from an explicit source",
                McpCodeBlock.Kind.SETUP,
                "java",
                List.of(),
                """
                        String value = System.getenv("REPLACE_WITH_ENVIRONMENT_VARIABLE");
                        if (value == null || value.isBlank()) {
                            throw new IllegalStateException("Missing required test data.");
                        }
                        """,
                "Paste near the setup for the failing test data dependency.",
                false,
                evidenceIds,
                List.of("Use an environment variable or SHAFT test data key that matches the real test contract."));
    }

    private static McpCodeBlock setupBlock(List<String> evidenceIds) {
        return new McpCodeBlock(
                "configuration-preflight",
                "Fail fast when required configuration is missing",
                McpCodeBlock.Kind.SETUP,
                "java",
                List.of(),
                """
                        String requiredValue = System.getenv("REPLACE_WITH_REQUIRED_ENVIRONMENT_VARIABLE");
                        if (requiredValue == null || requiredValue.isBlank()) {
                            throw new IllegalStateException("Required test environment configuration is missing.");
                        }
                        """,
                "Paste in setup before creating the driver or calling the external dependency.",
                false,
                evidenceIds,
                List.of("Replace the placeholder with the approved configuration source."));
    }

    private static McpCodeBlock investigationBlock(Diagnosis diagnosis, List<String> evidenceIds) {
        return new McpCodeBlock(
                "investigate-cited-evidence",
                "Investigation checklist",
                McpCodeBlock.Kind.INVESTIGATION,
                "text",
                List.of(),
                """
                        1. Open the cited Allure result and attachments.
                        2. Confirm the failing step, exception, and environment.
                        3. Update only the test code or locator that is directly supported by evidence.
                        4. Rerun the smallest affected test and confirm populated Allure result JSON.

                        Diagnosis: %s
                        """.formatted(diagnosis.summary()),
                "Use as a review checklist before writing code.",
                true,
                evidenceIds,
                List.of());
    }

    private static List<String> evidenceIds(Diagnosis diagnosis) {
        LinkedHashSet<String> result = new LinkedHashSet<>();
        for (Finding finding : diagnosis.findings()) {
            result.addAll(finding.evidenceIds());
        }
        for (Remediation remediation : diagnosis.remediations()) {
            result.addAll(remediation.evidenceIds());
        }
        return List.copyOf(result);
    }

    private static List<String> warnings(DoctorAnalysisResult result, McpProviderFallback fallback) {
        List<String> warnings = new ArrayList<>();
        if (result.bundle().redaction().omittedItems() > 0) {
            warnings.add(result.bundle().redaction().omittedItems()
                    + " evidence item(s) were omitted by Doctor policy or limits.");
        }
        if (!result.bundle().redaction().removedFieldNames().isEmpty()) {
            warnings.add("Doctor redacted sensitive field names from retained evidence.");
        }
        warnings.addAll(fallback.warnings());
        return List.copyOf(new LinkedHashSet<>(warnings));
    }

    private static List<String> textList(JsonNode values) {
        if (!values.isArray()) {
            return List.of();
        }
        List<String> result = new ArrayList<>();
        values.forEach(value -> {
            String text = value.asText("");
            if (!text.isBlank()) {
                result.add(text);
            }
        });
        return List.copyOf(new LinkedHashSet<>(result));
    }

    private static McpCodeBlock.Kind parseKind(String value) {
        try {
            return McpCodeBlock.Kind.valueOf(value.toUpperCase(Locale.ROOT));
        } catch (RuntimeException ignored) {
            return McpCodeBlock.Kind.PROVIDER_ADVISORY;
        }
    }

    private static String safeContent(String content) {
        if (content == null) {
            return "";
        }
        return content.length() > 8_000 ? content.substring(0, 8_000) : content;
    }

    private static String mediaType(String path) {
        String normalized = path.toLowerCase(Locale.ROOT);
        if (normalized.endsWith(".java")) {
            return "text/x-java-source";
        }
        if (normalized.endsWith(".json")) {
            return "application/json";
        }
        if (normalized.endsWith(".xml")) {
            return "application/xml";
        }
        return "text/plain";
    }

    private static String javaIdentifierOrDefault(String value, String fallback) {
        String candidate = value == null || value.isBlank() ? fallback : value.trim();
        if (!Character.isJavaIdentifierStart(candidate.charAt(0))) {
            return fallback;
        }
        for (int index = 1; index < candidate.length(); index++) {
            if (!Character.isJavaIdentifierPart(candidate.charAt(index))) {
                return fallback;
            }
        }
        return candidate;
    }

    private record ProviderBlocks(List<McpCodeBlock> blocks, McpProviderFallback fallback) {
        private ProviderBlocks {
            blocks = blocks == null ? List.of() : List.copyOf(blocks);
            fallback = fallback == null ? McpProviderFallback.disabled() : fallback;
        }
    }
}
