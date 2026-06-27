package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.generate.CaptureTargetInsertionPlan;
import com.shaft.capture.generate.CaptureTargetInsertionPlanner;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Extracts reusable code blocks from deterministic Capture generation output.
 */
final class McpCaptureCodeBlockService {
    private static final Pattern DRIVER_TOKEN = Pattern.compile("\\bdriver\\b");
    private static final Pattern LOCATOR_FIELD = Pattern.compile(
            "\\s*(?:private|protected|public)?\\s*(?:static\\s+)?final\\s+By\\s+"
                    + "([A-Za-z_$][\\w$]*)\\s*=\\s*(.+);\\s*");
    private static final Pattern METHOD_NAME = Pattern.compile("\\bvoid\\s+([A-Za-z_$][\\w$]*)\\s*\\(");
    private static final Pattern DRIVER_TYPE = Pattern.compile("\\bSHAFT\\.GUI\\.(WebDriver|Playwright)\\s+driver\\s*;");
    private static final Set<String> DIRECT_BY_STRATEGIES = Set.of(
            "CSS", "TEST_ID", "XPATH", "ID", "NAME", "CLASS_NAME", "TAG_NAME",
            "LINK_TEXT", "PARTIAL_LINK_TEXT");

    /**
     * Extracts full-class and test-method snippets from a generated Java source file.
     *
     * @param sourcePath generated Java source
     * @param driverVariableName desired driver variable name for method snippets
     * @return MCP code blocks
     */
    List<McpCodeBlock> fromGeneratedSource(Path sourcePath, String driverVariableName) {
        return fromGeneratedSource(sourcePath, driverVariableName, null);
    }

    /**
     * Extracts full-class, test-method, and Page Object insertion guidance from generated source.
     *
     * @param sourcePath generated Java source
     * @param driverVariableName desired driver variable name for method snippets
     * @param report optional structured generation report
     * @return MCP code blocks
     */
    List<McpCodeBlock> fromGeneratedSource(
            Path sourcePath,
            String driverVariableName,
            CaptureGenerationReport report) {
        return fromGeneratedSource(sourcePath, driverVariableName, report, null, "");
    }

    /**
     * Extracts generated code blocks plus focused record-at-target snippets.
     *
     * @param sourcePath generated Java source
     * @param driverVariableName desired driver variable name for method snippets
     * @param report optional structured generation report
     * @param targetSource existing Java source selected by the user
     * @param insertAfter method name or textual anchor to insert after
     * @return MCP code blocks
     */
    List<McpCodeBlock> fromGeneratedSource(
            Path sourcePath,
            String driverVariableName,
            CaptureGenerationReport report,
            Path targetSource,
            String insertAfter) {
        try {
            String source = Files.readString(sourcePath, StandardCharsets.UTF_8);
            List<String> imports = imports(source);
            String driver = javaIdentifierOrDefault(driverVariableName, "driver");
            String driverType = driverType(source);
            List<McpCodeBlock> blocks = new ArrayList<>();
            blocks.add(new McpCodeBlock(
                    "capture-full-class",
                    "Generated SHAFT replay class",
                    McpCodeBlock.Kind.FULL_CLASS,
                    "java",
                    imports,
                    source,
                    "Use as a generated TestNG class or copy selected methods into your suite.",
                    true,
                    List.of(),
                    List.of()));
            String method = testMethod(source);
            if (!method.isBlank()) {
                blocks.add(new McpCodeBlock(
                        "capture-test-method",
                        "Generated replay method body",
                        McpCodeBlock.Kind.TEST_METHOD,
                        "java",
                        imports,
                        DRIVER_TOKEN.matcher(method).replaceAll(driver),
                        "Paste into an existing class that already owns a " + driverType + " named " + driver + ".",
                        true,
                        List.of(),
                        List.of("Generated helper fields or test data references may also be needed.")));
            }
            PomCandidates pom = pomCandidates(source, method, driver, report);
            if (!pom.locators().isEmpty()) {
                blocks.add(locatorInventoryBlock(imports, pom.locators()));
            }
            if (!pom.actions().isEmpty()) {
                blocks.add(actionSequenceBlock(pom.actions()));
            }
            if (!pom.locators().isEmpty() || !pom.actions().isEmpty()) {
                blocks.add(pomInsertionGuideBlock(driver, driverType, pom));
            } else {
                blocks.add(manualMappingWarningBlock());
            }
            if (targetSource != null || (insertAfter != null && !insertAfter.isBlank())) {
                blocks.addAll(targetInsertionBlocks(sourcePath, targetSource, insertAfter, driver));
            }
            blocks.add(agentIntegrationBlock());
            return List.copyOf(blocks);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Generated Capture source could not be read.", exception);
        }
    }

    private static List<McpCodeBlock> targetInsertionBlocks(
            Path sourcePath,
            Path targetSource,
            String insertAfter,
            String driver) {
        CaptureTargetInsertionPlan plan = new CaptureTargetInsertionPlanner()
                .plan(sourcePath, targetSource, insertAfter, driver);
        List<McpCodeBlock> blocks = new ArrayList<>();
        if (!plan.locatorFields().isBlank()) {
            blocks.add(new McpCodeBlock(
                    "capture-target-locator-fields",
                    "Record-at-target locator fields",
                    McpCodeBlock.Kind.LOCATOR,
                    "java",
                    plan.imports(),
                    plan.locatorFields(),
                    "Paste into " + plan.targetSource().getFileName()
                            + " near the existing locator fields before adding the action snippet.",
                    plan.anchorFound(),
                    List.of(),
                    plan.warnings()));
        }
        if (!plan.actionSnippet().isBlank()) {
            blocks.add(new McpCodeBlock(
                    "capture-target-action-snippet",
                    "Record-at-target action snippet",
                    McpCodeBlock.Kind.ACTION,
                    "java",
                    List.of(),
                    plan.actionSnippet(),
                    plan.placement(),
                    plan.anchorFound(),
                    List.of(),
                    plan.warnings()));
        }
        blocks.add(new McpCodeBlock(
                "capture-target-insertion-guide",
                "Record-at-target insertion guide",
                McpCodeBlock.Kind.PROVIDER_ADVISORY,
                "text",
                List.of(),
                """
                        record-at-target insertion map
                        Target source: %s
                        Insert after: %s
                        Add imports and locator fields from capture-target-locator-fields when the target class does not already define them.
                        Paste capture-target-action-snippet at the anchor, then adapt helper/data calls to local suite conventions.
                        No source file was edited by this workflow.
                        """.formatted(plan.targetSource(), plan.insertAfter()),
                "Use these blocks as an approved edit plan for the selected source target.",
                plan.anchorFound(),
                List.of(),
                plan.warnings()));
        return blocks;
    }

    private static McpCodeBlock agentIntegrationBlock() {
        return new McpCodeBlock(
                "capture-agent-integration",
                "Agent repository integration plan",
                McpCodeBlock.Kind.PROVIDER_ADVISORY,
                "text",
                List.of(),
                """
                        The calling AI agent may use its own LLM and repository access to integrate this capture.
                        After capture_stop, present the generated result and ask the user to choose either a Java
                        snippet or repository insertion. Snippet mode should show the capture-full-class block as a
                        fenced java class with imports, locator fields, setup, actions, and teardown.
                        If the project has Page Object classes, move stable locator fields into the matching page class,
                        place the action lines into page methods, and keep the test method as orchestration only.
                        Preserve repository instructions, naming, setup/teardown, and existing test data conventions.
                        Apply the smallest source edit that compiles; do not paste a generic generated class when a
                        local Page Object pattern is already present.
                        """,
                "AI agent: inspect the current codebase, then insert the generated code into the most relevant classes.",
                true,
                List.of(),
                List.of());
    }

    private static McpCodeBlock locatorInventoryBlock(List<String> imports, List<LocatorCandidate> locators) {
        StringBuilder code = new StringBuilder();
        for (LocatorCandidate locator : locators) {
            if (!locator.description().isBlank()) {
                code.append("// ").append(locator.description()).append('\n');
            }
            code.append("private final By ")
                    .append(locator.suggestedFieldName())
                    .append(" = ")
                    .append(locator.expression())
                    .append(";\n");
        }
        return new McpCodeBlock(
                "capture-pom-locator-inventory",
                "Page Object locator inventory",
                McpCodeBlock.Kind.LOCATOR,
                "java",
                locatorImports(imports, code.toString()),
                code.toString(),
                "Paste these candidates as fields in the matching page class, then align names with local conventions.",
                false,
                evidenceIds(locators),
                List.of());
    }

    private static McpCodeBlock actionSequenceBlock(List<ActionCandidate> actions) {
        StringBuilder code = new StringBuilder();
        String currentFlow = "";
        for (ActionCandidate action : actions) {
            if (!action.flow().equals(currentFlow)) {
                currentFlow = action.flow();
                code.append("// Flow: ").append(currentFlow).append('\n');
            }
            code.append(action.line()).append('\n');
        }
        return new McpCodeBlock(
                "capture-pom-action-sequence",
                "Page Object action sequence",
                McpCodeBlock.Kind.ACTION,
                "java",
                List.of(),
                code.toString(),
                "Paste action lines into intent-named page methods; keep orchestration in tests.",
                false,
                List.of(),
                List.of());
    }

    private static McpCodeBlock pomInsertionGuideBlock(
            String driver,
            String driverType,
            PomCandidates pom) {
        String locatorSummary = pom.locators().isEmpty()
                ? "No concrete locator candidates were extracted."
                : "Use capture-pom-locator-inventory for " + pom.locators().size() + " locator candidate(s).";
        String actionSummary = pom.actions().isEmpty()
                ? "No concrete action lines were extracted."
                : "Use capture-pom-action-sequence for " + pom.actions().size() + " action/checkpoint line(s).";
        return new McpCodeBlock(
                "capture-pom-insertion-guide",
                "Capture-to-POM insertion guide",
                McpCodeBlock.Kind.PROVIDER_ADVISORY,
                "text",
                List.of(),
                """
                        Capture-to-POM insertion map
                        Locator fields -> page class: %s Paste stable locator candidates into the matching Page Object.
                        Action lines -> page methods: %s Move replay actions into intent-named methods that own a %s named %s.
                        Orchestration -> tests: keep the generated test method as scenario flow only; call page methods from tests.
                        Preserve repository setup, teardown, data loading, assertions, and naming conventions.
                        Do not auto-edit user repositories from MCP output; use these blocks as deterministic guidance.
                        """.formatted(locatorSummary, actionSummary, driverType, driver),
                "Use before editing repository files so generated replay code lands in Page Object seams.",
                false,
                List.of(),
                List.of());
    }

    private static McpCodeBlock manualMappingWarningBlock() {
        return new McpCodeBlock(
                "capture-pom-manual-mapping-warning",
                "Manual Page Object mapping required",
                McpCodeBlock.Kind.PROVIDER_ADVISORY,
                "text",
                List.of(),
                """
                        Capture-to-POM extraction did not find concrete locator or action candidates.
                        Keep the generated full-class and method snippets, then manually map replay steps into
                        the nearest Page Object fields, page methods, and test orchestration.
                        """,
                "Review the generated class manually before repository insertion.",
                false,
                List.of(),
                List.of("POM locator/action extraction found no concrete candidates; manual mapping is required."));
    }

    private static List<String> imports(String source) {
        return source.lines()
                .filter(line -> line.startsWith("import "))
                .map(line -> line.substring("import ".length(), line.length() - 1))
                .toList();
    }

    private static PomCandidates pomCandidates(
            String source,
            String method,
            String driver,
            CaptureGenerationReport report) {
        List<CaptureGenerationReport.LocatorDecision> decisions = report == null
                ? List.of()
                : report.locatorDecisions();
        return new PomCandidates(
                locatorCandidates(source, method, decisions),
                actionCandidates(method, driver));
    }

    private static List<LocatorCandidate> locatorCandidates(
            String source,
            String method,
            List<CaptureGenerationReport.LocatorDecision> decisions) {
        Map<String, LocatorCandidate> locators = new LinkedHashMap<>();
        for (String line : source.lines().toList()) {
            Matcher matcher = LOCATOR_FIELD.matcher(line);
            if (matcher.matches()) {
                String sourceName = matcher.group(1);
                String expression = stripTrailingSemicolon(matcher.group(2).trim());
                Optional<CaptureGenerationReport.LocatorDecision> decision = matchingDecision(expression, decisions);
                locators.putIfAbsent(expression, new LocatorCandidate(
                        lowerCamel(sourceName),
                        expression,
                        evidenceIds(decision),
                        description(sourceName, decision.orElse(null))));
            }
        }
        for (String line : method.lines().toList()) {
            for (String expression : locatorExpressions(line)) {
                if (locators.containsKey(expression)) {
                    continue;
                }
                Optional<CaptureGenerationReport.LocatorDecision> decision = matchingDecision(expression, decisions);
                String name = decision
                        .map(value -> suggestedFieldName(value.logicalElementId()))
                        .orElseGet(() -> suggestedFieldName(expressionName(expression)));
                locators.put(expression, new LocatorCandidate(
                        name,
                        expression,
                        evidenceIds(decision),
                        description("", decision.orElse(null))));
            }
        }
        for (CaptureGenerationReport.LocatorDecision decision : decisions) {
            boolean alreadyRepresented = locators.values().stream()
                    .anyMatch(locator -> locator.evidenceIds().containsAll(decision.eventIds()));
            if (!alreadyRepresented) {
                String expression = reportLocatorExpression(decision);
                locators.putIfAbsent("report:" + decision.logicalElementId(), new LocatorCandidate(
                        suggestedFieldName(decision.logicalElementId()),
                        expression,
                        decision.eventIds(),
                        description("", decision)));
            }
        }
        return List.copyOf(locators.values());
    }

    private static List<ActionCandidate> actionCandidates(String method, String driver) {
        if (method.isBlank()) {
            return List.of();
        }
        String flow = methodName(method);
        List<ActionCandidate> actions = new ArrayList<>();
        List<String> lines = method.lines().toList();
        for (int index = 1; index < lines.size(); index++) {
            if (index == lines.size() - 1 && lines.get(index).trim().equals("}")) {
                continue;
            }
            String line = lines.get(index);
            String trimmed = line.trim();
            if (trimmed.isBlank()) {
                continue;
            } else if (trimmed.startsWith("// Recorded checkpoint ")) {
                actions.add(new ActionCandidate(
                        flow,
                        "// Checkpoint: " + trimmed.substring("// Recorded checkpoint ".length())));
            } else {
                actions.add(new ActionCandidate(
                        flow,
                        DRIVER_TOKEN.matcher(trimmed).replaceAll(driver)));
            }
        }
        return List.copyOf(actions);
    }

    private static List<String> locatorExpressions(String line) {
        List<String> expressions = new ArrayList<>();
        int index = 0;
        while (index < line.length()) {
            int shaft = line.indexOf("SHAFT.GUI.Locator.", index);
            int by = line.indexOf("By.", index);
            int start = nextStart(shaft, by);
            if (start < 0) {
                break;
            }
            int end = expressionEnd(line, start);
            if (end <= start) {
                index = start + 1;
                continue;
            }
            expressions.add(line.substring(start, end).trim());
            index = end;
        }
        return List.copyOf(expressions);
    }

    private static int nextStart(int first, int second) {
        if (first < 0) {
            return second;
        }
        if (second < 0) {
            return first;
        }
        return Math.min(first, second);
    }

    private static int expressionEnd(String line, int start) {
        int depth = 0;
        boolean string = false;
        boolean escaped = false;
        for (int index = start; index < line.length(); index++) {
            char character = line.charAt(index);
            if (string) {
                if (escaped) {
                    escaped = false;
                } else if (character == '\\') {
                    escaped = true;
                } else if (character == '"') {
                    string = false;
                }
                continue;
            }
            if (character == '"') {
                string = true;
            } else if (character == '(') {
                depth++;
            } else if (character == ')') {
                depth--;
                if (depth == 0) {
                    int next = nextNonWhitespace(line, index + 1);
                    if (next < line.length() && line.charAt(next) == '.') {
                        index = next;
                    } else {
                        return index + 1;
                    }
                }
            } else if ((character == ',' || character == ';') && depth == 0) {
                return index;
            }
        }
        return line.length();
    }

    private static int nextNonWhitespace(String line, int start) {
        int index = start;
        while (index < line.length() && Character.isWhitespace(line.charAt(index))) {
            index++;
        }
        return index;
    }

    private static String testMethod(String source) {
        List<String> lines = source.lines().toList();
        for (int index = 0; index < lines.size(); index++) {
            if (!lines.get(index).trim().equals("@Test")) {
                continue;
            }
            int methodStart = index + 1;
            while (methodStart < lines.size() && !lines.get(methodStart).contains("{")) {
                methodStart++;
            }
            if (methodStart >= lines.size()) {
                return "";
            }
            int depth = 0;
            List<String> method = new ArrayList<>();
            for (int lineIndex = methodStart; lineIndex < lines.size(); lineIndex++) {
                String line = lines.get(lineIndex);
                method.add(line);
                for (int charIndex = 0; charIndex < line.length(); charIndex++) {
                    char character = line.charAt(charIndex);
                    if (character == '{') {
                        depth++;
                    } else if (character == '}') {
                        depth--;
                    }
                }
                if (depth == 0) {
                    return String.join("\n", method) + "\n";
                }
            }
        }
        return "";
    }

    private static String methodName(String method) {
        Matcher matcher = METHOD_NAME.matcher(method);
        return matcher.find() ? matcher.group(1) : "capturedFlow";
    }

    private static String driverType(String source) {
        Matcher matcher = DRIVER_TYPE.matcher(source);
        if (matcher.find() && matcher.group(1).equals("Playwright")) {
            return "SHAFT.GUI.Playwright";
        }
        return "SHAFT.GUI.WebDriver";
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

    private static Optional<CaptureGenerationReport.LocatorDecision> matchingDecision(
            String expression,
            List<CaptureGenerationReport.LocatorDecision> decisions) {
        return decisions.stream()
                .filter(decision -> !decision.expression().isBlank()
                        && expression.contains(decision.expression()))
                .findFirst();
    }

    private static String description(
            String sourceName,
            CaptureGenerationReport.LocatorDecision decision) {
        List<String> details = new ArrayList<>();
        if (!sourceName.isBlank()) {
            details.add("Source locator " + sourceName);
        }
        if (decision != null) {
            details.add("selected " + decision.strategy() + " locator \""
                    + decision.expression() + "\"");
            if (!decision.eventIds().isEmpty()) {
                details.add("events " + String.join(", ", decision.eventIds()));
            }
            if (!decision.alternatives().isEmpty()) {
                details.add("alternatives " + String.join(", ", decision.alternatives()));
            }
        }
        return String.join("; ", details);
    }

    private static List<String> evidenceIds(Optional<CaptureGenerationReport.LocatorDecision> decision) {
        return decision.map(CaptureGenerationReport.LocatorDecision::eventIds).orElse(List.of());
    }

    private static List<String> evidenceIds(List<LocatorCandidate> locators) {
        return locators.stream()
                .flatMap(locator -> locator.evidenceIds().stream())
                .distinct()
                .toList();
    }

    private static List<String> locatorImports(List<String> imports, String code) {
        List<String> result = new ArrayList<>(imports);
        if (result.stream().noneMatch(importName -> importName.equals("org.openqa.selenium.By"))) {
            result.add("org.openqa.selenium.By");
        }
        if (code.contains("SHAFT.GUI.") && result.stream()
                .noneMatch(importName -> importName.equals("com.shaft.driver.SHAFT"))) {
            result.add("com.shaft.driver.SHAFT");
        }
        return List.copyOf(result);
    }

    private static String reportLocatorExpression(CaptureGenerationReport.LocatorDecision decision) {
        String expression = javaString(decision.expression());
        String strategy = decision.strategy() == null ? "" : decision.strategy().toUpperCase(Locale.ROOT);
        if (DIRECT_BY_STRATEGIES.contains(strategy)) {
            return switch (strategy) {
                case "CSS", "TEST_ID" -> "By.cssSelector(\"" + expression + "\")";
                case "XPATH" -> "By.xpath(\"" + expression + "\")";
                case "ID" -> "SHAFT.GUI.Locator.hasAnyTagName().hasId(\"" + expression + "\").build()";
                case "NAME" -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"name\", \""
                        + expression + "\").build()";
                case "CLASS_NAME" -> "By.className(\"" + expression + "\")";
                case "TAG_NAME" -> "By.tagName(\"" + expression + "\")";
                case "LINK_TEXT" -> "By.linkText(\"" + expression + "\")";
                case "PARTIAL_LINK_TEXT" -> "By.partialLinkText(\"" + expression + "\")";
                default -> "By.cssSelector(\"" + expression + "\")";
            };
        }
        return "SHAFT.GUI.Locator.hasAnyTagName().hasText(\"" + expression + "\").build()";
    }

    private static String stripTrailingSemicolon(String value) {
        return value.endsWith(";") ? value.substring(0, value.length() - 1).trim() : value;
    }

    private static String suggestedFieldName(String value) {
        String base = lowerCamel(value);
        if (base.isBlank()) {
            return "capturedLocator";
        }
        return base.endsWith("Locator") ? base : base + "Locator";
    }

    private static String expressionName(String expression) {
        Matcher quoted = Pattern.compile("\"([^\"]+)\"").matcher(expression);
        if (quoted.find()) {
            return quoted.group(1);
        }
        String cleaned = expression.replaceAll("[^A-Za-z0-9]+", " ");
        return cleaned.isBlank() ? "captured" : cleaned;
    }

    private static String lowerCamel(String value) {
        String[] parts = value.replaceAll("([a-z0-9])([A-Z])", "$1 $2")
                .replaceAll("[^A-Za-z0-9]+", " ")
                .trim()
                .split("\\s+");
        if (parts.length == 0 || parts[0].isBlank()) {
            return "";
        }
        StringBuilder result = new StringBuilder(parts[0].toLowerCase(Locale.ROOT));
        for (int index = 1; index < parts.length; index++) {
            String part = parts[index].toLowerCase(Locale.ROOT);
            result.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
        }
        return javaIdentifierOrDefault(result.toString(), "capturedLocator");
    }

    private static String javaString(String value) {
        return Objects.toString(value, "")
                .replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    private record LocatorCandidate(
            String suggestedFieldName,
            String expression,
            List<String> evidenceIds,
            String description) {
        private LocatorCandidate {
            evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
        }
    }

    private record ActionCandidate(String flow, String line) {
    }

    private record PomCandidates(List<LocatorCandidate> locators, List<ActionCandidate> actions) {
        private PomCandidates {
            locators = locators == null ? List.of() : List.copyOf(locators);
            actions = actions == null ? List.of() : List.copyOf(actions);
        }
    }
}
