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
            if (!pom.locators().isEmpty() && !pom.actions().isEmpty()) {
                blocks.add(pageObjectDraftBlock(pom, driverType, driver));
            }
            blocks.addAll(reportBlocks(report, driver));
            if (targetSource != null || (insertAfter != null && !insertAfter.isBlank())) {
                blocks.addAll(targetInsertionBlocks(sourcePath, targetSource, insertAfter, driver));
            }
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
        return blocks;
    }

    private static McpCodeBlock locatorInventoryBlock(List<String> imports, List<LocatorCandidate> locators) {
        StringBuilder code = new StringBuilder();
        for (LocatorCandidate locator : locators) {
            if (!locator.description().isBlank()) {
                code.append("// ").append(locator.description()).append('\n');
            }
            code.append("// ")
                    .append(locator.suggestedFieldName())
                    .append(" -> ")
                    .append(locator.expression())
                    .append('\n');
            for (String alternative : locator.alternatives()) {
                code.append("// alternative -> ")
                        .append(alternativeLocatorExpression(alternative))
                        .append('\n');
            }
        }
        return new McpCodeBlock(
                "capture-pom-locator-inventory",
                "Page Object locator inventory",
                McpCodeBlock.Kind.LOCATOR,
                "java",
                locatorImports(imports, code.toString()),
                code.toString(),
                "Use these SHAFT locator expressions when extracting inline actions into page methods.",
                false,
                evidenceIds(locators),
                List.of());
    }

    private static List<McpCodeBlock> reportBlocks(CaptureGenerationReport report, String driver) {
        if (report == null) {
            return List.of();
        }
        List<McpCodeBlock> blocks = new ArrayList<>();
        if (!report.requiredUserInputs().isEmpty()) {
            blocks.add(setupBlock(report));
        }
        if (needsAssertionSuggestion(report)) {
            blocks.add(assertionSuggestionBlock(report, driver));
        }
        if (!report.controlFlowSuggestions().isEmpty()) {
            blocks.add(controlFlowReviewBlock(report));
        }
        return List.copyOf(blocks);
    }

    private static McpCodeBlock setupBlock(CaptureGenerationReport report) {
        StringBuilder code = new StringBuilder();
        code.append("# Resolve these before replaying the generated Capture test\n");
        code.append("testDataPath=").append(report.testDataPath().isBlank()
                ? "<generated-test-data.json>"
                : report.testDataPath()).append('\n');
        for (String input : report.requiredUserInputs()) {
            code.append("- ").append(input).append('\n');
        }
        return new McpCodeBlock(
                "capture-data-setup",
                "Capture replay setup inputs",
                McpCodeBlock.Kind.SETUP,
                "text",
                List.of(),
                code.toString(),
                "Set required environment variables, JSON data, and upload fixtures before replay.",
                false,
                evidenceIds(String.join(" ", report.requiredUserInputs())),
                List.of());
    }

    private static McpCodeBlock assertionSuggestionBlock(CaptureGenerationReport report, String driver) {
        String javaDriver = javaIdentifierOrDefault(driver, "driver");
        String code = """
                // Add after the recorded navigation or submit action once the expected page state is known.
                %s.browser().assertThat().url().contains("REPLACE_WITH_EXPECTED_PATH").perform();
                %s.browser().assertThat().title().contains("REPLACE_WITH_EXPECTED_TITLE").perform();
                """.formatted(javaDriver, javaDriver).stripIndent();
        List<String> warnings = assertionWarnings(report);
        return new McpCodeBlock(
                "capture-assertion-suggestions",
                "Post-action assertion suggestions",
                McpCodeBlock.Kind.ASSERTION,
                "java",
                List.of(),
                code,
                "Replace placeholders with the captured destination URL, title, or page-specific text.",
                false,
                evidenceIds(String.join(" ", warnings)),
                warnings);
    }

    private static McpCodeBlock controlFlowReviewBlock(CaptureGenerationReport report) {
        StringBuilder code = new StringBuilder();
        code.append("capture generate --session <capture.json> --control-flow-preview\n");
        code.append("capture generate --session <capture.json> --apply-control-flow-preview ")
                .append("target/shaft-capture/control-flow-preview.json\n\n");
        for (CaptureGenerationReport.ControlFlowSuggestion suggestion : report.controlFlowSuggestions()) {
            code.append("- ")
                    .append(suggestion.id())
                    .append(" [")
                    .append(suggestion.kind())
                    .append("] ")
                    .append(suggestion.recommendation())
                    .append('\n');
        }
        return new McpCodeBlock(
                "capture-control-flow-review",
                "Capture control-flow review",
                McpCodeBlock.Kind.INVESTIGATION,
                "text",
                List.of(),
                code.toString(),
                "Preview, review, then explicitly apply deterministic control-flow suggestions.",
                false,
                report.controlFlowSuggestions().stream()
                        .flatMap(suggestion -> suggestion.evidenceIds().stream())
                        .distinct()
                        .toList(),
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

    private static McpCodeBlock pageObjectDraftBlock(
            PomCandidates pom,
            String driverType,
            String driver) {
        String pageClassName = pageClassName(pom.actions().getFirst().flow());
        StringBuilder code = new StringBuilder();
        code.append("public final class ").append(pageClassName).append(" {\n");
        code.append("    private final ").append(driverType).append(' ').append(driver).append(";\n");
        for (LocatorCandidate locator : pom.locators()) {
            if (!locator.description().isBlank()) {
                code.append("    // ").append(locator.description()).append('\n');
            }
            code.append("    private final By ")
                    .append(locator.suggestedFieldName())
                    .append(" = ")
                    .append(locator.expression())
                    .append(";\n");
        }
        code.append("\n");
        code.append("    public ").append(pageClassName).append('(').append(driverType).append(' ')
                .append(driver).append(") {\n");
        code.append("        this.").append(driver).append(" = ").append(driver).append(";\n");
        code.append("    }\n");

        String currentFlow = "";
        String currentMethod = "";
        for (ActionCandidate action : pom.actions()) {
            if (!action.flow().equals(currentFlow)) {
                if (!currentFlow.isBlank()) {
                    code.append("        return this;\n");
                    code.append("    }\n");
                }
                currentFlow = action.flow();
                currentMethod = flowMethodName(currentFlow);
                code.append("\n");
                code.append("    public ").append(pageClassName).append(' ')
                        .append(currentMethod).append("() {\n");
            }
            code.append("        ").append(replaceLocatorExpressions(action.line(), pom.locators())).append('\n');
        }
        if (!currentFlow.isBlank()) {
            code.append("        return this;\n");
            code.append("    }\n");
        }
        code.append("}\n");
        return new McpCodeBlock(
                "capture-page-object-draft",
                "Page Object draft",
                McpCodeBlock.Kind.ACTION,
                "java",
                List.of("com.shaft.driver.SHAFT", "org.openqa.selenium.By"),
                code.toString(),
                "Use as a starting point for a Page Object; review naming and required data helpers before pasting.",
                false,
                evidenceIds(pom.locators()),
                List.of("Review requiredData and assertion placeholders before making the draft copy-paste ready."));
    }

    private static String replaceLocatorExpressions(String line, List<LocatorCandidate> locators) {
        String replaced = line;
        for (LocatorCandidate locator : locators) {
            replaced = replaced.replace(locator.expression(), locator.suggestedFieldName());
        }
        return replaced;
    }

    private static String pageClassName(String flow) {
        String seed = flow == null || flow.isBlank() ? "Captured" : flow.trim();
        if (seed.startsWith("replay") && seed.length() > "replay".length()) {
            seed = seed.substring("replay".length());
        }
        if (seed.isBlank()) {
            seed = "Captured";
        }
        String base = lowerCamel(seed, "captured");
        return Character.toUpperCase(base.charAt(0)) + base.substring(1) + "Page";
    }

    private static String flowMethodName(String flow) {
        return lowerCamel(flow == null || flow.isBlank() ? "capturedFlow" : flow, "capturedFlow");
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
                        description(sourceName, decision.orElse(null)),
                        alternatives(decision)));
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
                        description("", decision.orElse(null)),
                        alternatives(decision)));
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
                        description("", decision),
                        decision.alternatives()));
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
            int start = shaft;
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

    private static List<String> evidenceIds(String text) {
        List<String> ids = new ArrayList<>();
        Matcher matcher = Pattern.compile("\\b(?:event-\\d+|checkpoint-[A-Za-z0-9._-]+|action-\\d+)\\b")
                .matcher(text == null ? "" : text);
        while (matcher.find()) {
            ids.add(matcher.group());
        }
        return ids.stream().distinct().toList();
    }

    private static List<String> locatorImports(List<String> imports, String code) {
        List<String> result = new ArrayList<>(imports);
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
                case "CSS", "TEST_ID" -> "SHAFT.GUI.Locator.cssSelector(\"" + expression + "\")";
                case "XPATH" -> "SHAFT.GUI.Locator.xpath(\"" + expression + "\")";
                case "ID" -> "SHAFT.GUI.Locator.id(\"" + expression + "\")";
                case "NAME" -> "SHAFT.GUI.Locator.name(\"" + expression + "\")";
                case "CLASS_NAME" -> "SHAFT.GUI.Locator.className(\"" + expression + "\")";
                case "TAG_NAME" -> "SHAFT.GUI.Locator.tagName(\"" + expression + "\")";
                case "LINK_TEXT" -> "SHAFT.GUI.Locator.xpath(\"//a[normalize-space(.)='"
                        + expression + "']\")";
                case "PARTIAL_LINK_TEXT" -> "SHAFT.GUI.Locator.xpath(\"//a[contains(normalize-space(.), '"
                        + expression + "')]\")";
                default -> "SHAFT.GUI.Locator.cssSelector(\"" + expression + "\")";
            };
        }
        return "SHAFT.GUI.Locator.xpath(\"//*[normalize-space(.)='" + expression + "']\")";
    }

    private static String alternativeLocatorExpression(String alternative) {
        String cleaned = alternative == null ? "" : alternative.trim();
        int score = cleaned.indexOf(" (score ");
        if (score >= 0) {
            cleaned = cleaned.substring(0, score).trim();
        }
        int space = cleaned.indexOf(' ');
        if (space <= 0 || space == cleaned.length() - 1) {
            return cleaned;
        }
        return reportLocatorExpression(new CaptureGenerationReport.LocatorDecision(
                List.of(),
                "",
                cleaned.substring(0, space),
                cleaned.substring(space + 1),
                0,
                List.of(),
                List.of()));
    }

    private static boolean needsAssertionSuggestion(CaptureGenerationReport report) {
        return !assertionWarnings(report).isEmpty();
    }

    private static List<String> assertionWarnings(CaptureGenerationReport report) {
        List<String> warnings = new ArrayList<>();
        report.readinessWarnings().stream()
                .filter(warning -> warning.toLowerCase(Locale.ROOT).contains("assertion"))
                .forEach(warnings::add);
        report.warnings().stream()
                .filter(warning -> warning.contains("review/ASSERTION"))
                .forEach(warnings::add);
        return warnings.stream().distinct().toList();
    }

    private static List<String> alternatives(Optional<CaptureGenerationReport.LocatorDecision> decision) {
        return decision.map(CaptureGenerationReport.LocatorDecision::alternatives).orElse(List.of());
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
        return lowerCamel(value, "capturedLocator");
    }

    private static String lowerCamel(String value, String fallback) {
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
        return javaIdentifierOrDefault(result.toString(), fallback);
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
            String description,
            List<String> alternatives) {
        private LocatorCandidate {
            evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
            alternatives = alternatives == null ? List.of() : List.copyOf(alternatives);
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
