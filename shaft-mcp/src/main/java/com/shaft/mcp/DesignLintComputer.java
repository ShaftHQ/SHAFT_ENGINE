package com.shaft.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Deterministic Gherkin lint shared by MCP, CLI, and the plugin (issue #5953).
 */
final class DesignLintComputer {
    private static final int MAX_SCENARIOS = 12;
    private static final Pattern SCENARIO = Pattern.compile(
            "^\\s*Scenario(?:\\s+Outline)?\\s*:\\s*(.+?)\\s*$", Pattern.CASE_INSENSITIVE);
    private static final Pattern THEN = Pattern.compile("^\\s*Then\\b", Pattern.CASE_INSENSITIVE);
    private static final Pattern WHEN = Pattern.compile("^\\s*When\\b", Pattern.CASE_INSENSITIVE);
    private static final Pattern SHAFT_API = Pattern.compile(
            "(?i)\\b(?:shaft\\.|elementactions|validations\\.assert|driverfactory|by\\.xpath)\\b");

    private DesignLintComputer() {
    }

    static McpDesignLint lint(String gherkin, String waivedRaw) {
        String feature = gherkin == null ? "" : gherkin.strip();
        if (feature.isEmpty()) {
            return new McpDesignLint(
                    McpDesignLint.CURRENT_SCHEMA_VERSION,
                    McpDesignLint.STATUS_ERROR,
                    "No Gherkin to lint.",
                    List.of(),
                    true,
                    false);
        }
        List<McpDesignLintFinding> findings = new ArrayList<>();
        List<ScenarioBlock> scenarios = parseScenarios(feature);
        addLocatorFindings(findings, scenarios);
        addMissingThen(findings, scenarios);
        addDuplicateNames(findings, scenarios);
        addInventedApis(findings, scenarios);
        if (scenarios.size() > MAX_SCENARIOS) {
            findings.add(finding(findings.size() + 1, "error", "too_many_scenarios",
                    "Feature has " + scenarios.size() + " scenarios; keep at most " + MAX_SCENARIOS + "."));
        }
        Set<String> waived = waivedIds(waivedRaw);
        boolean acceptBlocked = false;
        for (McpDesignLintFinding finding : findings) {
            if ("error".equals(finding.level()) && !waived.contains(finding.id())) {
                acceptBlocked = true;
                break;
            }
        }
        String status = acceptBlocked ? McpDesignLint.STATUS_BLOCKED : McpDesignLint.STATUS_OK;
        String message = acceptBlocked
                ? "Error-level lint findings block accept until resolved or waived with a reason."
                : "Gherkin lint is clean.";
        return new McpDesignLint(
                McpDesignLint.CURRENT_SCHEMA_VERSION, status, message, findings, acceptBlocked, false);
    }

    private static McpDesignLintFinding finding(int index, String level, String rule, String message) {
        return new McpDesignLintFinding("LINT-%02d".formatted(index), level, rule, message);
    }

    private static Set<String> waivedIds(String waivedRaw) {
        Set<String> ids = new LinkedHashSet<>();
        if (waivedRaw == null || waivedRaw.isBlank()) {
            return ids;
        }
        for (String part : waivedRaw.split(",")) {
            int split = part.indexOf(':');
            if (split <= 0) {
                continue;
            }
            String id = part.substring(0, split).strip();
            String reason = part.substring(split + 1).strip();
            if (!id.isEmpty() && !reason.isEmpty()) {
                ids.add(id);
            }
        }
        return ids;
    }

    private record ScenarioBlock(String name, List<String> steps) {
    }

    private static List<ScenarioBlock> parseScenarios(String feature) {
        List<ScenarioBlock> scenarios = new ArrayList<>();
        String name = null;
        List<String> steps = new ArrayList<>();
        for (String raw : feature.split("\\R")) {
            String line = raw.strip();
            Matcher scenario = SCENARIO.matcher(line);
            if (scenario.matches()) {
                if (name != null) {
                    scenarios.add(new ScenarioBlock(name, List.copyOf(steps)));
                }
                name = scenario.group(1).strip();
                steps = new ArrayList<>();
                continue;
            }
            if (name != null && !line.isEmpty() && !line.startsWith("#") && !line.startsWith("@")) {
                steps.add(line);
            }
        }
        if (name != null) {
            scenarios.add(new ScenarioBlock(name, List.copyOf(steps)));
        }
        return scenarios;
    }

    private static void addLocatorFindings(List<McpDesignLintFinding> findings, List<ScenarioBlock> scenarios) {
        for (ScenarioBlock scenario : scenarios) {
            for (String step : scenario.steps()) {
                if (!WHEN.matcher(step).find()) {
                    continue;
                }
                String lower = step.toLowerCase(Locale.ROOT);
                if (lower.contains("click") && lower.contains("xpath")) {
                    findings.add(finding(findings.size() + 1, "error", "click_xpath",
                            "When step uses click and xpath in scenario '" + scenario.name() + "'."));
                }
            }
        }
    }

    private static void addMissingThen(List<McpDesignLintFinding> findings, List<ScenarioBlock> scenarios) {
        for (ScenarioBlock scenario : scenarios) {
            boolean then = false;
            for (String step : scenario.steps()) {
                if (THEN.matcher(step).find()) {
                    then = true;
                    break;
                }
            }
            if (!then) {
                findings.add(finding(findings.size() + 1, "error", "missing_then",
                        "Scenario '" + scenario.name() + "' has no Then step."));
            }
        }
    }

    private static void addDuplicateNames(List<McpDesignLintFinding> findings, List<ScenarioBlock> scenarios) {
        Map<String, Integer> counts = new LinkedHashMap<>();
        for (ScenarioBlock scenario : scenarios) {
            String key = scenario.name().toLowerCase(Locale.ROOT);
            counts.put(key, counts.getOrDefault(key, 0) + 1);
        }
        for (Map.Entry<String, Integer> entry : counts.entrySet()) {
            if (entry.getValue() > 1) {
                findings.add(finding(findings.size() + 1, "error", "duplicate_scenario",
                        "Duplicate scenario name '" + entry.getKey() + "'."));
            }
        }
    }

    private static void addInventedApis(List<McpDesignLintFinding> findings, List<ScenarioBlock> scenarios) {
        for (ScenarioBlock scenario : scenarios) {
            for (String step : scenario.steps()) {
                if (SHAFT_API.matcher(step).find()) {
                    findings.add(finding(findings.size() + 1, "error", "invented_api",
                            "Step invents a SHAFT API token in scenario '" + scenario.name() + "'."));
                }
            }
        }
    }
}
