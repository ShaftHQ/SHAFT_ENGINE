package com.shaft.gui.internal.aria;

import com.microsoft.playwright.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.yaml.snakeyaml.Yaml;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Captures and compares accessible-name-tree ("aria") snapshots: a pragmatic, DOM-walk-based subset of
 * ARIA role/name resolution (not full ARIA AAM spec compliance), serialized as a strict subset of
 * Playwright's aria snapshot YAML DSL (no {@code [level=n]} attributes, no regex names).
 *
 * <p>Both backends run the same {@code ariaSnapshot.js} DOM walk so web output is directly comparable.</p>
 */
public final class AriaSnapshotHelper {
    private static final Pattern LABEL_PATTERN = Pattern.compile("^(\\S+)(?:\\s+\"((?:[^\"\\\\]|\\\\.)*)\")?$");

    private AriaSnapshotHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Captures an aria snapshot of the element matched by {@code locator}, using Selenium JavaScript execution.
     *
     * @param driver active WebDriver instance (must support {@link JavascriptExecutor})
     * @param locator locator of the element to snapshot
     * @return the captured snapshot serialized as YAML
     */
    public static String captureAriaSnapshot(WebDriver driver, By locator) {
        if (!(driver instanceof JavascriptExecutor executor)) {
            throw new IllegalArgumentException("WebDriver must support JavaScript execution.");
        }
        WebElement element = driver.findElement(locator);
        Object raw = executor.executeScript(readAriaSnapshotScript() + ";return __shaftAriaSnapshot(arguments[0]);", element);
        return serialize(toNodes(raw));
    }

    /**
     * Captures an aria snapshot of the element matched by {@code locator}, using Playwright JavaScript evaluation.
     *
     * @param locator Playwright locator of the element to snapshot
     * @return the captured snapshot serialized as YAML
     */
    public static String captureAriaSnapshot(Locator locator) {
        Object raw = locator.evaluate("(element) => { " + readAriaSnapshotScript() + "; return __shaftAriaSnapshot(element); }");
        return serialize(toNodes(raw));
    }

    /**
     * Serializes an accessible-name-tree forest to SHAFT's aria snapshot YAML format.
     *
     * @param forest the top-level nodes to serialize
     * @return the YAML representation
     */
    public static String serialize(List<AriaNode> forest) {
        StringBuilder builder = new StringBuilder();
        appendNodes(builder, forest, 0);
        return builder.toString();
    }

    /**
     * Parses SHAFT's aria snapshot YAML format back into an accessible-name-tree forest.
     *
     * @param yaml the YAML representation
     * @return the parsed top-level nodes, or an empty list for blank input
     */
    public static List<AriaNode> parse(String yaml) {
        if (yaml == null || yaml.isBlank()) {
            return List.of();
        }
        return fromYamlList(new Yaml().load(yaml));
    }

    /**
     * Performs a partial-subset structural match: every baseline node must match an actual node at the
     * same depth, in order (role equal; name equal after trim, or the baseline name may be blank to match
     * any actual name); extra actual siblings/children are allowed. This is a pragmatic (greedy, not fully
     * backtracking) subsequence match, sufficient for visual/structural regression baselining.
     *
     * @param baselineYaml the stored baseline snapshot
     * @param actualYaml the freshly captured snapshot
     * @return the match result, with a readable diff message on mismatch
     */
    public static AriaMatchResult match(String baselineYaml, String actualYaml) {
        List<AriaNode> baseline = parse(baselineYaml);
        List<AriaNode> actual = parse(actualYaml);
        String diff = matchForest(baseline, actual, "");
        return new AriaMatchResult(diff == null, diff == null ? "" : diff);
    }

    private static String matchForest(List<AriaNode> baseline, List<AriaNode> actual, String path) {
        int actualIndex = 0;
        for (AriaNode expected : baseline) {
            boolean found = false;
            while (actualIndex < actual.size()) {
                AriaNode candidate = actual.get(actualIndex++);
                if (roleAndNameMatch(expected, candidate)) {
                    String childPath = path + "/" + describe(expected);
                    String childDiff = matchForest(expected.children(), candidate.children(), childPath);
                    if (childDiff == null) {
                        found = true;
                        break;
                    }
                }
            }
            if (!found) {
                return "Expected node " + describe(expected) + " at " + (path.isEmpty() ? "/" : path)
                        + " was not found in the actual aria snapshot.";
            }
        }
        return null;
    }

    private static boolean roleAndNameMatch(AriaNode expected, AriaNode actual) {
        if (!expected.role().equals(actual.role())) {
            return false;
        }
        return expected.name().isBlank() || expected.name().equals(actual.name());
    }

    private static String describe(AriaNode node) {
        return node.name().isBlank() ? node.role() : node.role() + " \"" + node.name() + "\"";
    }

    private static void appendNodes(StringBuilder builder, List<AriaNode> nodes, int indentLevel) {
        String indent = "  ".repeat(indentLevel);
        for (AriaNode node : nodes) {
            String label = formatLabel(node.role(), node.name());
            if (node.children().isEmpty()) {
                builder.append(indent).append("- ").append(label).append('\n');
            } else {
                builder.append(indent).append("- ").append(label).append(":\n");
                appendNodes(builder, node.children(), indentLevel + 1);
            }
        }
    }

    private static String formatLabel(String role, String name) {
        if (name == null || name.isBlank()) {
            return role;
        }
        return role + " \"" + name.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }

    @SuppressWarnings("unchecked")
    private static List<AriaNode> toNodes(Object raw) {
        if (!(raw instanceof List<?> rawList)) {
            return List.of();
        }
        List<AriaNode> nodes = new ArrayList<>();
        for (Object item : rawList) {
            if (item instanceof Map<?, ?> map) {
                String role = String.valueOf(map.get("role"));
                Object nameObj = map.get("name");
                nodes.add(new AriaNode(role, nameObj == null ? "" : String.valueOf(nameObj), toNodes(map.get("children"))));
            }
        }
        return nodes;
    }

    private static List<AriaNode> fromYamlList(Object loaded) {
        if (!(loaded instanceof List<?> list)) {
            return List.of();
        }
        List<AriaNode> nodes = new ArrayList<>();
        for (Object item : list) {
            nodes.add(fromYamlItem(item));
        }
        return nodes;
    }

    private static AriaNode fromYamlItem(Object item) {
        if (item instanceof String scalar) {
            String[] roleAndName = parseLabel(scalar);
            return new AriaNode(roleAndName[0], roleAndName[1], List.of());
        }
        if (item instanceof Map<?, ?> map && map.size() == 1) {
            Map.Entry<?, ?> entry = map.entrySet().iterator().next();
            String[] roleAndName = parseLabel(String.valueOf(entry.getKey()));
            return new AriaNode(roleAndName[0], roleAndName[1], fromYamlList(entry.getValue()));
        }
        throw new IllegalArgumentException("Malformed aria snapshot node: " + item);
    }

    private static String[] parseLabel(String label) {
        Matcher matcher = LABEL_PATTERN.matcher(label.trim());
        if (!matcher.matches()) {
            return new String[]{label.trim(), ""};
        }
        String role = matcher.group(1);
        String name = matcher.group(2);
        if (name == null) {
            return new String[]{role, ""};
        }
        return new String[]{role, name.replace("\\\"", "\"").replace("\\\\", "\\")};
    }

    private static String readAriaSnapshotScript() {
        try (InputStream stream = AriaSnapshotHelper.class.getClassLoader().getResourceAsStream("ariaSnapshot.js")) {
            if (stream == null) {
                throw new IllegalStateException("ariaSnapshot.js was not found on the classpath.");
            }
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * Result of a partial-subset aria snapshot comparison.
     *
     * @param matched {@code true} when every baseline node was found in the actual snapshot
     * @param diffMessage a readable description of the first mismatch, or empty when matched
     */
    public record AriaMatchResult(boolean matched, String diffMessage) {
    }
}
