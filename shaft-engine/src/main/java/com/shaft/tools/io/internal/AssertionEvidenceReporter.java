package com.shaft.tools.io.internal;

import com.shaft.tools.internal.support.ReportHtmlTheme;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Renders a self-contained HTML "assertion evidence" card for a single {@code Assert}/{@code Verify}
 * validation, redacting secrets and diffing expected/actual values, so Allure attachments can show
 * exactly what a validation compared without leaking sensitive data or exceeding a bounded size.
 */
public final class AssertionEvidenceReporter {

    private static final int MAX_SIDE_CHARACTERS = 20_000;
    private static final int MAX_DIFF_LINES = 400;
    private static final int TEXT_SHAPE_THRESHOLD = 60;
    private static final long MAX_LCS_CELLS = 200_000L;
    private static final ObjectMapper JSON_MAPPER = JsonMapper.builder()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .build();

    private AssertionEvidenceReporter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Renders a self-contained HTML "assertion evidence" card for one validation.
     *
     * @param categoryLabel "Assert" (hard) or "Verify" (soft) — as passed by ValidationsHelper
     * @param passed        true when the validation passed
     * @param expected      the reported expected value (may be null)
     * @param actual        the reported actual value (may be null)
     * @return a complete self-contained HTML document string, or "" when there is nothing
     * meaningful to show (both expected and actual are null/blank)
     */
    public static String renderCard(String categoryLabel, boolean passed, Object expected, Object actual) {
        if (isBlank(expected) && isBlank(actual)) {
            return "";
        }
        // A pure pass/fail flag (element-state and visual validations report their outcome as a
        // boolean) is not comparable evidence — the real evidence there is the screenshot, page
        // snapshot, or image diff attached separately. Rendering a "true vs false" card would only
        // add noise and duplicate what the step message already states.
        if (isBooleanFlag(expected) && isBooleanFlag(actual)) {
            return "";
        }
        try {
            return render(categoryLabel, passed, expected, actual);
        } catch (RuntimeException e) {
            return renderFallback(categoryLabel, passed, expected, actual);
        }
    }

    /**
     * Renders a domain-consistent evidence card for an accessibility (aria snapshot) validation.
     *
     * <p>Accessibility validations report their outcome as a pass/fail boolean, which
     * {@link #renderCard} deliberately skips because a "true vs false" flag carries no comparable
     * content. This overload instead takes the expected/actual aria-snapshot YAML so the card can
     * show the same bounded, redacted line diff assertion cards use — a summary line plus a deep
     * attachment, rather than a full YAML dump in the step name (issue #3532 E).
     *
     * @param passed       true when the aria snapshot matched its baseline
     * @param expectedYaml the baseline aria-snapshot YAML (may be null/blank on first capture)
     * @param actualYaml   the captured aria-snapshot YAML
     * @return a complete self-contained HTML document string, or "" when there is nothing
     * meaningful to show (both sides are null/blank)
     */
    public static String renderAccessibilityCard(boolean passed, Object expectedYaml, Object actualYaml) {
        if (isBlank(expectedYaml) && isBlank(actualYaml)) {
            return "";
        }
        try {
            return render("Accessibility", passed, expectedYaml, actualYaml);
        } catch (RuntimeException e) {
            return renderFallback("Accessibility", passed, expectedYaml, actualYaml);
        }
    }

    private static String render(String categoryLabel, boolean passed, Object expected, Object actual) {
        Capped expectedText = cap(safeRedact(rawValue(expected)));
        Capped actualText = cap(safeRedact(rawValue(actual)));

        JsonNode expectedJson = tryParseJsonContainer(expectedText.text());
        JsonNode actualJson = tryParseJsonContainer(actualText.text());
        boolean bothJson = expectedJson != null && actualJson != null;

        String shape;
        String body;
        if (bothJson) {
            shape = "JSON";
            String prettyExpected = prettyPrint(expectedJson, expectedText.text());
            String prettyActual = prettyPrint(actualJson, actualText.text());
            body = diffSection(prettyExpected, prettyActual) + rawSection(expectedText, actualText);
        } else if (isTextShape(expectedText.text(), actualText.text())) {
            shape = "Text";
            body = diffSection(expectedText.text(), actualText.text()) + rawSection(expectedText, actualText);
        } else {
            shape = "Value";
            body = scalarSection(expectedText, actualText);
        }

        return document(categoryLabel, passed, shape, body);
    }

    /**
     * Minimal, exception-proof degraded rendering used when {@link #render} fails for any reason.
     * Trace/evidence generation must never break test execution because of a weird value.
     */
    private static String renderFallback(String categoryLabel, boolean passed, Object expected, Object actual) {
        try {
            String category = categoryText(categoryLabel);
            String status = passed ? "PASSED" : "FAILED";
            String exp = escapeHtml(safeRedact(rawValue(expected)));
            String act = escapeHtml(safeRedact(rawValue(actual)));
            return "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
                    + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                    + "<title>Assertion Evidence</title><style>" + ReportHtmlTheme.style() + "</style></head><body>"
                    + "<div class=\"aer-card\"><div class=\"aer-header\">"
                    + "<span class=\"aer-category\">" + escapeHtml(category) + "</span>"
                    + "<span class=\"status-chip " + ReportHtmlTheme.statusClass(status) + "\">" + status + "</span>"
                    + "</div><div class=\"aer-scalar\">"
                    + "<div class=\"aer-scalar-row\"><span class=\"aer-scalar-label\">Expected</span>"
                    + "<span class=\"aer-scalar-value\">" + exp + "</span></div>"
                    + "<div class=\"aer-scalar-row\"><span class=\"aer-scalar-label\">Actual</span>"
                    + "<span class=\"aer-scalar-value\">" + act + "</span></div>"
                    + "</div></div></body></html>";
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static String document(String categoryLabel, boolean passed, String shape, String bodyHtml) {
        String category = categoryText(categoryLabel);
        String status = passed ? "PASSED" : "FAILED";
        return "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
                + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
                + "<title>Assertion Evidence</title>\n<style>\n" + ReportHtmlTheme.style() + cardCss() + "</style>\n"
                + "</head>\n<body>\n<div class=\"aer-card\">\n"
                + "<div class=\"aer-header\">"
                + "<span class=\"aer-category\">" + escapeHtml(category) + "</span>"
                + "<span class=\"status-chip " + ReportHtmlTheme.statusClass(status) + "\">" + status + "</span>"
                + "<span class=\"status-chip aer-shape\">" + escapeHtml(shape) + "</span>"
                + "</div>\n"
                + bodyHtml
                + "\n</div>\n</body>\n</html>\n";
    }

    private static String categoryText(String categoryLabel) {
        if (categoryLabel == null) {
            return "Assertion";
        }
        return switch (categoryLabel.trim()) {
            case "Assert" -> "Assertion";
            case "Verify" -> "Verification";
            default -> categoryLabel;
        };
    }

    private static String scalarSection(Capped expected, Capped actual) {
        return "<div class=\"aer-scalar\">"
                + "<div class=\"aer-scalar-row\"><span class=\"aer-scalar-label\">Expected</span>"
                + "<span class=\"aer-scalar-value\">" + escapeHtml(expected.withMarker()) + "</span></div>"
                + "<div class=\"aer-scalar-row\"><span class=\"aer-scalar-label\">Actual</span>"
                + "<span class=\"aer-scalar-value\">" + escapeHtml(actual.withMarker()) + "</span></div>"
                + "</div>";
    }

    private static String rawSection(Capped expected, Capped actual) {
        return "<details class=\"aer-raw\"><summary>Raw values</summary><dl class=\"aer-raw-list\">"
                + "<dt>Expected</dt><dd><pre>" + escapeHtml(expected.withMarker()) + "</pre></dd>"
                + "<dt>Actual</dt><dd><pre>" + escapeHtml(actual.withMarker()) + "</pre></dd>"
                + "</dl></details>";
    }

    private static String diffSection(String left, String right) {
        List<DiffLine> lines = lineDiff(left, right);
        StringBuilder html = new StringBuilder("<div class=\"aer-diff\">");
        int shown = Math.min(lines.size(), MAX_DIFF_LINES);
        for (int i = 0; i < shown; i++) {
            DiffLine line = lines.get(i);
            html.append("<div class=\"aer-diff-line ").append(line.kind().cssClass()).append("\">")
                    .append("<span class=\"aer-diff-marker\">").append(line.kind().marker()).append("</span>")
                    .append("<span class=\"aer-diff-text\">").append(escapeHtml(line.text())).append("</span>")
                    .append("</div>");
        }
        if (lines.size() > MAX_DIFF_LINES) {
            html.append("<div class=\"aer-diff-line aer-diff-truncated\">… [diff truncated, ")
                    .append(lines.size() - MAX_DIFF_LINES).append(" more lines omitted]</div>");
        }
        html.append("</div>");
        return html.toString();
    }

    private static List<DiffLine> lineDiff(String left, String right) {
        List<String> a = splitLines(left);
        List<String> b = splitLines(right);
        if ((long) a.size() * (long) b.size() > MAX_LCS_CELLS) {
            return setDiff(a, b);
        }
        return lcsDiff(a, b);
    }

    private static List<String> splitLines(String value) {
        if (value.isEmpty()) {
            return List.of();
        }
        return List.of(value.split("\\R", -1));
    }

    /**
     * Classic LCS-based line diff (dynamic programming), used when both sides are small enough
     * that the O(n*m) table stays bounded ({@link #MAX_LCS_CELLS}).
     */
    private static List<DiffLine> lcsDiff(List<String> a, List<String> b) {
        int n = a.size();
        int m = b.size();
        int[][] lcs = new int[n + 1][m + 1];
        for (int i = n - 1; i >= 0; i--) {
            for (int j = m - 1; j >= 0; j--) {
                lcs[i][j] = a.get(i).equals(b.get(j)) ? lcs[i + 1][j + 1] + 1 : Math.max(lcs[i + 1][j], lcs[i][j + 1]);
            }
        }
        List<DiffLine> result = new ArrayList<>();
        int i = 0;
        int j = 0;
        while (i < n && j < m) {
            if (a.get(i).equals(b.get(j))) {
                result.add(new DiffLine(DiffKind.SAME, a.get(i)));
                i++;
                j++;
            } else if (lcs[i + 1][j] >= lcs[i][j + 1]) {
                result.add(new DiffLine(DiffKind.REMOVED, a.get(i)));
                i++;
            } else {
                result.add(new DiffLine(DiffKind.ADDED, b.get(j)));
                j++;
            }
        }
        while (i < n) {
            result.add(new DiffLine(DiffKind.REMOVED, a.get(i)));
            i++;
        }
        while (j < m) {
            result.add(new DiffLine(DiffKind.ADDED, b.get(j)));
            j++;
        }
        return result;
    }

    /**
     * Cheap O(n+m) fallback for oversized inputs where the full LCS table would be too large:
     * a line is "same" when it appears on both sides (regardless of position), otherwise it is
     * attributed to whichever side it came from. Order fidelity is traded for bounded cost.
     */
    private static List<DiffLine> setDiff(List<String> a, List<String> b) {
        Set<String> bSet = new LinkedHashSet<>(b);
        Set<String> aSet = new LinkedHashSet<>(a);
        List<DiffLine> result = new ArrayList<>();
        for (String line : a) {
            result.add(new DiffLine(bSet.contains(line) ? DiffKind.SAME : DiffKind.REMOVED, line));
        }
        for (String line : b) {
            if (!aSet.contains(line)) {
                result.add(new DiffLine(DiffKind.ADDED, line));
            }
        }
        return result;
    }

    private static boolean isTextShape(String expected, String actual) {
        return expected.contains("\n") || actual.contains("\n")
                || expected.length() > TEXT_SHAPE_THRESHOLD || actual.length() > TEXT_SHAPE_THRESHOLD;
    }

    private static JsonNode tryParseJsonContainer(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        try {
            JsonNode node = JSON_MAPPER.readTree(value);
            return node != null && (node.isObject() || node.isArray()) ? node : null;
        } catch (RuntimeException e) {
            return null;
        }
    }

    private static String prettyPrint(JsonNode node, String fallback) {
        try {
            return JSON_MAPPER.writeValueAsString(node);
        } catch (RuntimeException e) {
            return fallback;
        }
    }

    private static Capped cap(String value) {
        String safe = value == null ? "" : value;
        if (safe.length() <= MAX_SIDE_CHARACTERS) {
            return new Capped(safe, false, 0);
        }
        return new Capped(safe.substring(0, MAX_SIDE_CHARACTERS), true, safe.length() - MAX_SIDE_CHARACTERS);
    }

    private static boolean isBlank(Object value) {
        if (value == null) {
            return true;
        }
        return String.valueOf(value).trim().isEmpty();
    }

    /**
     * A value that is only a true/false outcome flag ({@link Boolean} or {@link AtomicBoolean}),
     * as element-state and visual validations report. Such values carry no comparable content, so
     * the evidence card is skipped for them.
     */
    private static boolean isBooleanFlag(Object value) {
        return value instanceof Boolean || value instanceof java.util.concurrent.atomic.AtomicBoolean;
    }

    private static String rawValue(Object value) {
        return value == null ? "" : String.valueOf(value);
    }

    private static String safeRedact(String value) {
        try {
            return FailureTraceReporter.redact(value);
        } catch (RuntimeException e) {
            return value == null ? "" : value;
        }
    }

    /**
     * Escapes {@code & < > " '} for safe embedding anywhere in the card's HTML body.
     * {@link ReportHtmlTheme#escapeHtml(String)} does not escape apostrophes, which this card's
     * contract requires, so apostrophe handling is layered on top of the shared helper.
     */
    private static String escapeHtml(String value) {
        return ReportHtmlTheme.escapeHtml(value).replace("'", "&#39;");
    }

    private static String cardCss() {
        return """
                body{padding:16px}
                .aer-card{max-width:960px;margin:0 auto}
                .aer-header{display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin-bottom:12px}
                .aer-category{font-weight:700;font-size:15px}
                .aer-shape{color:var(--shaft-text-muted);background:rgba(var(--shaft-primary-rgb),.08);border-color:var(--shaft-border)}
                .aer-scalar{display:flex;flex-direction:column;gap:6px;margin:10px 0}
                .aer-scalar-row{display:flex;gap:8px;align-items:baseline}
                .aer-scalar-label{font-weight:700;color:var(--shaft-text-muted);min-width:70px;flex:0 0 auto}
                .aer-scalar-value{overflow-wrap:anywhere;font-family:ui-monospace,SFMono-Regular,Consolas,"Liberation Mono",monospace}
                .aer-diff{border:1px solid var(--shaft-border);border-radius:8px;overflow:auto;max-height:420px;font-family:ui-monospace,SFMono-Regular,Consolas,"Liberation Mono",monospace;font-size:.86em;margin:10px 0}
                .aer-diff-line{display:flex;gap:8px;padding:2px 8px;white-space:pre-wrap;overflow-wrap:anywhere}
                .aer-diff-line.aer-diff-removed{background:rgba(197,48,48,.10);color:var(--shaft-fail)}
                .aer-diff-line.aer-diff-added{background:rgba(20,128,74,.10);color:var(--shaft-pass)}
                .aer-diff-line.aer-diff-same{color:var(--shaft-text)}
                .aer-diff-line.aer-diff-truncated{color:var(--shaft-text-muted);font-style:italic}
                .aer-diff-marker{flex:0 0 auto;font-weight:700}
                .aer-raw pre{white-space:pre-wrap;overflow-wrap:anywhere;margin:4px 0}
                .aer-raw dt{font-weight:700;color:var(--shaft-text-muted);margin-top:8px}
                .aer-raw dd{margin:0}
                """;
    }

    private enum DiffKind {
        SAME(" ", "aer-diff-same"),
        ADDED("+", "aer-diff-added"),
        REMOVED("-", "aer-diff-removed");

        private final String marker;
        private final String cssClass;

        DiffKind(String marker, String cssClass) {
            this.marker = marker;
            this.cssClass = cssClass;
        }

        String marker() {
            return marker;
        }

        String cssClass() {
            return cssClass;
        }
    }

    private record DiffLine(DiffKind kind, String text) {
    }

    private record Capped(String text, boolean truncated, int truncatedChars) {
        String withMarker() {
            return truncated ? text + "\n… [truncated " + truncatedChars + " characters]" : text;
        }
    }
}
