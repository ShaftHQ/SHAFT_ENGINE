package com.shaft.tools.internal.support;

import java.util.Locale;

/**
 * Shared CSS tokens for SHAFT-generated offline HTML reports.
 */
public final class ReportHtmlTheme {
    private ReportHtmlTheme() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the self-contained SHAFT report stylesheet.
     *
     * @return CSS stylesheet for generated reports
     */
    public static String style() {
        return """
                :root {
                  color-scheme: light dark;
                  --shaft-primary: #006ec0;
                  --shaft-primary-rgb: 0, 110, 192;
                  --shaft-deep: #102a31;
                  --shaft-deep-alt: #181f2a;
                  --shaft-muted: #c8d6e7;
                  --shaft-on-dark: #ffffff;
                  --shaft-on-primary: #ffffff;
                  --shaft-bg: #f7f9fb;
                  --shaft-surface: #ffffff;
                  --shaft-text: #17202a;
                  --shaft-text-muted: #5f6f81;
                  --shaft-border: #d9e2ec;
                  --shaft-pass: #14804a;
                  --shaft-warn: #b7791f;
                  --shaft-fail: #c53030;
                  --shaft-shadow: 0 18px 45px rgba(24, 31, 42, .12);
                }
                @media (prefers-color-scheme: dark) {
                  :root {
                    --shaft-primary: #4cc2ff;
                    --shaft-primary-rgb: 76, 194, 255;
                    --shaft-deep: #07111f;
                    --shaft-deep-alt: #102a31;
                    --shaft-muted: #dff5f4;
                    --shaft-on-dark: #f5fdff;
                    --shaft-on-primary: #07111f;
                    --shaft-bg: #07111f;
                    --shaft-surface: #102a31;
                    --shaft-text: #f5fdff;
                    --shaft-text-muted: #c8d6e7;
                    --shaft-border: rgba(223, 245, 244, .24);
                    --shaft-shadow: 0 18px 45px rgba(0, 0, 0, .34);
                  }
                }
                * { box-sizing: border-box; }
                html {
                  width: 100%;
                  min-width: 0;
                  overflow-x: hidden;
                }
                body {
                  margin: 0;
                  width: 100%;
                  min-width: 0;
                  overflow-x: hidden;
                  background: var(--shaft-bg);
                  color: var(--shaft-text);
                  font-family: "Segoe UI", system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
                  font-size: 14px;
                  line-height: 1.5;
                }
                a { color: var(--shaft-primary); font-weight: 600; text-decoration: none; }
                a:hover { text-decoration: underline; }
                img, video, svg, canvas { max-width: 100%; height: auto; }
                .report-shell { width: 100%; min-height: 100vh; overflow-x: hidden; }
                .report-header {
                  background: linear-gradient(135deg, var(--shaft-deep), var(--shaft-deep-alt));
                  color: var(--shaft-on-dark);
                  padding: 22px 24px;
                }
                .report-header-inner {
                  width: 100%;
                  max-width: 1180px;
                  margin: 0 auto;
                  display: flex;
                  align-items: center;
                  gap: 14px;
                  min-width: 0;
                }
                .brand-mark {
                  width: 44px;
                  height: 44px;
                  display: inline-grid;
                  place-items: center;
                  flex: 0 0 auto;
                  border: 1px solid rgba(var(--shaft-primary-rgb), .42);
                  border-radius: 8px;
                  background: rgba(var(--shaft-primary-rgb), .18);
                  color: var(--shaft-on-dark);
                  font-weight: 700;
                }
                h1, h2, h3 { margin: 0; line-height: 1.2; }
                h1 { font-size: 24px; font-weight: 700; }
                h2 { font-size: 17px; font-weight: 700; }
                h3 { font-size: 15px; font-weight: 700; }
                .subtitle { margin: 5px 0 0; color: var(--shaft-muted); }
                .report-main {
                  width: 100%;
                  max-width: 1180px;
                  min-width: 0;
                  margin: 0 auto;
                  padding: 20px 24px 32px;
                }
                .panel {
                  margin: 0 0 16px;
                  padding: 16px;
                  border: 1px solid var(--shaft-border);
                  border-radius: 8px;
                  background: var(--shaft-surface);
                  box-shadow: var(--shaft-shadow);
                }
                .metric-grid {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
                  gap: 10px;
                  margin: 16px 0;
                }
                .metric-card {
                  padding: 14px;
                  border: 1px solid var(--shaft-border);
                  border-radius: 8px;
                  background: rgba(var(--shaft-primary-rgb), .05);
                }
                .metric-label {
                  color: var(--shaft-text-muted);
                  font-size: 12px;
                  font-weight: 600;
                  text-transform: uppercase;
                }
                .metric-value { margin-top: 4px; font-size: 24px; font-weight: 700; }
                .status-chip {
                  display: inline-flex;
                  align-items: center;
                  max-width: 100%;
                  min-height: 24px;
                  padding: 3px 9px;
                  border: 1px solid var(--shaft-border);
                  border-radius: 999px;
                  background: rgba(var(--shaft-primary-rgb), .08);
                  color: var(--shaft-text);
                  font-size: 12px;
                  font-weight: 700;
                  overflow-wrap: anywhere;
                }
                .status-chip.pass, .status-chip.passed { color: var(--shaft-pass); background: rgba(20, 128, 74, .1); }
                .status-chip.warn, .status-chip.warning, .status-chip.skipped { color: var(--shaft-warn); background: rgba(183, 121, 31, .12); }
                .status-chip.fail, .status-chip.failed, .status-chip.error { color: var(--shaft-fail); background: rgba(197, 48, 48, .1); }
                .toolbar {
                  display: flex;
                  flex-wrap: wrap;
                  align-items: center;
                  gap: 10px;
                  margin: 12px 0;
                }
                input, select, button, textarea {
                  border: 1px solid var(--shaft-border);
                  border-radius: 8px;
                  color: var(--shaft-text);
                  font: inherit;
                }
                input, select, textarea { min-width: 0; background: var(--shaft-surface); }
                input, select { min-height: 38px; padding: 8px 10px; }
                button {
                  min-height: 38px;
                  padding: 8px 12px;
                  background: var(--shaft-primary);
                  color: var(--shaft-on-primary);
                  font-weight: 700;
                  cursor: pointer;
                }
                button.secondary {
                  background: var(--shaft-surface);
                  color: var(--shaft-primary);
                }
                button:hover { filter: brightness(.96); }
                .table-wrap {
                  width: 100%;
                  max-width: 100%;
                  overflow-x: hidden;
                  overflow-y: visible;
                  border: 1px solid var(--shaft-border);
                  border-radius: 8px;
                }
                table {
                  width: 100%;
                  max-width: 100%;
                  table-layout: fixed;
                  border-collapse: collapse;
                  background: var(--shaft-surface);
                }
                th, td {
                  padding: 10px 12px;
                  border-bottom: 1px solid var(--shaft-border);
                  text-align: left;
                  vertical-align: top;
                  overflow-wrap: anywhere;
                  word-break: break-word;
                }
                th {
                  background: rgba(var(--shaft-primary-rgb), .08);
                  color: var(--shaft-text);
                  font-size: 12px;
                  font-weight: 700;
                  text-transform: uppercase;
                }
                tr:hover td { background: rgba(var(--shaft-primary-rgb), .04); }
                pre, code {
                  font-family: ui-monospace, SFMono-Regular, Consolas, "Liberation Mono", monospace;
                }
                pre {
                  max-height: 62vh;
                  overflow: auto;
                  overflow-x: hidden;
                  padding: 12px;
                  border-radius: 8px;
                  background: var(--shaft-deep);
                  color: var(--shaft-on-dark);
                  white-space: pre-wrap;
                  overflow-wrap: anywhere;
                }
                code {
                  color: var(--shaft-primary);
                  overflow-wrap: anywhere;
                }
                details { border: 1px solid var(--shaft-border); border-radius: 8px; background: var(--shaft-surface); }
                summary { cursor: pointer; padding: 12px 14px; font-weight: 700; }
                details > div { padding: 0 14px 14px; }
                .muted { color: var(--shaft-text-muted); }
                .warn { color: var(--shaft-warn); font-weight: 700; }
                .donut-wrap { position: relative; display: inline-grid; place-items: center; margin-top: 8px; }
                .donut-center {
                  position: absolute;
                  padding: 2px 9px;
                  border-radius: 999px;
                  background: var(--shaft-surface);
                  font-size: 18px;
                  font-weight: 700;
                  color: var(--shaft-text);
                }
                .chart-legend {
                  display: flex;
                  flex-wrap: wrap;
                  gap: 4px 14px;
                  margin-top: 10px;
                  font-size: 12px;
                  font-weight: 600;
                  color: var(--shaft-text-muted);
                }
                .legend-item { display: inline-flex; align-items: center; gap: 6px; }
                .legend-swatch {
                  display: inline-block;
                  width: 10px;
                  height: 10px;
                  border-radius: 2px;
                  flex: 0 0 auto;
                }
                .legend-swatch.pass { background: var(--shaft-pass); }
                .legend-swatch.fail { background: var(--shaft-fail); }
                .legend-swatch.warn { background: var(--shaft-warn); }
                @media (max-width: 760px) {
                  .report-header-inner { align-items: flex-start; }
                  .report-main, .report-header { padding-left: 14px; padding-right: 14px; }
                  .metric-grid { grid-template-columns: 1fr; }
                }
                """;
    }

    /**
     * Escapes user-supplied text for safe embedding in generated report HTML.
     *
     * @param value raw text, or {@code null}
     * @return HTML-escaped text, never {@code null}
     */
    public static String escapeHtml(String value) {
        return value == null ? "" : value
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;");
    }

    /**
     * Maps a report status to a CSS class suffix.
     *
     * @param status report status text
     * @return normalized status class
     */
    public static String statusClass(String status) {
        String value = status == null ? "" : status.toLowerCase(Locale.ROOT);
        if (value.contains("pass")) {
            return "passed";
        }
        if (value.contains("warn") || value.contains("skip") || value.contains("not_configured")) {
            return "warn";
        }
        if (value.contains("fail") || value.contains("error")) {
            return "failed";
        }
        return "neutral";
    }
}
