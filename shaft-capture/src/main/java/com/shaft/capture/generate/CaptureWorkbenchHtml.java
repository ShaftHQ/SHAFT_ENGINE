package com.shaft.capture.generate;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Writes the local Capture recording and generated-code review workbench.
 */
final class CaptureWorkbenchHtml {
    private CaptureWorkbenchHtml() {
        throw new IllegalStateException("Utility class");
    }

    static void write(
            Path path,
            Path sourcePath,
            CaptureGenerationReport report,
            CaptureReview review,
            boolean overwrite) {
        if (path == null || (Files.exists(path) && !overwrite)) {
            return;
        }
        try {
            Files.createDirectories(path.toAbsolutePath().normalize().getParent());
            Files.writeString(path, html(sourcePath, report, review), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Capture workbench HTML could not be written.", exception);
        }
    }

    private static String html(Path sourcePath, CaptureGenerationReport report, CaptureReview review)
            throws IOException {
        String source = sourcePath == null || !Files.isRegularFile(sourcePath)
                ? ""
                : Files.readString(sourcePath, StandardCharsets.UTF_8);
        StringBuilder rows = new StringBuilder();
        for (CodegenFeatureCatalog.Feature feature : CodegenFeatureCatalog.features()) {
            rows.append("<tr><td>").append(escape(feature.category())).append("</td><td>")
                    .append(escape(feature.name())).append("</td><td><code>")
                    .append(escape(feature.playwrightControl())).append("</code></td><td>")
                    .append(escape(feature.shaftSupport())).append("</td><td>")
                    .append(escape(feature.notes())).append("</td></tr>\n");
        }
        return """
                <!doctype html>
                <html lang="en">
                <head>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <title>SHAFT Capture Workbench</title>
                  <style>
                    :root { color-scheme: light dark; --shaft-primary:#006ec0; --shaft-primary-rgb:0,110,192; --shaft-deep:#102a31; --shaft-deep-alt:#181f2a; --shaft-muted:#c8d6e7; --shaft-on-dark:#ffffff; --shaft-bg:#f7f9fb; --shaft-surface:#ffffff; --shaft-text:#17202a; --shaft-text-muted:#5f6f81; --shaft-border:#d9e2ec; --shaft-shadow:0 18px 45px rgba(24,31,42,.12); }
                    @media (prefers-color-scheme: dark) { :root { --shaft-primary:#4cc2ff; --shaft-primary-rgb:76,194,255; --shaft-deep:#07111f; --shaft-deep-alt:#102a31; --shaft-muted:#dff5f4; --shaft-on-dark:#f5fdff; --shaft-bg:#07111f; --shaft-surface:#102a31; --shaft-text:#f5fdff; --shaft-text-muted:#c8d6e7; --shaft-border:rgba(223,245,244,.24); --shaft-shadow:0 18px 45px rgba(0,0,0,.34); } }
                    * { box-sizing: border-box; }
                    html, body { width: 100%%; min-width: 0; overflow-x: hidden; }
                    body { font-family: "Segoe UI", system-ui, -apple-system, BlinkMacSystemFont, sans-serif; margin: 0; color: var(--shaft-text); background: var(--shaft-bg); font-size: 14px; line-height: 1.5; }
                    header { background: linear-gradient(135deg, var(--shaft-deep), var(--shaft-deep-alt)); color: var(--shaft-on-dark); }
                    header > div, main { width: 100%%; max-width: 1180px; min-width: 0; margin: 0 auto; padding: 22px 24px; }
                    header > div { display: flex; align-items: center; gap: 14px; }
                    .brand-mark { width: 44px; height: 44px; display: inline-grid; place-items: center; border: 1px solid rgba(var(--shaft-primary-rgb), .42); border-radius: 8px; background: rgba(var(--shaft-primary-rgb), .18); font-weight: 700; }
                    h1, h2 { margin: 0; line-height: 1.2; }
                    h1 { font-size: 24px; }
                    h2 { font-size: 17px; }
                    .subtitle { margin: 5px 0 0; color: var(--shaft-muted); }
                    section { margin: 16px 0; padding: 16px; background: var(--shaft-surface); border: 1px solid var(--shaft-border); border-radius: 8px; box-shadow: var(--shaft-shadow); }
                    label { display: block; margin: 8px 0 4px; color: var(--shaft-text-muted); font-weight: 600; }
                    input, select, textarea { width: 100%%; box-sizing: border-box; min-height: 38px; padding: 8px 10px; border: 1px solid var(--shaft-border); border-radius: 8px; background: var(--shaft-surface); color: var(--shaft-text); font: inherit; }
                    textarea { min-height: 420px; font-family: ui-monospace, Consolas, monospace; font-size: 13px; }
                    button { margin-top: 10px; min-height: 38px; padding: 8px 12px; border: 1px solid var(--shaft-primary); border-radius: 8px; background: var(--shaft-primary); color: var(--shaft-on-dark); font-weight: 700; cursor: pointer; }
                    pre { white-space: pre-wrap; overflow-wrap: anywhere; padding: 12px; background: var(--shaft-deep); color: var(--shaft-on-dark); border-radius: 8px; }
                    table { width: 100%%; max-width: 100%%; table-layout: fixed; border-collapse: collapse; font-size: 14px; }
                    th, td { border-bottom: 1px solid var(--shaft-border); padding: 10px 12px; text-align: left; vertical-align: top; overflow-wrap: anywhere; word-break: break-word; }
                    th { background: rgba(var(--shaft-primary-rgb), .08); font-size: 12px; text-transform: uppercase; }
                    .status-chip { display: inline-flex; align-items: center; min-height: 24px; padding: 3px 9px; border: 1px solid var(--shaft-border); border-radius: 999px; background: rgba(var(--shaft-primary-rgb), .08); color: var(--shaft-on-dark); font-size: 12px; font-weight: 700; }
                    .table-wrap { max-width: 100%%; overflow-x: hidden; overflow-y: visible; border: 1px solid var(--shaft-border); border-radius: 8px; }
                    .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 12px; }
                  </style>
                </head>
                <body>
                  <header>
                    <div><span class="brand-mark">S</span><div>
                      <h1>SHAFT Capture Workbench</h1>
                      <p class="subtitle">Session %s · <span class="status-chip">review score %d</span> <span class="status-chip">%s</span></p>
                    </div></div>
                  </header>
                  <main>
                    <section>
                      <h2>Review Summary</h2>
                      <pre>%s</pre>
                    </section>
                    <section>
                      <h2>Record</h2>
                      <div class="grid">
                        <div><label for="url">URL</label><input id="url" value="https://example.com"></div>
                        <div><label for="browser">Browser</label><select id="browser"><option>chrome</option><option>edge</option><option>chromium</option></select></div>
                        <div><label for="viewport">Viewport</label><input id="viewport" placeholder="1280,720"></div>
                        <div><label for="testid">Test id attribute</label><input id="testid" placeholder="data-testid"></div>
                      </div>
                      <button type="button" onclick="recordCommand()">Build record command</button>
                      <pre id="recordOut">capture start --url https://example.com</pre>
                    </section>
                    <section>
                      <h2>Checkpoint</h2>
                      <div class="grid">
                        <div><label for="checkpointText">Description</label><input id="checkpointText" value="assert the current page state"></div>
                        <div><label for="checkpointKind">Kind</label><select id="checkpointKind"><option>USER_MARKER</option><option>ASSERTION</option><option>PAGE_TRANSITION</option><option>RECOVERY</option><option>FLOW_START</option><option>FLOW_END</option></select></div>
                      </div>
                      <button type="button" onclick="checkpointCommand()">Build checkpoint command</button>
                      <pre id="checkpointOut">capture checkpoint --description "assert the current page state" --kind USER_MARKER</pre>
                    </section>
                    <section>
                      <h2>Generated Code</h2>
                      <textarea id="source">%s</textarea>
                      <button type="button" onclick="saveSource()">Save edited source</button>
                      <pre id="saveOut">%s</pre>
                    </section>
                    <section>
                      <h2>Review</h2>
                      <pre>%s</pre>
                    </section>
                    <section>
                      <h2>Playwright Codegen Feature Map</h2>
                      <div class="table-wrap">
                      <table>
                        <thead><tr><th>Category</th><th>Feature</th><th>Playwright</th><th>SHAFT</th><th>Notes</th></tr></thead>
                        <tbody>
                        %s
                        </tbody>
                      </table>
                      </div>
                    </section>
                  </main>
                  <script>
                    const sourcePath = %s;
                    function quote(value) { return '"' + String(value).replaceAll('"', '\\\\"') + '"'; }
                    function recordCommand() {
                      const args = ['capture start --url', quote(url.value), '--browser', browser.value];
                      if (viewport.value.trim()) args.push('--viewport-size', quote(viewport.value.trim()));
                      if (testid.value.trim()) args.push('--test-id-attribute', quote(testid.value.trim()));
                      recordOut.textContent = args.join(' ');
                    }
                    function checkpointCommand() {
                      checkpointOut.textContent = 'capture checkpoint --description ' + quote(checkpointText.value) + ' --kind ' + checkpointKind.value;
                    }
                    async function saveSource() {
                      const blob = new Blob([source.value], {type: 'text/x-java-source'});
                      if (window.showSaveFilePicker) {
                        const handle = await showSaveFilePicker({suggestedName: sourcePath.split(/[\\\\/]/).pop() || 'CapturedJourneyTest.java'});
                        const writable = await handle.createWritable();
                        await writable.write(blob);
                        await writable.close();
                        saveOut.textContent = 'Saved through browser file picker.';
                        return;
                      }
                      const link = document.createElement('a');
                      link.href = URL.createObjectURL(blob);
                      link.download = sourcePath.split(/[\\\\/]/).pop() || 'CapturedJourneyTest.java';
                      link.click();
                      saveOut.textContent = 'Downloaded edited source.';
                    }
                  </script>
                </body>
                </html>
                """.formatted(
                escape(report.sessionId()),
                review.readinessScore(),
                escape(report.status().name()),
                escape(reviewSummary(report, review)),
                escape(source),
                escape(sourcePath == null ? "" : sourcePath.toString()),
                escape(String.join("\n", review.suggestions())),
                rows,
                javascriptString(sourcePath == null ? "" : sourcePath.toString()));
    }

    private static String reviewSummary(CaptureGenerationReport report, CaptureReview review) {
        StringBuilder summary = new StringBuilder();
        summary.append("Status: ").append(report.status()).append('\n');
        summary.append("Readiness: ").append(report.readiness()).append(" (score ")
                .append(review.readinessScore()).append(")").append('\n');
        appendList(summary, "Blockers", review.blockers());
        appendList(summary, "Required inputs", report.requiredUserInputs());
        appendList(summary, "Risks", review.risks());
        appendList(summary, "Findings", review.findings().stream()
                .map(finding -> finding.category() + "/" + finding.severity() + ": " + finding.summary())
                .toList());
        appendList(summary, "Locator decisions", report.locatorDecisions().stream()
                .map(decision -> decision.logicalElementId() + " -> " + decision.strategy()
                        + " \"" + decision.expression() + "\"; alternatives "
                        + decision.alternatives().size())
                .toList());
        appendList(summary, "Control-flow suggestions", report.controlFlowSuggestions().stream()
                .map(suggestion -> suggestion.id() + " " + suggestion.kind() + ": " + suggestion.recommendation())
                .toList());
        summary.append("Code blocks: full class, replay method, POM locator inventory, action sequence");
        if (!report.requiredUserInputs().isEmpty()) {
            summary.append(", setup");
        }
        if (report.readinessWarnings().stream().anyMatch(warning -> warning.toLowerCase().contains("assertion"))) {
            summary.append(", assertion suggestions");
        }
        if (!report.controlFlowSuggestions().isEmpty()) {
            summary.append(", control-flow review");
        }
        return summary.toString();
    }

    private static void appendList(StringBuilder target, String label, java.util.List<String> values) {
        target.append(label).append(':');
        if (values == null || values.isEmpty()) {
            target.append(" none\n");
            return;
        }
        target.append('\n');
        values.forEach(value -> target.append("- ").append(value).append('\n'));
    }

    private static String escape(String value) {
        return (value == null ? "" : value)
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;");
    }

    private static String javascriptString(String value) {
        return "\"" + (value == null ? "" : value)
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n") + "\"";
    }
}
