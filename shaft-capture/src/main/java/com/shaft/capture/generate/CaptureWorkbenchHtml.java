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
                    body { font-family: system-ui, sans-serif; margin: 0; color: #1f2933; background: #f7f9fb; }
                    header, main { max-width: 1180px; margin: 0 auto; padding: 20px; }
                    header { background: #102a43; color: white; max-width: none; }
                    section { margin: 16px 0; padding: 16px; background: white; border: 1px solid #d9e2ec; border-radius: 8px; }
                    label { display: block; margin: 8px 0 4px; font-weight: 600; }
                    input, select, textarea { width: 100%%; box-sizing: border-box; padding: 8px; border: 1px solid #bcccdc; border-radius: 4px; font: inherit; }
                    textarea { min-height: 420px; font-family: ui-monospace, Consolas, monospace; font-size: 13px; }
                    button { margin-top: 10px; padding: 8px 12px; border: 0; border-radius: 4px; background: #0b69a3; color: white; font-weight: 700; cursor: pointer; }
                    pre { white-space: pre-wrap; padding: 10px; background: #102a43; color: #f0f4f8; border-radius: 4px; }
                    table { width: 100%%; border-collapse: collapse; font-size: 14px; }
                    th, td { border-bottom: 1px solid #d9e2ec; padding: 8px; text-align: left; vertical-align: top; }
                    .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 12px; }
                  </style>
                </head>
                <body>
                  <header>
                    <h1>SHAFT Capture Workbench</h1>
                    <p>Session %s | review score %d | status %s</p>
                  </header>
                  <main>
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
                        <div><label for="checkpointKind">Kind</label><select id="checkpointKind"><option>USER_MARKER</option><option>ASSERTION</option><option>PAGE_TRANSITION</option><option>RECOVERY</option></select></div>
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
                      <table>
                        <thead><tr><th>Category</th><th>Feature</th><th>Playwright</th><th>SHAFT</th><th>Notes</th></tr></thead>
                        <tbody>
                        %s
                        </tbody>
                      </table>
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
                escape(source),
                escape(sourcePath == null ? "" : sourcePath.toString()),
                escape(String.join("\n", review.suggestions())),
                rows,
                javascriptString(sourcePath == null ? "" : sourcePath.toString()));
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
