package com.shaft.tools.io.internal;

import com.shaft.tools.internal.support.ReportHtmlTheme;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/**
 * Renders the single-file SHAFT trace viewer from the versioned front-end resources under
 * {@code META-INF/shaft/trace-viewer/}. The trace JSON is embedded gzip-compressed and base64-encoded,
 * and the page decodes it with {@code DecompressionStream}, so the file stays self-contained and offline.
 */
final class TraceViewerHtml {
    private static final String RESOURCE_ROOT = "META-INF/shaft/trace-viewer/";
    private static final String DATA_OPEN = "<pre hidden id=\"trace-data\" data-encoding=\"gzip+base64\">";
    private static final String TRUNCATION_OPEN = "<pre hidden id=\"trace-truncation\">";
    private static final String DATA_PLACEHOLDER = "@TRACE_DATA@";
    private static final String TRUNCATION_PLACEHOLDER = "@TRACE_TRUNCATION@";
    private static volatile String template;

    private TraceViewerHtml() {
    }

    static String render(String json, List<String> omitted) {
        String page = template();
        int data = page.indexOf(DATA_PLACEHOLDER);
        page = page.substring(0, data) + encode(json) + page.substring(data + DATA_PLACEHOLDER.length());
        int truncation = page.indexOf(TRUNCATION_PLACEHOLDER);
        return page.substring(0, truncation) + escapeHtml(truncationJson(omitted))
                + page.substring(truncation + TRUNCATION_PLACEHOLDER.length());
    }

    /** Decodes the trace JSON embedded in a rendered viewer page. */
    static String embeddedJson(String html) {
        int start = html.indexOf(DATA_OPEN) + DATA_OPEN.length();
        return decode(html.substring(start, html.indexOf("</pre>", start)));
    }

    /** Replaces the trace JSON embedded in a rendered viewer page. */
    static String withEmbeddedJson(String html, String json) {
        int start = html.indexOf(DATA_OPEN) + DATA_OPEN.length();
        return html.substring(0, start) + encode(json) + html.substring(html.indexOf("</pre>", start));
    }

    /** Returns the truncation JSON embedded in a rendered viewer page. */
    static String embeddedTruncation(String html) {
        int start = html.indexOf(TRUNCATION_OPEN) + TRUNCATION_OPEN.length();
        return html.substring(start, html.indexOf("</pre>", start));
    }

    static String encode(String json) {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (GZIPOutputStream gzip = new GZIPOutputStream(bytes)) {
            gzip.write(json.getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return Base64.getEncoder().encodeToString(bytes.toByteArray());
    }

    static String decode(String encoded) {
        try (InputStream gzip = new GZIPInputStream(new ByteArrayInputStream(
                Base64.getDecoder().decode(encoded.strip())))) {
            return new String(gzip.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static String template() {
        String cached = template;
        if (cached == null) {
            String mainScript = resource("viewer-core.js") + "\n" + resource("viewer.js");
            if (mainScript.contains("</script")) {
                throw new IllegalStateException("Trace viewer scripts must not contain a closing script tag.");
            }
            cached = resource("index.html")
                    .replace("/*@SHAFT_THEME_STYLE@*/", ReportHtmlTheme.style())
                    .replace("/*@VIEWER_CSS@*/", resource("viewer.css"))
                    .replace("/*@VIEWER_CORE_JS@*/\n/*@VIEWER_JS@*/", mainScript)
                    .replace("/*@VIEWER_BOOTSTRAP_JS@*/", resource("bootstrap.js"));
            template = cached;
        }
        return cached;
    }

    private static String resource(String name) {
        try (InputStream input = TraceViewerHtml.class.getClassLoader().getResourceAsStream(RESOURCE_ROOT + name)) {
            if (input == null) {
                throw new IllegalStateException("Missing trace viewer resource " + RESOURCE_ROOT + name);
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static String truncationJson(List<String> omitted) {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < omitted.size(); i++) {
            json.append(i > 0 ? ", " : "").append('"').append(JsonEscapes.escape(omitted.get(i))).append('"');
        }
        return json.append(']').toString();
    }

    private static String escapeHtml(String value) {
        return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }
}
