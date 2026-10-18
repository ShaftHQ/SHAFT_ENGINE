package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Offline render of a hand-built Playwright trace. No browser, device, or network call.
 */
public class PlaywrightTraceOfflineRenderTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final byte[] PNG = new byte[]{(byte) 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a};
    private static final String PAGE = "https://shop.test/page";
    private static final String LOGO = "https://shop.test/logo.png";

    @Test
    public void rendersEmbeddedResourcesStylesAndRedactsPasswordFields() throws Exception {
        Path archive = archive();
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            String document = PlaywrightTraceOfflineAdapter.snapshotDocument(loaded, "rich@1");

            Assert.assertTrue(document.startsWith("<!DOCTYPE html>"), document);
            Assert.assertTrue(document.contains("a&lt;b&gt;&amp;"), document);
            Assert.assertTrue(document.contains("value=\"********\""), document);
            Assert.assertFalse(document.contains("secret"), document);
            Assert.assertFalse(document.contains("alert(1)"), document);
            Assert.assertFalse(document.contains("<SCRIPT"), document);
            Assert.assertFalse(document.contains("<META"), document);
            Assert.assertFalse(document.contains("<IFRAME"), document);
            Assert.assertTrue(document.contains("href=\"#\""), document);
            Assert.assertTrue(document.contains("action=\"\""), document);
            Assert.assertTrue(document.contains("cite=\"\""), document);
            Assert.assertTrue(document.contains("alt=\"say &quot;hi&quot;\""), document);
            Assert.assertTrue(document.contains("data:image/png;base64,"), document);
            Assert.assertTrue(document.contains("data:application/octet-stream;base64,"), document);
            Assert.assertTrue(document.contains("<style>"), document);
            Assert.assertTrue(document.contains(".b{color:blue}"), document);
            Assert.assertTrue(document.contains("@layer theme{"), document);
            Assert.assertTrue(document.contains("@supports (display: flex){"), document);
            Assert.assertTrue(document.contains("@media screen{"), document);
            Assert.assertTrue(document.contains("\\3C /style"), document);
            Assert.assertTrue(document.contains("url(data:,"), document);
            Assert.assertFalse(document.contains("</IMG>"), document);
            Assert.assertFalse(document.contains("</BR>"), document);

            String reused = PlaywrightTraceOfflineAdapter.snapshotDocument(loaded, "ref@1");
            Assert.assertTrue(reused.contains("reused"), reused);
            Assert.assertTrue(reused.contains("data:image/png;base64,"), reused);
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    public void reportsUnavailableMalformedAndBudgetStatuses() throws Exception {
        Path archive = archive();
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            var documents = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded,
                    java.util.List.of("missing@1", "child@1", "bad@1", "rich@1"), 1024);

            Assert.assertEquals(documents.get("missing@1").status(), "unavailable");
            Assert.assertEquals(documents.get("child@1").status(), "unavailable");
            Assert.assertEquals(documents.get("bad@1").status(), "malformed");
            Assert.assertEquals(documents.get("rich@1").status(), "available");
            Assert.assertTrue(documents.get("rich@1").content().contains("a&lt;b&gt;&amp;"));

            var overBudget = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded,
                    java.util.List.of("rich@1"), 0);
            Assert.assertEquals(overBudget.get("rich@1").status(), "omitted-budget");
            Assert.assertEquals(overBudget.get("rich@1").content(), "");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    public void renderWrapsTheDocumentInASandboxedIframe() throws Exception {
        Path archive = archive();
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            String page = PlaywrightTraceOfflineAdapter.render(loaded, "base@1");
            Assert.assertTrue(page.contains("id=\"playwright-snapshot\""), page);
            Assert.assertTrue(page.contains("src=\"data:text/html;charset=utf-8;base64,"), page);
            Assert.assertTrue(page.contains("sandbox=\"\""), page);
            Assert.assertFalse(page.contains("reused"), page);
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    private static Path archive() throws IOException {
        Map<String, byte[]> entries = new LinkedHashMap<>();
        entries.put("0-trace.trace", traceBytes());
        entries.put("0-trace.network", ndjson(false,
                resource(LOGO, "GET", 304, "sha-other", "image/png", "frame@1", 1),
                resource(LOGO, "POST", 200, "sha-other", "image/png", "frame@1", 2),
                resource(LOGO, "GET", 200, "sha-other", "image/png", "frame@9", 3),
                resource(LOGO, "GET", 200, "sha-logo", "image/png", "frame@1", 4),
                resource("https://shop.test/app.css", "GET", 200, "sha-css", "text/css", "frame@1", 5),
                resource("https://shop.test/extra.css", "GET", 200, "sha-extra", "text/css", "frame@1", 6),
                resource("https://shop.test/pixel.png", "GET", 200, "sha-pixel", "image/png", "frame@1", 7),
                resource("https://shop.test/blank.bin", "GET", 200, "sha-blank", "", "frame@1", 8),
                resource(LOGO, "GET", 200, "sha-logo-late", "image/png", "frame@1", 250),
                resource(LOGO, "GET", 200, "sha-logo", "image/png", "frame@1", 10000)));
        entries.put("resources/sha-logo", PNG);
        entries.put("resources/sha-logo-late", PNG);
        entries.put("resources/sha-other", PNG);
        entries.put("resources/sha-pixel", PNG);
        entries.put("resources/sha-blank", "xyz".getBytes(StandardCharsets.UTF_8));
        entries.put("resources/sha-css", """
                @import url("extra.css");
                .a{background:url(pixel.png)}
                """.getBytes(StandardCharsets.UTF_8));
        entries.put("resources/sha-extra", ".b{color:blue}".getBytes(StandardCharsets.UTF_8));
        Path archive = Files.createTempFile("playwright-offline-render", ".zip");
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
            for (var entry : entries.entrySet()) {
                output.putNextEntry(new ZipEntry(entry.getKey()));
                output.write(entry.getValue());
                output.closeEntry();
            }
        }
        return archive;
    }

    private static byte[] traceBytes() {
        ArrayNode logoOverride = JSON.createArrayNode().add(override(LOGO, "sha-logo", null));
        ArrayNode refOverride = JSON.createArrayNode().add(override(LOGO, "", 2));
        ObjectNode base = frame("base@1", "frame@1", PAGE, "html", 50, true,
                element("HTML", object(), text("reused")), logoOverride);
        ObjectNode rich = frame("rich@1", "frame@1", PAGE, "html", 100, true, richHtml(), logoOverride);
        ObjectNode otherFrame = frame("rich@1", "frame@2", "https://shop.test/child", null, 100, false,
                element("HTML", object(), text("child-only")), null);
        ObjectNode child = frame("child@1", "frame@2", "https://shop.test/child", null, 110, false,
                element("HTML", object(), text("child-only")), null);
        ObjectNode ref = frame("ref@1", "frame@1", PAGE, null, 300, true,
                element("HTML", object(), reference(2, 0), element("IMG", object("src", "logo.png")), reference(5, 0)),
                refOverride);
        ObjectNode bad = frame("bad@1", "frame@1", PAGE, null, 400, true,
                element("HTML", object(), reference(0, 0)), null);
        return ndjson(true, contextOptions(), base, rich, otherFrame, child, ref, bad);
    }

    private static ArrayNode richHtml() {
        ObjectNode input = object(
                "type", "password",
                "__playwright_value_", "secret",
                "onclick", "nope",
                "srcdoc", "x");
        ObjectNode image = object("src", "logo.png", "alt", "say \"hi\"");
        ObjectNode anchor = object("href", "https://example.test/go", "ping", "https://example.test/p");
        ObjectNode form = object("action", "https://shop.test/post");
        ObjectNode styled = object("style",
                "@import \"extra.css\" layer(theme) supports(display: flex) screen;color:url(nope.png)");
        ObjectNode span = object("cite", "missing.png", "sandbox", "allow", "srcset", "a,b");
        return element("HTML", object("lang", "en"), element("BODY", object("background", "blank.bin"),
                text("a<b>&\"c"),
                element("IMG", image),
                element("INPUT", input),
                element("SCRIPT", object(), text("alert(1)")),
                element("META", object("charset", "utf-8")),
                element("A", anchor, text("go")),
                element("FORM", form),
                element("DIV", styled,
                        element("BR", object()),
                        element("LINK", object("rel", "stylesheet", "href", "app.css")),
                        element("STYLE", object(), text("p{content:\"</style>\"}"))),
                element("IFRAME", object("src", "https://shop.test/frame")),
                JSON.createArrayNode(),
                element("1bad", object()),
                element("SPAN", span)));
    }

    private static ObjectNode contextOptions() {
        ObjectNode record = JSON.createObjectNode();
        record.put("version", 8);
        record.put("type", "context-options");
        record.put("origin", "library");
        record.put("wallTime", 10000);
        record.put("monotonicTime", 100);
        return record;
    }

    private static ObjectNode frame(String name, String frameId, String url, String doctype, double timestamp,
                                    boolean mainFrame, ArrayNode html, ArrayNode overrides) {
        ObjectNode snapshot = JSON.createObjectNode();
        snapshot.put("snapshotName", name);
        snapshot.put("frameId", frameId);
        snapshot.put("frameUrl", url);
        if (doctype != null) {
            snapshot.put("doctype", doctype);
        }
        snapshot.put("timestamp", timestamp);
        snapshot.put("isMainFrame", mainFrame);
        snapshot.set("html", html);
        if (overrides != null) {
            snapshot.set("resourceOverrides", overrides);
        }
        ObjectNode record = JSON.createObjectNode();
        record.put("type", "frame-snapshot");
        record.set("snapshot", snapshot);
        return record;
    }

    private static ObjectNode resource(String url, String method, int status, String sha1, String mimeType,
                                       String frameId, double time) {
        ObjectNode content = JSON.createObjectNode();
        content.put("_sha1", sha1);
        content.put("mimeType", mimeType);
        ObjectNode response = JSON.createObjectNode();
        response.put("status", status);
        response.set("content", content);
        ObjectNode request = JSON.createObjectNode();
        request.put("url", url);
        request.put("method", method);
        ObjectNode snapshot = JSON.createObjectNode();
        snapshot.set("request", request);
        snapshot.set("response", response);
        snapshot.put("_frameref", frameId);
        snapshot.put("_monotonicTime", time);
        ObjectNode record = JSON.createObjectNode();
        record.put("type", "resource-snapshot");
        record.set("snapshot", snapshot);
        return record;
    }

    private static ObjectNode override(String url, String sha1, Integer ref) {
        ObjectNode override = JSON.createObjectNode();
        override.put("url", url);
        override.put("sha1", sha1);
        if (ref != null) {
            override.put("ref", ref);
        }
        return override;
    }

    private static ArrayNode element(String name, ObjectNode attributes, JsonNode... children) {
        ArrayNode element = JSON.createArrayNode();
        element.add(name);
        element.add(attributes);
        for (JsonNode child : children) {
            element.add(child);
        }
        return element;
    }

    private static ObjectNode object(String... pairs) {
        ObjectNode object = JSON.createObjectNode();
        for (int index = 0; index < pairs.length; index += 2) {
            object.put(pairs[index], pairs[index + 1]);
        }
        return object;
    }

    private static JsonNode text(String value) {
        return JSON.getNodeFactory().textNode(value);
    }

    private static ArrayNode reference(int delta, int nodeIndex) {
        ArrayNode pair = JSON.createArrayNode();
        pair.add(delta);
        pair.add(nodeIndex);
        ArrayNode reference = JSON.createArrayNode();
        reference.add(pair);
        return reference;
    }

    private static byte[] ndjson(boolean carriageReturn, JsonNode... records) {
        StringBuilder builder = new StringBuilder();
        for (int index = 0; index < records.length; index++) {
            builder.append(JSON.writeValueAsString(records[index]));
            builder.append(index == 0 && carriageReturn ? "\r\n\n" : "\n");
        }
        return builder.toString().getBytes(StandardCharsets.UTF_8);
    }
}
