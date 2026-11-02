package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Offline render-limit failures for a hand-built Playwright trace. No browser, device, or network call.
 */
public class PlaywrightTraceOfflineRenderLimitTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final String PAGE = "https://shop.test/page";

    @Test
    public void longTextPastTheRenderBudgetIsOmitted() throws Exception {
        Path archive = pageArchive("long@1", element("HTML", object(), text("x".repeat(64))), Map.of());
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            var documents = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded, List.of("long@1"), 8);
            Assert.assertEquals(documents.get("long@1").status(), "omitted-budget");
            Assert.assertEquals(documents.get("long@1").content(), "");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    public void stylesheetLargerThanTheRemainingBudgetIsOmitted() throws Exception {
        Map<String, byte[]> css = Map.of(
                "resources/sha-app", "x".repeat(4_000).getBytes(StandardCharsets.UTF_8));
        Path archive = pageArchive("wide@1", element("HTML", object(),
                element("LINK", object("rel", "stylesheet", "href", "app.css"))), css);
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            var documents = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded, List.of("wide@1"), 400);
            Assert.assertEquals(documents.get("wide@1").status(), "omitted-budget");
            Assert.assertEquals(documents.get("wide@1").content(), "");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    public void nestedStylesheetLargerThanTheCssBufferIsOmitted() throws Exception {
        Map<String, byte[]> css = Map.of(
                "resources/sha-app", "@import \"big.css\";".getBytes(StandardCharsets.UTF_8),
                "resources/sha-big", "y".repeat(3_000).getBytes(StandardCharsets.UTF_8));
        Path archive = pageArchive("nest@1", element("HTML", object(),
                element("LINK", object("rel", "stylesheet", "href", "app.css"))), css);
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            var documents = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded, List.of("nest@1"), 2_000);
            Assert.assertEquals(documents.get("nest@1").status(), "omitted-budget");
            Assert.assertEquals(documents.get("nest@1").content(), "");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    private static Path pageArchive(String snapshotName, ArrayNode html, Map<String, byte[]> resources) throws IOException {
        Map<String, byte[]> entries = new LinkedHashMap<>();
        entries.put("0-trace.trace", ndjson(contextOptions(), frame(snapshotName, html)));
        ObjectNode[] records = new ObjectNode[resources.size()];
        int index = 0;
        if (resources.containsKey("resources/sha-app")) {
            records[index++] = resource("https://shop.test/app.css", "sha-app", "text/css");
        }
        if (resources.containsKey("resources/sha-big")) {
            records[index] = resource("https://shop.test/big.css", "sha-big", "text/css");
        }
        if (records.length > 0 && records[0] != null) {
            entries.put("0-trace.network", ndjson(java.util.Arrays.copyOf(records, index + (resources.containsKey("resources/sha-big") ? 1 : 0))));
        } else {
            entries.put("0-trace.network", ndjson(contextOptions()));
        }
        entries.putAll(resources);
        return zip(entries);
    }

    private static Path zip(Map<String, byte[]> entries) throws IOException {
        Path archive = Files.createTempFile("playwright-offline-limit", ".zip");
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
            for (var entry : entries.entrySet()) {
                output.putNextEntry(new ZipEntry(entry.getKey()));
                output.write(entry.getValue());
                output.closeEntry();
            }
        }
        return archive;
    }

    private static byte[] ndjson(ObjectNode... records) {
        StringBuilder builder = new StringBuilder();
        for (ObjectNode record : records) {
            if (record != null) {
                builder.append(JSON.writeValueAsString(record)).append('\n');
            }
        }
        return builder.toString().getBytes(StandardCharsets.UTF_8);
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

    private static ObjectNode frame(String name, ArrayNode html) {
        ObjectNode snapshot = JSON.createObjectNode();
        snapshot.put("snapshotName", name);
        snapshot.put("frameId", "frame@1");
        snapshot.put("frameUrl", PAGE);
        snapshot.put("doctype", "html");
        snapshot.put("timestamp", 50);
        snapshot.put("isMainFrame", true);
        snapshot.set("html", html);
        ObjectNode record = JSON.createObjectNode();
        record.put("type", "frame-snapshot");
        record.set("snapshot", snapshot);
        return record;
    }

    private static ObjectNode resource(String url, String sha1, String mime) {
        ObjectNode content = JSON.createObjectNode();
        content.put("_sha1", sha1);
        content.put("mimeType", mime);
        ObjectNode response = JSON.createObjectNode();
        response.put("status", 200);
        response.set("content", content);
        ObjectNode request = JSON.createObjectNode();
        request.put("url", url);
        request.put("method", "GET");
        ObjectNode snapshot = JSON.createObjectNode();
        snapshot.set("request", request);
        snapshot.set("response", response);
        snapshot.put("_frameref", "frame@1");
        snapshot.put("_monotonicTime", 1);
        ObjectNode record = JSON.createObjectNode();
        record.put("type", "resource-snapshot");
        record.set("snapshot", snapshot);
        return record;
    }

    private static ArrayNode element(String name, ObjectNode attributes, tools.jackson.databind.JsonNode... children) {
        ArrayNode element = JSON.createArrayNode();
        element.add(name);
        element.add(attributes);
        for (var child : children) {
            element.add(child);
        }
        return element;
    }

    private static ObjectNode object(String... pairs) {
        ObjectNode object = JSON.createObjectNode();
        for (int index = 0; index + 1 < pairs.length; index += 2) {
            object.put(pairs[index], pairs[index + 1]);
        }
        return object;
    }

    private static tools.jackson.databind.JsonNode text(String value) {
        return JSON.getNodeFactory().textNode(value);
    }
}
