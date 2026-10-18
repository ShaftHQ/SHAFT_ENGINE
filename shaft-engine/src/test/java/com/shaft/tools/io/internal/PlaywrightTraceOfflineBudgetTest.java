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
 * Offline budget failures for a hand-built Playwright trace. No browser, device, or network call.
 */
public class PlaywrightTraceOfflineBudgetTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final byte[] PNG = new byte[]{(byte) 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a};
    private static final String PAGE = "https://shop.test/page";
    private static final int IMPORTS = 17;

    @Test
    public void nestedStylesheetImportsPastTheDepthLimitAreOmitted() throws Exception {
        Path archive = importArchive();
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            var documents = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded, List.of("deep@1"), 1_000_000);
            Assert.assertEquals(documents.get("deep@1").status(), "omitted-budget");
            Assert.assertEquals(documents.get("deep@1").content(), "");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    public void embeddedImagePastTheSnapshotBudgetIsOmitted() throws Exception {
        Path archive = imageArchive();
        try {
            var loaded = PlaywrightTraceArchiveLoader.load(archive);
            String document = PlaywrightTraceOfflineAdapter.snapshotDocument(loaded, "image@1");
            Assert.assertTrue(document.contains("data:image/png;base64,"), document);

            var overBudget = PlaywrightTraceOfflineAdapter.snapshotDocuments(loaded, List.of("image@1"), 40);
            Assert.assertEquals(overBudget.get("image@1").status(), "omitted-budget");
            Assert.assertEquals(overBudget.get("image@1").content(), "");

            var aboveMaximum = PlaywrightTraceOfflineAdapter.snapshotDocuments(
                    loaded, List.of("image@1"), 8 * 1024 * 1024 + 1);
            Assert.assertEquals(aboveMaximum.get("image@1").status(), "omitted-budget");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    private static Path importArchive() throws IOException {
        Map<String, byte[]> entries = new LinkedHashMap<>();
        ObjectNode[] resources = new ObjectNode[IMPORTS + 1];
        for (int level = 0; level <= IMPORTS; level++) {
            String url = "https://shop.test/level-" + level + ".css";
            String sha1 = "sha-level-" + level;
            resources[level] = resource(url, sha1);
            String css = level == IMPORTS ? ".end{color:black}" : "@import \"level-" + (level + 1) + ".css\";";
            entries.put("resources/" + sha1, css.getBytes(StandardCharsets.UTF_8));
        }
        entries.put("0-trace.trace", ndjson(contextOptions(), frame("deep@1", element("HTML", object(),
                element("LINK", object("rel", "stylesheet", "href", "level-0.css"))))));
        entries.put("0-trace.network", ndjson(resources));
        return zip(entries);
    }

    private static Path imageArchive() throws IOException {
        Map<String, byte[]> entries = new LinkedHashMap<>();
        entries.put("0-trace.trace", ndjson(contextOptions(), frame("image@1", element("HTML", object(),
                element("IMG", object("src", "logo.png"))))));
        entries.put("0-trace.network", ndjson(resource("https://shop.test/logo.png", "sha-logo")));
        entries.put("resources/sha-logo", PNG);
        return zip(entries);
    }

    private static Path zip(Map<String, byte[]> entries) throws IOException {
        Path archive = Files.createTempFile("playwright-offline-budget", ".zip");
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
            builder.append(JSON.writeValueAsString(record)).append('\n');
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

    private static ObjectNode resource(String url, String sha1) {
        ObjectNode content = JSON.createObjectNode();
        content.put("_sha1", sha1);
        content.put("mimeType", url.endsWith(".css") ? "text/css" : "image/png");
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
}
