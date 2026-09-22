package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Properties;

class PlaywrightTraceViewerAssetsTest {
    private static final String ORIGIN_MANIFEST = "/META-INF/shaft/playwright-trace-viewer-origin.json";
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final List<ExpectedAsset> EXPECTED_ASSETS = List.of(
            new ExpectedAsset("driver/package/lib/vite/traceViewer/index.html", 2363,
                    "139ac80123fb450cfc0631296e9f01463313ca2fec961b3a247ec6c9528447a2"),
            new ExpectedAsset("driver/package/lib/vite/traceViewer/snapshot.html", 389,
                    "c608c8899c95c5a74d3f556e20146c3d852b9df85ae2854c659a7cc773c9b727"),
            new ExpectedAsset("driver/package/lib/vite/traceViewer/sw.bundle.js", 95459,
                    "37b230329280e28e32b03df20738d2ed3bfd0ea45b408a69213158bb8400adb3"));

    @Test
    void packagedViewerDeclaresItsPinnedUpstreamOrigin() throws IOException {
        JsonNode manifest = JSON.readTree(requiredResource(ORIGIN_MANIFEST));

        Assert.assertEquals(manifest.path("schemaVersion").asText(), "1.0");
        Assert.assertEquals(manifest.path("name").asText(), "Microsoft Playwright Trace Viewer");
        Assert.assertEquals(manifest.path("version").asText(), "1.63.0");
        Assert.assertEquals(manifest.path("tag").asText(), "v1.63.0");
        Assert.assertEquals(manifest.path("commit").asText(),
                "1b025d7e20a026371cd5f98ba0cdce48892737c8");
        Assert.assertEquals(manifest.path("repository").asText(), "https://github.com/microsoft/playwright");
        Assert.assertEquals(manifest.path("sourcePath").asText(), "packages/trace-viewer");
        Assert.assertEquals(manifest.path("license").asText(), "Apache-2.0");
        Assert.assertEquals(manifest.path("licenseUrl").asText(),
                "https://github.com/microsoft/playwright/blob/v1.63.0/LICENSE");
        Assert.assertEquals(manifest.path("licenseResource").asText(),
                "META-INF/shaft/licenses/playwright-1.63.0-LICENSE.txt");
        Assert.assertEquals(manifest.path("noticeResource").asText(),
                "META-INF/shaft/licenses/playwright-1.63.0-NOTICE.txt");
        Assert.assertEquals(strings(manifest.path("intendedIntegrationBoundary")), List.of(
                "packages/trace-viewer", "packages/isomorphic/trace"));
        Assert.assertTrue(manifest.path("bundledUpstreamMaterial").isArray());
        Assert.assertTrue(manifest.path("bundledUpstreamMaterial").isEmpty(),
                "The provenance record must not claim upstream code or assets before they are packaged.");

        String license = requiredResource("/" + manifest.path("licenseResource").asText());
        Assert.assertTrue(license.contains("Apache License"));
        Assert.assertTrue(license.contains("Version 2.0, January 2004"));
        Assert.assertTrue(license.contains("Portions Copyright (c) Microsoft Corporation."));

        String notice = requiredResource("/" + manifest.path("noticeResource").asText());
        Assert.assertTrue(notice.contains("Copyright (c) Microsoft Corporation"));
        Assert.assertTrue(notice.contains("derived from the Puppeteer project"));

        List<JsonNode> assets = List.copyOf(manifest.path("requiredDriverAssets").values());
        Assert.assertEquals(assets.stream()
                        .map(asset -> new ExpectedAsset(asset.path("path").asText(), asset.path("size").asInt(),
                                asset.path("sha256").asText()))
                        .toList(), EXPECTED_ASSETS);
        Assert.assertEquals(driverVersion(), "1.63.0");
        for (ExpectedAsset asset : EXPECTED_ASSETS) {
            try (InputStream input = contextLoader().getResourceAsStream(asset.path())) {
                Assert.assertNotNull(input,
                        "The pinned Playwright driver dependency is missing required trace-viewer asset " + asset.path());
                byte[] bytes = input.readAllBytes();
                Assert.assertEquals(bytes.length, asset.size(), "Asset size drift: " + asset.path());
                Assert.assertEquals(sha256(bytes), asset.sha256(), "Asset digest drift: " + asset.path());
            }
        }
    }

    private static String requiredResource(String name) throws IOException {
        try (InputStream input = PlaywrightTraceViewerAssetsTest.class.getResourceAsStream(name)) {
            Assert.assertNotNull(input, "Required packaged provenance resource is missing: " + name);
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static List<String> strings(JsonNode array) {
        Assert.assertTrue(array.isArray(), "Expected a JSON array but found: " + array);
        List<String> values = new ArrayList<>();
        for (JsonNode value : array.values()) {
            values.add(value.asText());
        }
        return List.copyOf(values);
    }

    private static ClassLoader contextLoader() {
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        return loader == null ? PlaywrightTraceViewerAssetsTest.class.getClassLoader() : loader;
    }

    private static String driverVersion() throws IOException {
        Properties properties = new Properties();
        try (InputStream input = contextLoader().getResourceAsStream(
                "META-INF/maven/com.microsoft.playwright/driver/pom.properties")) {
            Assert.assertNotNull(input, "The Playwright driver dependency metadata is missing.");
            properties.load(input);
        }
        return properties.getProperty("version");
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    private record ExpectedAsset(String path, int size, String sha256) {
    }
}
