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
    private static final List<String> REQUIRED_ASSET_PATHS = List.of(
            "driver/package/lib/vite/traceViewer/index.html",
            "driver/package/lib/vite/traceViewer/snapshot.html",
            "driver/package/lib/vite/traceViewer/sw.bundle.js");

    @Test
    void packagedViewerDeclaresItsPinnedUpstreamOrigin() throws IOException {
        JsonNode manifest = JSON.readTree(requiredResource(ORIGIN_MANIFEST));
        String manifestVersion = manifest.path("version").asText();
        String expectedTag = "v" + manifestVersion;

        Assert.assertEquals(manifest.path("schemaVersion").asText(), "1.0");
        Assert.assertEquals(manifest.path("name").asText(), "Microsoft Playwright Trace Viewer");
        Assert.assertTrue(manifestVersion.matches("\\d+\\.\\d+\\.\\d+"),
                "Manifest version must be semantic x.y.z: " + manifestVersion);
        Assert.assertEquals(manifest.path("tag").asText(), expectedTag);
        Assert.assertTrue(manifest.path("commit").asText().matches("[0-9a-f]{40}"),
                "Manifest commit must be a 40-hex SHA: " + manifest.path("commit").asText());
        Assert.assertEquals(manifest.path("repository").asText(), "https://github.com/microsoft/playwright");
        Assert.assertEquals(manifest.path("sourcePath").asText(), "packages/trace-viewer");
        Assert.assertEquals(manifest.path("license").asText(), "Apache-2.0");
        Assert.assertEquals(manifest.path("licenseUrl").asText(),
                "https://github.com/microsoft/playwright/blob/" + expectedTag + "/LICENSE");
        Assert.assertEquals(manifest.path("licenseResource").asText(),
                "META-INF/shaft/licenses/playwright-" + manifestVersion + "-LICENSE.txt");
        Assert.assertEquals(manifest.path("noticeResource").asText(),
                "META-INF/shaft/licenses/playwright-" + manifestVersion + "-NOTICE.txt");
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

        List<ExpectedAsset> assets = List.copyOf(manifest.path("requiredDriverAssets").values()).stream()
                .map(asset -> new ExpectedAsset(asset.path("path").asText(), asset.path("size").asInt(),
                        asset.path("sha256").asText()))
                .toList();
        Assert.assertEquals(assets.stream().map(ExpectedAsset::path).toList(), REQUIRED_ASSET_PATHS);
        String actualDriverVersion = driverVersion();
        Assert.assertTrue(actualDriverVersion.matches("\\d+\\.\\d+\\.\\d+"),
                "Playwright driver version must be semantic x.y.z: " + actualDriverVersion);
        Assert.assertEquals(actualDriverVersion.split("\\.")[0], manifestVersion.split("\\.")[0],
                "Playwright driver major version drifted from provenance manifest: driver="
                        + actualDriverVersion + ", manifest=" + manifestVersion);
        boolean exactPin = actualDriverVersion.equals(manifestVersion);
        for (ExpectedAsset asset : assets) {
            Assert.assertTrue(asset.size() > 0, "Manifest asset size must be positive: " + asset.path());
            Assert.assertTrue(asset.sha256().matches("[0-9a-f]{64}"),
                    "Manifest asset sha256 must be 64-hex: " + asset.path());
            try (InputStream input = contextLoader().getResourceAsStream(asset.path())) {
                Assert.assertNotNull(input,
                        "The Playwright driver dependency is missing required trace-viewer asset " + asset.path());
                byte[] bytes = input.readAllBytes();
                Assert.assertTrue(bytes.length > 0, "Packaged trace-viewer asset is empty: " + asset.path());
                if (exactPin) {
                    Assert.assertEquals(bytes.length, asset.size(), "Asset size drift: " + asset.path());
                    Assert.assertEquals(sha256(bytes), asset.sha256(), "Asset digest drift: " + asset.path());
                }
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
