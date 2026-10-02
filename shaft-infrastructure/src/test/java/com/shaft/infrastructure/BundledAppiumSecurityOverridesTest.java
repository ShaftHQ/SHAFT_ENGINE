package com.shaft.infrastructure;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Appium 3.6.0 pins axios 1.18.1 exactly, so Dependabot security updates for axios
 * are impossible without an npm override (#6358).
 */
class BundledAppiumSecurityOverridesTest {
    @ParameterizedTest
    @ValueSource(strings = {"appium", "appium-ios", "appium-windows"})
    void appiumBundlesOverrideAxiosToThePatchedRelease(String bundle) throws Exception {
        String root = "/com/shaft/infrastructure/" + bundle + "/";
        String manifest = read(root + "package.json");
        String lock = read(root + "package-lock.json").replace("\r\n", "\n");

        assertTrue(manifest.contains("\"axios\": \"1.20.0\""), bundle + " package.json overrides axios");
        assertTrue(lock.matches("(?s).*\"node_modules/axios\"\\s*:\\s*\\{\\s*\"version\"\\s*:\\s*\"1\\.20\\.0\".*"),
                bundle + " lockfile resolves axios 1.20.0");
        assertTrue(!lock.contains("axios-1.18.1.tgz"), bundle + " lockfile has no vulnerable axios 1.18.1");
    }

    private static String read(String resource) throws Exception {
        try (var input = BundledAppiumSecurityOverridesTest.class.getResourceAsStream(resource)) {
            return new String(Objects.requireNonNull(input, resource).readAllBytes(), StandardCharsets.UTF_8);
        }
    }
}
