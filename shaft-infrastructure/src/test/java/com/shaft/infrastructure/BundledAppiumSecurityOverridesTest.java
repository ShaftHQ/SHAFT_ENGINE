package com.shaft.infrastructure;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Appium 3.6.0 pins axios 1.18.1 exactly, so Dependabot security updates for axios
 * are impossible without an npm override (#6358).
 */
class BundledAppiumSecurityOverridesTest {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final Pattern SEMVER = Pattern.compile("([0-9]+)\\.([0-9]+)\\.([0-9]+)");

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

    /**
     * npm overrides cannot reach a dependency's bundled tree, so a bumped package can ship an
     * older copy of an overridden (security-pinned) package; appium-uiautomator2-driver 8.7.0
     * bundles axios 1.19.0 (#6382). No copy may resolve below its override.
     */
    @ParameterizedTest
    @ValueSource(strings = {"appium", "appium-ios", "appium-windows", "lighthouse", "reporting"})
    void everyCopyOfAnOverriddenPackageResolvesAtOrAboveItsOverride(String bundle) throws Exception {
        String root = "/com/shaft/infrastructure/" + bundle + "/";
        JsonNode overrides = JSON.readTree(read(root + "package.json")).path("overrides");
        JsonNode packages = JSON.readTree(read(root + "package-lock.json")).path("packages");
        List<String> belowOverride = new ArrayList<>();
        for (Map.Entry<String, JsonNode> override : overrides.properties()) {
            for (Map.Entry<String, JsonNode> entry : packages.properties()) {
                String version = entry.getValue().path("version").asText();
                if (entry.getKey().endsWith("node_modules/" + override.getKey())
                        && compareVersions(version, override.getValue().asText()) < 0) {
                    belowOverride.add(entry.getKey() + "@" + version + " < " + override.getValue().asText());
                }
            }
        }
        assertEquals(List.of(), belowOverride, bundle + " lockfile escapes its npm overrides");
    }

    private static int compareVersions(String left, String right) {
        Matcher a = SEMVER.matcher(left);
        Matcher b = SEMVER.matcher(right);
        assertTrue(a.lookingAt() && b.lookingAt(), "not semver: " + left + " / " + right);
        for (int group = 1; group <= 3; group++) {
            int difference = compareDigits(a.group(group), b.group(group));
            if (difference != 0) {
                return difference;
            }
        }
        return 0;
    }

    private static int compareDigits(String left, String right) {
        String a = left.replaceFirst("^0+(?=.)", "");
        String b = right.replaceFirst("^0+(?=.)", "");
        return a.length() != b.length() ? Integer.compare(a.length(), b.length()) : a.compareTo(b);
    }

    private static String read(String resource) throws Exception {
        try (var input = BundledAppiumSecurityOverridesTest.class.getResourceAsStream(resource)) {
            return new String(Objects.requireNonNull(input, resource).readAllBytes(), StandardCharsets.UTF_8);
        }
    }
}
