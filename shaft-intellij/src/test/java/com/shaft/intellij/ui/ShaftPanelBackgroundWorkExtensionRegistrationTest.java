package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Guards how {@link ShaftPanelBackgroundWorkExtension} reaches every test in this module (issue
 * #4500). The extension is auto-detected through {@code
 * META-INF/services/org.junit.jupiter.api.extension.Extension}, the same mechanism the IntelliJ
 * Platform test framework uses for its own leak checks, so nothing references it by name and its
 * registration can be lost in complete silence — the whole module simply stops tearing down its
 * panels and the flake comes back blamed on unrelated tests.
 *
 * <p>That is not hypothetical: the registration first lived in {@code
 * src/test/resources/META-INF/services/}, and a SHAFT test run regenerates that directory from
 * scratch (Allure and TestNG listener registrations), deleting it. It now lives in this module's own
 * {@code src/test/junit-extensions} resources root, wired in {@code build.gradle.kts}. This test
 * fails the moment that wiring is gone.
 */
class ShaftPanelBackgroundWorkExtensionRegistrationTest {
    @Test
    void theBackgroundWorkExtensionIsRegisteredForAutoDetection() throws IOException {
        assertTrue(registeredJUnitExtensions().contains(ShaftPanelBackgroundWorkExtension.class.getName()),
                "ShaftPanelBackgroundWorkExtension must be listed in a "
                        + "META-INF/services/org.junit.jupiter.api.extension.Extension on the test classpath, "
                        + "or no test in this module tears its assistant panels down. Registered: "
                        + registeredJUnitExtensions());
    }

    @Test
    void junitExtensionAutoDetectionIsEnabled() {
        assertTrue(Boolean.getBoolean("junit.jupiter.extensions.autodetection.enabled"),
                "Auto-detection is what activates both the platform's leak checks and this module's "
                        + "teardown extension; without it every service-file registration is inert");
    }

    private static List<String> registeredJUnitExtensions() throws IOException {
        List<String> names = new ArrayList<>();
        var resources = Collections.list(ShaftPanelBackgroundWorkExtensionRegistrationTest.class.getClassLoader()
                .getResources("META-INF/services/org.junit.jupiter.api.extension.Extension"));
        for (URL resource : resources) {
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(resource.openStream(), StandardCharsets.UTF_8))) {
                reader.lines()
                        .map(line -> line.replaceAll("#.*", "").trim())
                        .filter(line -> !line.isEmpty())
                        .forEach(names::add);
            }
        }
        return names;
    }
}
