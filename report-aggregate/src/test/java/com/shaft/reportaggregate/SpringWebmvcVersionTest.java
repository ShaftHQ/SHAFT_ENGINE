package com.shaft.reportaggregate;

import org.junit.jupiter.api.Test;
import org.springframework.web.servlet.DispatcherServlet;

import java.nio.file.Path;
import java.util.jar.JarFile;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Dependabot alert 327: the report-aggregate graph must resolve spring-webmvc 7.0.9 or newer.
 */
class SpringWebmvcVersionTest {
    @Test
    void classpathSpringWebmvcIsAtLeastThePatchedRelease() throws Exception {
        var location = DispatcherServlet.class.getProtectionDomain().getCodeSource().getLocation();
        try (JarFile jar = new JarFile(Path.of(location.toURI()).toFile())) {
            String version = jar.getManifest().getMainAttributes().getValue("Implementation-Version");
            assertTrue(version != null && !version.isBlank(), "spring-webmvc manifest version");
            String[] parts = version.split("[.-]");
            int major = Integer.parseInt(parts[0]);
            int minor = Integer.parseInt(parts[1]);
            int patch = Integer.parseInt(parts[2]);
            assertTrue(major > 7 || (major == 7 && minor > 0) || (major == 7 && minor == 0 && patch >= 9),
                    "spring-webmvc " + version + " is below 7.0.9");
        }
    }
}
