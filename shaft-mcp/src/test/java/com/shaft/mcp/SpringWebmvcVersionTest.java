package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.springframework.web.servlet.DispatcherServlet;

import java.util.jar.JarFile;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Dependabot alerts 340 and 327: spring-webmvc before 7.0.9.
 */
class SpringWebmvcVersionTest {
    @Test
    void classpathSpringWebmvcIsAtLeastThePatchedRelease() throws Exception {
        var location = DispatcherServlet.class.getProtectionDomain().getCodeSource().getLocation();
        try (JarFile jar = new JarFile(Path.of(location.toURI()).toFile())) {
            String version = jar.getManifest().getMainAttributes().getValue("Implementation-Version");
            assertTrue(version != null && !version.isBlank(), "spring-webmvc manifest version");
            assertTrue(compare(version, "7.0.9") >= 0, "spring-webmvc " + version + " is below 7.0.9");
        }
    }

    private static int compare(String left, String right) {
        String[] a = left.split("[.-]");
        String[] b = right.split("[.-]");
        int length = Math.max(a.length, b.length);
        for (int i = 0; i < length; i++) {
            int av = i < a.length && a[i].chars().allMatch(Character::isDigit) ? Integer.parseInt(a[i]) : 0;
            int bv = i < b.length && b[i].chars().allMatch(Character::isDigit) ? Integer.parseInt(b[i]) : 0;
            if (av != bv) {
                return Integer.compare(av, bv);
            }
        }
        return 0;
    }
}
