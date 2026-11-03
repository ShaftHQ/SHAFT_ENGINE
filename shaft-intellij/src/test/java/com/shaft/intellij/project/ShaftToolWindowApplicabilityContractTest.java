package com.shaft.intellij.project;

import com.shaft.intellij.ShaftToolWindowFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * W1.7 contract: tool window factory applicability mirrors {@link ShaftProjectDetector}.
 */
class ShaftToolWindowApplicabilityContractTest {
    @AfterEach
    void clearCache() {
        ShaftProjectDetector.clearCacheForTests();
    }

    @Test
    void factoryExistsForApplicableRegistration() {
        assertNotNull(new ShaftToolWindowFactory());
    }

    @Test
    void detectorAcceptsShaftPom(@TempDir Path root) throws Exception {
        Files.writeString(root.resolve("pom.xml"),
                "<project><dependencies><dependency><groupId>io.github.shafthq</groupId>"
                        + "<artifactId>shaft-engine</artifactId></dependency></dependencies></project>");
        assertTrue(ShaftProjectDetector.isShaftProject(root));
    }

    @Test
    void detectorRejectsPlainPom(@TempDir Path root) throws Exception {
        Files.writeString(root.resolve("pom.xml"),
                "<project><dependencies><dependency><groupId>junit</groupId>"
                        + "<artifactId>junit</artifactId></dependency></dependencies></project>");
        assertFalse(ShaftProjectDetector.isShaftProject(root));
    }
}
