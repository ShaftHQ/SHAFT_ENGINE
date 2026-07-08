package com.shaft.intellij.project;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins {@link ShaftProjectDetector}: the shared check that keeps SHAFT-only actions from firing in
 * an ordinary Java project that never depends on SHAFT.
 */
class ShaftProjectDetectorTest {
    @BeforeEach
    void clearCache() {
        ShaftProjectDetector.clearCacheForTests();
    }

    @Test
    void detectsAMavenShaftDependencyInTheRootPom(@TempDir Path projectRoot) throws IOException {
        writeFile(projectRoot.resolve("pom.xml"), """
                <project>
                    <dependencies>
                        <dependency>
                            <groupId>io.github.shafthq</groupId>
                            <artifactId>shaft-engine</artifactId>
                        </dependency>
                    </dependencies>
                </project>
                """);

        assertTrue(ShaftProjectDetector.isShaftProject(projectRoot));
    }

    @Test
    void plainMavenProjectWithNoShaftDependencyIsNotAShaftProject(@TempDir Path projectRoot) throws IOException {
        writeFile(projectRoot.resolve("pom.xml"), """
                <project>
                    <dependencies>
                        <dependency>
                            <groupId>org.springframework.boot</groupId>
                            <artifactId>spring-boot-starter-web</artifactId>
                        </dependency>
                    </dependencies>
                </project>
                """);

        assertFalse(ShaftProjectDetector.isShaftProject(projectRoot));
    }

    @Test
    void detectsAGradleShaftDependency(@TempDir Path projectRoot) throws IOException {
        writeFile(projectRoot.resolve("build.gradle"), """
                dependencies {
                    testImplementation 'io.github.shafthq:shaft-engine:6.0.0'
                }
                """);

        assertTrue(ShaftProjectDetector.isShaftProject(projectRoot));
    }

    @Test
    void detectsAShaftDependencyInADirectChildModule(@TempDir Path projectRoot) throws IOException {
        Path module = Files.createDirectory(projectRoot.resolve("web-tests"));
        writeFile(projectRoot.resolve("pom.xml"), "<project><modules><module>web-tests</module></modules></project>");
        writeFile(module.resolve("pom.xml"), "<project><artifactId>shaft-bom</artifactId></project>");

        assertTrue(ShaftProjectDetector.isShaftProject(projectRoot));
    }

    @Test
    void emptyDirectoryIsNotAShaftProject(@TempDir Path projectRoot) {
        assertFalse(ShaftProjectDetector.isShaftProject(projectRoot));
    }

    @Test
    void nullProjectIsNotAShaftProject() {
        assertFalse(ShaftProjectDetector.isShaftProject((com.intellij.openapi.project.Project) null));
    }

    private static void writeFile(Path file, String content) throws IOException {
        Files.writeString(file, content);
    }
}
