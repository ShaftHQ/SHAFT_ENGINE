package com.shaft.intellij.wizard;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMavenProjectScaffoldTest {
    @Test
    void writesPomPropertiesSecretsExampleAndSmokeTest(@TempDir Path root) throws Exception {
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        settings.setGroupId("com.demo");
        settings.setArtifactId("demo-tests");
        ShaftMavenProjectScaffold.write(root, settings);

        assertTrue(Files.isRegularFile(root.resolve("pom.xml")));
        assertTrue(Files.isRegularFile(root.resolve("src/main/resources/custom.properties")));
        assertTrue(Files.isRegularFile(root.resolve("src/main/resources/secrets.properties.example")));
        assertTrue(Files.isRegularFile(root.resolve("src/test/java/com/demo/SmokeTest.java")));
        String pom = Files.readString(root.resolve("pom.xml"));
        assertTrue(pom.contains("shaft-engine"));
        assertTrue(pom.contains("com.demo"));
        assertTrue(Files.readString(root.resolve(".gitignore")).contains("secrets.properties"));
    }
}
