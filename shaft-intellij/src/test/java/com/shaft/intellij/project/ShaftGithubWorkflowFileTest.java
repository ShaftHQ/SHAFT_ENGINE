package com.shaft.intellij.project;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftGithubWorkflowFileTest {
    @TempDir
    Path root;

    @Test
    void mavenProjectGetsHeadlessWorkflowThatUploadsAllureResults() throws IOException {
        Files.writeString(root.resolve("pom.xml"), "<project/>");

        ShaftGithubWorkflowFile.Result result = ShaftGithubWorkflowFile.generate(root);

        String yaml = Files.readString(result.file(), StandardCharsets.UTF_8);
        assertAll(
                () -> assertEquals(ShaftGithubWorkflowFile.Outcome.CREATED, result.outcome()),
                () -> assertEquals(root.resolve(".github/workflows/shaft-tests.yml"), result.file()),
                () -> assertTrue(yaml.contains("JAVA_TOOL_OPTIONS: -DheadlessExecution=true"), yaml),
                () -> assertTrue(yaml.contains("run: mvn -B test"), yaml),
                () -> assertTrue(yaml.contains("cache: 'maven'"), yaml),
                () -> assertTrue(yaml.contains("if: always()"), yaml),
                () -> assertTrue(yaml.contains("uses: actions/upload-artifact@"), yaml),
                () -> assertTrue(yaml.contains("path: allure-results"), yaml));
    }

    @Test
    void mavenWrapperIsPreferredWhenPresent() throws IOException {
        Files.writeString(root.resolve("pom.xml"), "<project/>");
        Files.writeString(root.resolve("mvnw"), "#!/bin/sh");

        ShaftGithubWorkflowFile.Result result = ShaftGithubWorkflowFile.generate(root);

        assertTrue(Files.readString(result.file()).contains("run: chmod +x mvnw && ./mvnw -B test"));
    }

    @Test
    void gradleWrapperProjectRunsGradleTests() throws IOException {
        Files.writeString(root.resolve("build.gradle.kts"), "");
        Files.writeString(root.resolve("gradlew"), "#!/bin/sh");

        ShaftGithubWorkflowFile.Result result = ShaftGithubWorkflowFile.generate(root);

        String yaml = Files.readString(result.file());
        assertAll(
                () -> assertEquals(ShaftGithubWorkflowFile.Outcome.CREATED, result.outcome()),
                () -> assertTrue(yaml.contains("run: chmod +x gradlew && ./gradlew test --no-daemon"), yaml),
                () -> assertTrue(yaml.contains("cache: 'gradle'"), yaml));
    }

    @Test
    void existingWorkflowIsNeverOverwritten() throws IOException {
        Files.writeString(root.resolve("pom.xml"), "<project/>");
        Path existing = root.resolve(ShaftGithubWorkflowFile.RELATIVE_PATH);
        Files.createDirectories(existing.getParent());
        Files.writeString(existing, "name: mine\n");

        ShaftGithubWorkflowFile.Result result = ShaftGithubWorkflowFile.generate(root);

        assertAll(
                () -> assertEquals(ShaftGithubWorkflowFile.Outcome.ALREADY_EXISTS, result.outcome()),
                () -> assertEquals("name: mine\n", Files.readString(existing)));
    }

    @Test
    void projectWithoutMavenOrGradleWrapperWritesNothing() throws IOException {
        Files.writeString(root.resolve("build.gradle"), "");

        ShaftGithubWorkflowFile.Result result = ShaftGithubWorkflowFile.generate(root);

        assertAll(
                () -> assertEquals(ShaftGithubWorkflowFile.Outcome.UNSUPPORTED_BUILD, result.outcome()),
                () -> assertFalse(Files.exists(root.resolve(".github"))));
    }
}
