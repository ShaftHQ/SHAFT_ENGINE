package com.shaft.intellij.wizard;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Writes a minimal Maven TestNG + SHAFT project tree (W2.1 / J2 AC).
 */
public final class ShaftMavenProjectScaffold {
    private ShaftMavenProjectScaffold() {
        throw new IllegalStateException("Utility class");
    }

    public static void write(@NotNull Path root, @NotNull ShaftMavenProjectSettings settings) throws IOException {
        Files.createDirectories(root.resolve("src/test/java"));
        Files.createDirectories(root.resolve("src/main/resources"));
        Files.createDirectories(root.resolve("src/test/resources"));
        Files.writeString(root.resolve("pom.xml"), pomXml(settings), StandardCharsets.UTF_8);
        Files.writeString(root.resolve("src/main/resources/custom.properties"),
                "# SHAFT project defaults\n"
                        + "targetAddress=\n"
                        + "headlessExecution=true\n",
                StandardCharsets.UTF_8);
        Files.writeString(root.resolve("src/main/resources/secrets.properties.example"),
                "# Copy to secrets.properties (gitignored) and fill values.\n"
                        + "# cloud.username=\n"
                        + "# cloud.accessKey=\n",
                StandardCharsets.UTF_8);
        Files.writeString(root.resolve(".gitignore"),
                "target/\n"
                        + ".idea/\n"
                        + "*.iml\n"
                        + "secrets.properties\n"
                        + "allure-results/\n"
                        + "allure-report/\n",
                StandardCharsets.UTF_8);
        String packagePath = settings.getGroupId().replace('.', '/');
        Path testDir = root.resolve("src/test/java").resolve(packagePath);
        Files.createDirectories(testDir);
        Files.writeString(testDir.resolve("SmokeTest.java"), smokeTest(settings), StandardCharsets.UTF_8);
    }

    private static @NotNull String pomXml(@NotNull ShaftMavenProjectSettings settings) {
        return """
                <?xml version="1.0" encoding="UTF-8"?>
                <project xmlns="http://maven.apache.org/POM/4.0.0"
                         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
                  <modelVersion>4.0.0</modelVersion>
                  <groupId>%s</groupId>
                  <artifactId>%s</artifactId>
                  <version>%s</version>
                  <properties>
                    <maven.compiler.release>17</maven.compiler.release>
                    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
                    <suiteXmlFile/>
                  </properties>
                  <dependencyManagement>
                    <dependencies>
                      <dependency>
                        <groupId>io.github.shafthq</groupId>
                        <artifactId>shaft-bom</artifactId>
                        <version>RELEASE</version>
                        <type>pom</type>
                        <scope>import</scope>
                      </dependency>
                    </dependencies>
                  </dependencyManagement>
                  <dependencies>
                    <dependency>
                      <groupId>io.github.shafthq</groupId>
                      <artifactId>shaft-engine</artifactId>
                      <scope>test</scope>
                    </dependency>
                    <dependency>
                      <groupId>org.testng</groupId>
                      <artifactId>testng</artifactId>
                      <scope>test</scope>
                    </dependency>
                  </dependencies>
                </project>
                """.formatted(settings.getGroupId(), settings.getArtifactId(), settings.getVersion());
    }

    private static @NotNull String smokeTest(@NotNull ShaftMavenProjectSettings settings) {
        return """
                package %s;

                import org.testng.annotations.Test;

                public class SmokeTest {
                    @Test
                    public void projectScaffolds() {
                        // Replace with a SHAFT fluent test once MCP / recorder is ready.
                    }
                }
                """.formatted(settings.getGroupId());
    }
}
