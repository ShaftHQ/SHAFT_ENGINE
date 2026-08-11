package com.shaft.capture.generate;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.testng.Assert;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GeneratedTestValidatorReplayIsolationTest {

    @TempDir
    Path temp;

    @Test
    void replayPreservesAndHonorsTheProjectsHeadedConfiguration() throws Exception {
        Path customProperties = temp.resolve("src/main/resources/properties/custom.properties");
        Files.createDirectories(customProperties.getParent());
        String original = "headlessExecution=false" + System.lineSeparator()
                + "customReplaySetting=keep-me" + System.lineSeparator();
        Files.writeString(customProperties, original, StandardCharsets.UTF_8);

        replayMissingClass();

        assertEquals(original, Files.readString(customProperties, StandardCharsets.UTF_8),
                "Replay validation must not rewrite the project's source configuration");
        String args = Files.readString(
                onlyAttemptDirectory().resolve("replay.args"), StandardCharsets.UTF_8);
        assertTrue(args.contains("-DheadlessExecution=false"),
                "The replay subprocess must honor the project's configured headed mode");
        assertFalse(args.contains("-DheadlessExecution=true"));
    }

    @Test
    void replayDoesNotCountAnAllureResultFromAnEarlierAttempt() throws Exception {
        Path allureResults = temp.resolve("target/shaft-capture/replay/allure-results");
        Files.createDirectories(allureResults);
        Files.writeString(allureResults.resolve("stale-result.json"),
                "{\"name\":\"stale failure\",\"status\":\"failed\"}", StandardCharsets.UTF_8);

        CaptureGenerationReport.Validation result = replayMissingClass();

        assertEquals(0, result.allureResultCount(),
                "Only Allure results created by the current replay attempt may be counted");
        assertTrue(Files.exists(allureResults.resolve("stale-result.json")),
                "Replay isolation must not delete artifacts owned by an earlier attempt");
        assertTrue(result.diagnostics().stream().anyMatch(message -> message.contains("attempt-")),
                "A failed replay must identify the unique attempt directory that owns its diagnostics");
    }

    @Test
    void realShaftBootstrapStaysInsideTheAttemptDirectory() throws Exception {
        Path projectProperties = temp.resolve("src/main/resources/properties");
        Files.createDirectories(projectProperties);
        Path siblingProperties = projectProperties.resolve("replay-probe.properties");
        String siblingContents = "replayProbeSetting=preserved" + System.lineSeparator();
        Files.writeString(siblingProperties, siblingContents, StandardCharsets.UTF_8);

        CaptureGenerationReport.Validation result = replayClass(ShaftBootstrapProbe.class.getName());

        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED, result.status(),
                result.diagnostics().toString());
        assertFalse(Files.exists(temp.resolve("src/main/resources/properties/custom.properties")),
                "A real SHAFT bootstrap must not create configuration inside project sources");
        assertEquals(siblingContents, Files.readString(siblingProperties, StandardCharsets.UTF_8),
                "Replay must leave every sibling project property file byte-for-byte unchanged");
    }

    private CaptureGenerationReport.Validation replayMissingClass() {
        return replayClass("generated.capture.ClassThatDoesNotExist");
    }

    private CaptureGenerationReport.Validation replayClass(String className) {
        Path classes = temp.resolve("classes");
        Path resources = temp.resolve("src/test/resources");
        return new GeneratedTestValidator().replay(
                className,
                classes,
                resources,
                temp,
                Duration.ofSeconds(90));
    }

    private Path onlyAttemptDirectory() throws Exception {
        try (var attempts = Files.list(temp.resolve("target/shaft-capture/replay"))) {
            return attempts.filter(Files::isDirectory)
                    .filter(path -> path.getFileName().toString().startsWith("attempt-"))
                    .findFirst()
                    .orElseThrow();
        }
    }

    public static class ShaftBootstrapProbe {
        @org.testng.annotations.Test
        public void initializesShaftPropertiesWithoutTouchingProjectSources() {
            Assert.assertTrue(SHAFT.Properties.web.headlessExecution());
            Assert.assertEquals(System.getProperty("replayProbeSetting"), "preserved");
        }
    }
}
