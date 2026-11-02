package com.shaft.intellij.build;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

class IntellijBuildJdkContractTest {
    @Test
    void buildDaemonAndCiUseJdk25WhilePluginBytecodeRemainsJava17() throws IOException {
        String settings = Files.readString(Path.of("settings.gradle.kts"));
        String build = Files.readString(Path.of("build.gradle.kts"));
        String wrapper = Files.readString(Path.of("gradle/wrapper/gradle-wrapper.properties"));
        String verificationAction = Files.readString(Path.of("../.github/actions/intellij-verify/action.yml"));
        String guidedLive = Files.readString(Path.of("../.github/workflows/guided-workflows-live.yml"));
        String liveTools = Files.readString(Path.of("../.github/workflows/live-tools-nightly.yml"));
        String recordingRunbook = Files.readString(Path.of("../tools/intellij-plugin-recording/RUNBOOK.md"));

        assertTrue(settings.contains("maxSupportedDaemonJavaVersion = JavaVersion.VERSION_25"));
        assertTrue(verificationAction.contains("java-version: '25'"));
        assertTrue(verificationAction.contains(
                "bash shaft-intellij/gradlew -p shaft-intellij check buildPlugin verifyPlugin"));
        assertTrue(verificationAction.contains(
                "bash shaft-intellij/gradlew -p shaft-intellij signPlugin publishPlugin"));
        assertTrue(wrapper.contains("gradle-9.3.0-bin.zip"));
        assertTrue(wrapper.contains("distributionSha256Sum=0d585f69da091fc5b2beced877feab55a3064d43b8a1d46aeb07996b0915e0e0"));
        assertTrue(guidedLive.contains("java-version: '25'"));
        assertTrue(guidedLive.contains("bash shaft-intellij/gradlew -p shaft-intellij test"));
        assertEquals(4, occurrences(liveTools, "java-version: '25'"));
        assertEquals(5, occurrences(liveTools, "bash shaft-intellij/gradlew -p shaft-intellij test"));
        assertTrue(recordingRunbook.contains("JDK 25 for the `shaft-intellij` Gradle daemon"));
        assertTrue(build.contains("sourceCompatibility = JavaVersion.VERSION_17"));
        assertTrue(build.contains("targetCompatibility = JavaVersion.VERSION_17"));
        assertTrue(build.contains("options.release.set(17)"));
    }

    private static int occurrences(String text, String expected) {
        return text.split(java.util.regex.Pattern.quote(expected), -1).length - 1;
    }
}
