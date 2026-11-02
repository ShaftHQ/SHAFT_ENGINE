package com.shaft.intellij.build;

import org.junit.jupiter.api.Test;
import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.GradleRunner;

import java.nio.file.Path;

import static org.gradle.testkit.runner.TaskOutcome.SUCCESS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class IntellijRunIdeCrashLogContractTest {
    @Test
    void everyRunIdeTaskWritesFatalErrorLogsToItsMutableSandboxLogDirectory() {
        BuildResult result = GradleRunner.create()
                .withProjectDir(Path.of(".").toFile())
                .withTestKitDir(Path.of(System.getProperty("shaft.intellij.gradleUserHome")).toFile())
                .withGradleVersion("9.3.0")
                .withArguments("verifyRunIdeCrashLogConfiguration", "--stacktrace")
                .forwardOutput()
                .build();

        assertEquals(SUCCESS, result.task(":verifyRunIdeCrashLogConfiguration").getOutcome());
        assertTrue(result.getOutput().contains("BUILD SUCCESSFUL"));
    }
}
