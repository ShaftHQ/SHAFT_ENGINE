package com.shaft.intellij.notifications;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@code processTerminated} itself drives {@code ExecutionEnvironment}/{@code ToolWindowManager},
 * which need a live IntelliJ platform and are not exercised by this lightweight Gradle unit test
 * JVM (same documented gap as {@link ShaftToolWorkflowLauncherTest}). This class instead covers
 * every pure piece {@code processTerminated} delegates to: argument building, evidence-path
 * resolution, real-test-identity normalization (issue #3547), and the user-cancellation skip
 * decision (issue #3591).
 */
class FailedRunDoctorNotifierTest {
    @Test
    void isUserCancelledRunIsFalseWhenNeverRecorded() {
        // The common case: a run that fails on its own (assertion failure, non-zero JVM exit) never
        // goes through destroyProcess(), so processWillTerminate is never called and no value is
        // ever stored. This must NOT be treated as a cancellation, or genuinely failed runs would
        // silently stop notifying (issue #3591).
        assertFalse(FailedRunDoctorNotifier.isUserCancelledRun(null));
    }

    @Test
    void isUserCancelledRunIsFalseWhenRecordedAsNotDestroyed() {
        // processWillTerminate(event, false) corresponds to detachProcess(), not a user Stop.
        assertFalse(FailedRunDoctorNotifier.isUserCancelledRun(Boolean.FALSE));
    }

    @Test
    void isUserCancelledRunIsTrueWhenRecordedAsForciblyDestroyed() {
        // processWillTerminate(event, true) corresponds to destroyProcess() -- the user pressed
        // Stop, or the IDE force-killed the run.
        assertTrue(FailedRunDoctorNotifier.isUserCancelledRun(Boolean.TRUE));
    }

    @Test
    void doctorArgumentsBuildsCorrectJsonStructure() {
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments("allure-results");

        assertEquals(".", arguments.get("repositoryRoot").getAsString());
        assertEquals("target/shaft-doctor", arguments.get("outputDirectory").getAsString());
        assertTrue(arguments.get("includeScreenshots").getAsBoolean());
        assertTrue(arguments.get("includePageSnapshots").getAsBoolean());
        assertEquals(1, arguments.get("minimumAllureResults").getAsInt());
        assertFalse(arguments.get("useAi").getAsBoolean());
        assertFalse(arguments.get("allowLocalAi").getAsBoolean());
        assertFalse(arguments.get("allowRemoteAi").getAsBoolean());
        assertEquals("driver", arguments.get("driverVariableName").getAsString());
    }

    @Test
    void doctorArgumentsIncludesTheResolvedAllureResultsPath() {
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments("target/allure-results");

        assertTrue(arguments.has("allureResultPaths"));
        assertTrue(arguments.get("allureResultPaths").isJsonArray());
        assertEquals(1, arguments.getAsJsonArray("allureResultPaths").size());
        assertEquals("target/allure-results", arguments.getAsJsonArray("allureResultPaths").get(0).getAsString());
    }

    @Test
    void doctorArgumentsLeavesAllureResultPathsEmptyWhenPathIsUnknown() {
        // A null/blank path must produce an empty array, not a guessed literal: DoctorService
        // treats an empty allureResultPaths as "auto-discover the newest evidence in the
        // workspace," which is the whole point of not hardcoding a directory that might not exist
        // (the bug this replaces -- see resolveAllureResultsPath's javadoc).
        assertTrue(FailedRunDoctorNotifier.doctorArguments(null)
                .getAsJsonArray("allureResultPaths").isEmpty());
        assertTrue(FailedRunDoctorNotifier.doctorArguments("  ")
                .getAsJsonArray("allureResultPaths").isEmpty());
    }

    @Test
    void doctorArgumentsIncludesEmptyHistoricalBundlesAndSourcePaths() {
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments("allure-results");

        assertTrue(arguments.has("historicalBundlePaths"));
        assertTrue(arguments.get("historicalBundlePaths").isJsonArray());
        assertEquals(0, arguments.getAsJsonArray("historicalBundlePaths").size());

        assertTrue(arguments.has("allowedSourcePaths"));
        assertTrue(arguments.get("allowedSourcePaths").isJsonArray());
        assertEquals(0, arguments.getAsJsonArray("allowedSourcePaths").size());
    }

    @Test
    void healerArgumentsBuildsCorrectJsonStructure() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments("CheckoutTest");

        assertEquals(".", arguments.get("repositoryRoot").getAsString());
        assertEquals("target/shaft-healer", arguments.get("outputDirectory").getAsString());
        assertEquals(1, arguments.get("maxAttempts").getAsInt());
        assertTrue(arguments.get("includeScreenshots").getAsBoolean());
        assertTrue(arguments.get("includePageSnapshots").getAsBoolean());
        assertFalse(arguments.get("networkValidationApproved").getAsBoolean());
        assertFalse(arguments.get("useConfiguredAi").getAsBoolean());
        assertFalse(arguments.get("allowLocalAi").getAsBoolean());
        assertFalse(arguments.get("allowRemoteAi").getAsBoolean());
        assertEquals("driver", arguments.get("driverVariableName").getAsString());
    }

    @Test
    void healerArgumentsThreadsTheRealFailingTestIntoTheCommand() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments("CheckoutTest");

        assertTrue(arguments.has("testCommand"));
        assertTrue(arguments.get("testCommand").isJsonArray());
        assertEquals(4, arguments.getAsJsonArray("testCommand").size());
        assertEquals("mvn", arguments.getAsJsonArray("testCommand").get(0).getAsString());
        assertEquals("-q", arguments.getAsJsonArray("testCommand").get(1).getAsString());
        assertEquals("-Dtest=CheckoutTest", arguments.getAsJsonArray("testCommand").get(2).getAsString());
        assertEquals("test", arguments.getAsJsonArray("testCommand").get(3).getAsString());
    }

    @Test
    void healerArgumentsNeverReintroducesTheHardcodedExampleTestPlaceholder() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments("LoginFlowTest");

        String testCommandEntry = arguments.getAsJsonArray("testCommand").get(2).getAsString();
        assertFalse(testCommandEntry.contains("ExampleTest"));
        assertEquals("-Dtest=LoginFlowTest", testCommandEntry);
    }

    @Test
    void healerArgumentsFallsBackToAWildcardWhenIdentityIsUnknown() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments(null);

        // Never crashes and never fabricates a specific nonexistent class name; the wildcard at
        // least matches the common *Test naming convention instead of a class that does not exist.
        assertEquals("-Dtest=*Test", arguments.getAsJsonArray("testCommand").get(2).getAsString());
    }

    @Test
    void healerArgumentsIncludesEmptyAllowedSourcePaths() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments("CheckoutTest");

        assertTrue(arguments.has("allowedSourcePaths"));
        assertTrue(arguments.get("allowedSourcePaths").isJsonArray());
        assertEquals(0, arguments.getAsJsonArray("allowedSourcePaths").size());
    }

    @Test
    void normalizeTestIdentityLeavesASimpleClassNameUnchanged() {
        assertEquals("CheckoutTest", FailedRunDoctorNotifier.normalizeTestIdentity("CheckoutTest"));
    }

    @Test
    void normalizeTestIdentityConvertsClassDotMethodToMavenSyntax() {
        assertEquals("CheckoutTest#testLogin",
                FailedRunDoctorNotifier.normalizeTestIdentity("CheckoutTest.testLogin"));
    }

    @Test
    void normalizeTestIdentityStripsAnIdeRunCounterSuffix() {
        assertEquals("CheckoutTest", FailedRunDoctorNotifier.normalizeTestIdentity("CheckoutTest (1)"));
    }

    @Test
    void normalizeTestIdentityFallsBackToTheRawProfileNameWhenUnresolvable() {
        // Not a recognizable class/method shape (a custom suite name with spaces): the profile
        // name itself is still the best available identity, per the issue's "fall back to the
        // profile name (don't crash)" instruction.
        assertEquals("All Smoke Tests", FailedRunDoctorNotifier.normalizeTestIdentity("All Smoke Tests"));
    }

    @Test
    void normalizeTestIdentityHandlesNullAndBlank() {
        assertEquals("", FailedRunDoctorNotifier.normalizeTestIdentity(null));
        assertEquals("", FailedRunDoctorNotifier.normalizeTestIdentity("   "));
    }

    @Test
    void resolveAllureResultsPathPrefersTheProjectRootDirectoryWhenPresent() throws IOException {
        Path projectRoot = Files.createTempDirectory("shaft-doctor-notifier-test");
        try {
            Path allureResults = projectRoot.resolve("allure-results");
            Files.createDirectories(allureResults);
            Files.writeString(allureResults.resolve("abc-result.json"), "{}");

            assertEquals("allure-results", FailedRunDoctorNotifier.resolveAllureResultsPath(projectRoot));
        } finally {
            deleteRecursively(projectRoot);
        }
    }

    @Test
    void resolveAllureResultsPathFallsBackToTheMavenDefaultLayout() throws IOException {
        Path projectRoot = Files.createTempDirectory("shaft-doctor-notifier-test");
        try {
            Path allureResults = projectRoot.resolve("target").resolve("allure-results");
            Files.createDirectories(allureResults);
            Files.writeString(allureResults.resolve("abc-result.json"), "{}");

            assertEquals("target/allure-results",
                    FailedRunDoctorNotifier.resolveAllureResultsPath(projectRoot).replace('\\', '/'));
        } finally {
            deleteRecursively(projectRoot);
        }
    }

    @Test
    void resolveAllureResultsPathReturnsNullWhenNoEvidenceExists() throws IOException {
        Path projectRoot = Files.createTempDirectory("shaft-doctor-notifier-test");
        try {
            assertNull(FailedRunDoctorNotifier.resolveAllureResultsPath(projectRoot));
        } finally {
            deleteRecursively(projectRoot);
        }
    }

    @Test
    void resolveNewestTraceDirectoryReturnsNullWhenNoTraceExists() throws IOException {
        Path projectRoot = Files.createTempDirectory("shaft-doctor-notifier-test");
        try {
            assertNull(FailedRunDoctorNotifier.resolveNewestTraceDirectory(projectRoot));
        } finally {
            deleteRecursively(projectRoot);
        }
    }

    @Test
    void resolveNewestTraceDirectoryFindsATraceUnderTargetShaftTraces() throws IOException {
        Path projectRoot = Files.createTempDirectory("shaft-doctor-notifier-test");
        try {
            Path traceDirectory = projectRoot.resolve("target").resolve("shaft-traces").resolve("run-1");
            Files.createDirectories(traceDirectory);
            Files.writeString(traceDirectory.resolve("index.json"), "{}");

            String resolved = FailedRunDoctorNotifier.resolveNewestTraceDirectory(projectRoot);

            assertEquals("target/shaft-traces/run-1", resolved.replace('\\', '/'));
        } finally {
            deleteRecursively(projectRoot);
        }
    }

    private static void deleteRecursively(Path root) throws IOException {
        if (!Files.exists(root)) {
            return;
        }
        try (var paths = Files.walk(root)) {
            paths.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException ignored) {
                    // Best-effort cleanup; the OS temp folder is the backstop.
                }
            });
        }
    }
}
