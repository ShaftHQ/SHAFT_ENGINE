package com.shaft.intellij.notifications;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FailedRunDoctorNotifierTest {
    @Test
    void doctorArgumentsBuildsCorrectJsonStructure() {
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments();

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
    void doctorArgumentsIncludesAllureResultPaths() {
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments();

        assertTrue(arguments.has("allureResultPaths"));
        assertTrue(arguments.get("allureResultPaths").isJsonArray());
        assertEquals(1, arguments.getAsJsonArray("allureResultPaths").size());
        assertEquals("allure-results", arguments.getAsJsonArray("allureResultPaths").get(0).getAsString());
    }

    @Test
    void doctorArgumentsIncludesEmptyHistoricalBundlesAndSourcePaths() {
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments();

        assertTrue(arguments.has("historicalBundlePaths"));
        assertTrue(arguments.get("historicalBundlePaths").isJsonArray());
        assertEquals(0, arguments.getAsJsonArray("historicalBundlePaths").size());

        assertTrue(arguments.has("allowedSourcePaths"));
        assertTrue(arguments.get("allowedSourcePaths").isJsonArray());
        assertEquals(0, arguments.getAsJsonArray("allowedSourcePaths").size());
    }

    @Test
    void healerArgumentsBuildsCorrectJsonStructure() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments();

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
    void healerArgumentsIncludesTestCommand() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments();

        assertTrue(arguments.has("testCommand"));
        assertTrue(arguments.get("testCommand").isJsonArray());
        assertEquals(4, arguments.getAsJsonArray("testCommand").size());
        assertEquals("mvn", arguments.getAsJsonArray("testCommand").get(0).getAsString());
        assertEquals("-q", arguments.getAsJsonArray("testCommand").get(1).getAsString());
        assertEquals("-Dtest=ExampleTest", arguments.getAsJsonArray("testCommand").get(2).getAsString());
        assertEquals("test", arguments.getAsJsonArray("testCommand").get(3).getAsString());
    }

    @Test
    void healerArgumentsIncludesEmptyAllowedSourcePaths() {
        JsonObject arguments = FailedRunDoctorNotifier.healerArguments();

        assertTrue(arguments.has("allowedSourcePaths"));
        assertTrue(arguments.get("allowedSourcePaths").isJsonArray());
        assertEquals(0, arguments.getAsJsonArray("allowedSourcePaths").size());
    }
}
