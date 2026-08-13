package com.shaft.tools.io.internal;

import com.shaft.gui.capabilities.AutomationBackend;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

class PlaywrightTraceImporterTest {
    @Test
    void importsAndMergesThePinnedOfficialPlaywrightTrace() throws Exception {
        String sample = System.getProperty("shaft.playwrightTraceSample", "");
        if (sample.isBlank()) {
            throw new SkipException("Set shaft.playwrightTraceSample to run the pinned official-sample acceptance.");
        }
        Path samplePath = Path.of(sample);
        Assert.assertEquals(Files.size(samplePath), 167_630L);
        Assert.assertEquals(java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256")
                        .digest(Files.readAllBytes(samplePath))),
                "76a2cbb0451bda1b799c3cc3a8270874d33e246976a5dbd4ec7be8fae8234f24");

        PlaywrightTraceImporter.ImportedTrace imported = PlaywrightTraceImporter.importTrace(samplePath, List.of());

        PlaywrightTraceImporter.NativeAction fill = imported.actions().stream()
                .filter(action -> "call@166".equals(action.callId()))
                .findFirst().orElseThrow();
        Assert.assertEquals(fill.stepId(), "pw:api@30");
        Assert.assertEquals(fill.method(), "fill");
        Assert.assertTrue(fill.title().startsWith("Fill \"buy some cheese\""));
        Assert.assertEquals(fill.beforeSnapshot(), "before@call@166");
        Assert.assertEquals(fill.inputSnapshot(), "input@call@166");
        Assert.assertEquals(fill.afterSnapshot(), "after@call@166");
        Assert.assertTrue(fill.logs().stream().anyMatch(log -> log.contains("attempting fill action")));
        Assert.assertTrue(fill.source().endsWith("integration.spec.ts:406:19"), fill.source());
        Assert.assertTrue(fill.startEpochMillis() > 1_700_000_000_000L);
        Assert.assertTrue(fill.endEpochMillis() >= fill.startEpochMillis());
    }

    @Test
    void correlatesOnlyUniquePlaywrightOperationAndTimeMatches() throws Exception {
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":110,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{},\"stepId\":\"step@1\"}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@1\",\"endTime\":120}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@2\",\"startTime\":140,"
                        + "\"class\":\"Frame\",\"method\":\"fill\",\"params\":{},\"stepId\":\"step@2\"}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@2\",\"endTime\":150}\n");
        try {
            List<TraceEventRecorder.ActionEvent> shaftActions = List.of(
                    action("action-1", AutomationBackend.MICROSOFT_PLAYWRIGHT, "click", "1970-01-01T00:00:10.011Z", 8),
                    action("action-2", AutomationBackend.SELENIUM_WEBDRIVER, "fill", "1970-01-01T00:00:10.041Z", 8),
                    action("action-3", AutomationBackend.MICROSOFT_PLAYWRIGHT, "fill", "1970-01-01T00:00:30Z", 8));

            PlaywrightTraceImporter.ImportedTrace imported = PlaywrightTraceImporter.importTrace(archive, shaftActions);

            Assert.assertEquals(imported.correlations().size(), 1);
            Assert.assertEquals(imported.correlations().getFirst().shaftActionId(), "action-1");
            Assert.assertEquals(imported.correlations().getFirst().playwrightCallId(), "call@1");
            Assert.assertEquals(imported.correlatedActions().getFirst().metadata().get("playwrightCallId"), "call@1");
            Assert.assertFalse(imported.correlatedActions().get(1).metadata().containsKey("playwrightCallId"));
            Assert.assertFalse(imported.correlatedActions().get(2).metadata().containsKey("playwrightCallId"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void leavesEqualDistanceCandidatesUncorrelated() throws Exception {
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@left\",\"startTime\":110,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@left\",\"endTime\":111}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@right\",\"startTime\":112,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@right\",\"endTime\":113}\n");
        try {
            PlaywrightTraceImporter.ImportedTrace imported = PlaywrightTraceImporter.importTrace(archive, List.of(
                    action("action-1", AutomationBackend.MICROSOFT_PLAYWRIGHT, "click",
                            "1970-01-01T00:00:10.011Z", 1)));
            Assert.assertTrue(imported.correlations().isEmpty(), "Equal-distance candidates must remain ambiguous.");
            Assert.assertFalse(imported.correlatedActions().getFirst().metadata().containsKey("playwrightCallId"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void rejectsLegacyVersionsThatRequirePlaywrightModernization() throws Exception {
        Path archive = PlaywrightTraceTestFixtures.writeTrace(
                "{\"version\":7,\"type\":\"context-options\",\"origin\":\"library\","
                        + "\"wallTime\":10000,\"monotonicTime\":100}\n");
        try {
            IOException failure = Assert.expectThrows(IOException.class,
                    () -> PlaywrightTraceImporter.importTrace(archive, List.of()));
            Assert.assertEquals(failure.getMessage(),
                    "Unsupported Playwright trace version 7 in 0-trace.trace; only version 8 is importable.");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void boundsImportedActionsAcrossAllTraceStreams() throws Exception {
        Path archive = Files.createTempFile("playwright-import-total-actions", ".zip");
        StringBuilder first = contextWithActions("one", 6_000);
        StringBuilder second = contextWithActions("two", 4_001);
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
            for (Map.Entry<String, StringBuilder> entry : Map.of(
                    "one.trace", first, "two.trace", second).entrySet()) {
                output.putNextEntry(new ZipEntry(entry.getKey()));
                output.write(entry.getValue().toString().getBytes(java.nio.charset.StandardCharsets.UTF_8));
                output.closeEntry();
            }
        }
        try {
            IOException failure = Assert.expectThrows(IOException.class,
                    () -> PlaywrightTraceImporter.importTrace(archive, List.of()));
            Assert.assertEquals(failure.getMessage(), "Playwright trace exceeds the 10000 imported action limit.");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void rejectsInvalidContextOrderingAndUnsafeActionTimes() throws Exception {
        for (Map.Entry<String, String> fixture : Map.of(
                "action-before-context",
                "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":1,\"method\":\"click\"}\n"
                        + contextOptions(0, 0),
                "duplicate-context", contextOptions(0, 0) + contextOptions(1, 1),
                "non-finite-time", contextOptions(0, 0)
                        + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":\"NaN\","
                        + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n",
                "overflow-time", contextOptions(1.0E308, -1.0E308)
                        + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":1.0E308,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n").entrySet()) {
            Path archive = PlaywrightTraceTestFixtures.writeTrace(fixture.getValue());
            try {
                IOException failure = Assert.expectThrows(IOException.class,
                        () -> PlaywrightTraceImporter.importTrace(archive, List.of()));
                Assert.assertTrue(failure.getMessage().contains("Playwright trace"),
                        fixture.getKey() + ": " + failure.getMessage());
            } finally {
                Files.deleteIfExists(archive);
            }
        }
    }

    @Test
    void ignoresOrphanLogsLikeThePinnedPlaywrightModernizer() throws Exception {
        Path archive = PlaywrightTraceTestFixtures.writeTrace(contextOptions(10_000, 100)
                + "{\"type\":\"log\",\"callId\":\"missing\",\"time\":101,\"message\":\"orphan\"}\n"
                + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":110,"
                + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n"
                + "{\"type\":\"after\",\"callId\":\"call@1\",\"endTime\":111}\n");
        try {
            PlaywrightTraceImporter.ImportedTrace imported = PlaywrightTraceImporter.importTrace(archive, List.of());
            Assert.assertEquals(imported.actions().size(), 1);
            Assert.assertTrue(imported.actions().getFirst().logs().isEmpty());
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void extremeShaftInstantCannotOverflowIntoAFalseCorrelation() throws Exception {
        Path archive = PlaywrightTraceTestFixtures.writeTrace(contextOptions(0, 0)
                + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":1,"
                + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n");
        try {
            PlaywrightTraceImporter.ImportedTrace imported = PlaywrightTraceImporter.importTrace(archive, List.of(
                    action("action-1", AutomationBackend.MICROSOFT_PLAYWRIGHT, "click",
                            "+1000000000-12-31T23:59:59.999999999Z", 1)));
            Assert.assertTrue(imported.correlations().isEmpty());
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void loadsSourceFromTheStandardStacksSidecar() throws Exception {
        Path archive = Files.createTempFile("playwright-import-stacks", ".zip");
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
            output.putNextEntry(new ZipEntry("0-trace.trace"));
            output.write((contextOptions(10_000, 100)
                    + "{\"type\":\"before\",\"callId\":\"call@164\",\"startTime\":110,"
                    + "\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n")
                    .getBytes(java.nio.charset.StandardCharsets.UTF_8));
            output.closeEntry();
            output.putNextEntry(new ZipEntry("0-trace.stacks"));
            output.write(("{\"files\":[\"customer/LoginTest.java\"],"
                    + "\"stacks\":[[164,[[0,42,7,\"submit\"]]]]}\n")
                    .getBytes(java.nio.charset.StandardCharsets.UTF_8));
            output.closeEntry();
        }
        try {
            PlaywrightTraceImporter.NativeAction action = PlaywrightTraceImporter.importTrace(archive, List.of())
                    .actions().getFirst();
            Assert.assertEquals(action.source(), "customer/LoginTest.java:42:7");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    private static String contextOptions(double wallTime, double monotonicTime) {
        return "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\",\"wallTime\":"
                + wallTime + ",\"monotonicTime\":" + monotonicTime + "}\n";
    }

    private static StringBuilder contextWithActions(String prefix, int count) {
        StringBuilder trace = new StringBuilder("{\"version\":8,\"type\":\"context-options\","
                + "\"origin\":\"library\",\"wallTime\":10000,\"monotonicTime\":100}\n");
        for (int index = 0; index < count; index++) {
            trace.append("{\"type\":\"before\",\"callId\":\"").append(prefix).append('@').append(index)
                    .append("\",\"startTime\":").append(110 + index)
                    .append(",\"class\":\"Frame\",\"method\":\"click\",\"params\":{}}\n");
        }
        return trace;
    }

    private static TraceEventRecorder.ActionEvent action(String id, AutomationBackend backend, String name,
                                                         String startTime, long durationMs) {
        return new TraceEventRecorder.ActionEvent(id, backend, "element", name, "passed", startTime, durationMs,
                "locator", "", "", "", "", "", List.of(), Map.of(), Map.of(), "", "", "");
    }
}
