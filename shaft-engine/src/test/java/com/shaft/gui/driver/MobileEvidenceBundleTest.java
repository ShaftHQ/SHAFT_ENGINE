package com.shaft.gui.driver;

import com.shaft.tools.io.trace.TraceArtifactReference;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.AbstractMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class MobileEvidenceBundleTest {
    private static final Instant CAPTURED_AT = Instant.parse("2026-08-12T00:00:00Z");

    @Test
    public void bundleShouldDefensivelyCopyAndFreezeEveryCollection() {
        Map<String, String> application = new LinkedHashMap<>(Map.of("appPackage", "app.example"));
        Map<String, String> device = new LinkedHashMap<>(Map.of("platformName", "Android"));
        List<MobileLogMessage> messages = new ArrayList<>(List.of(
                new MobileLogMessage(CAPTURED_AT, "logcat", "message")));
        List<MobileLogError> errors = new ArrayList<>(List.of(
                new MobileLogError(CAPTURED_AT, "logcat", "type", "error")));
        List<MobilePerformanceSample> performance = new ArrayList<>(List.of(
                new MobilePerformanceSample(CAPTURED_AT, "app.example", "memoryinfo",
                        List.of("value"), List.of(List.of(1L)))));
        List<TraceArtifactReference> artifacts = new ArrayList<>(List.of(artifact("screenshot", "screenshot.png")));
        Map<String, String> omissions = new LinkedHashMap<>(Map.of("recording", "no-retained-recording"));

        MobileEvidenceBundle bundle = new MobileEvidenceBundle(CAPTURED_AT, Path.of("target", "..", "evidence.zip"),
                "NATIVE_APP", application, device, messages, errors, performance, artifacts, omissions);

        application.clear();
        device.clear();
        messages.clear();
        errors.clear();
        performance.clear();
        artifacts.clear();
        omissions.clear();

        Assert.assertEquals(bundle.capturedAt(), CAPTURED_AT);
        Assert.assertEquals(bundle.archive(), Path.of("evidence.zip").toAbsolutePath().normalize());
        Assert.assertEquals(bundle.context(), "NATIVE_APP");
        Assert.assertEquals(bundle.applicationMetadata(), Map.of("appPackage", "app.example"));
        Assert.assertEquals(bundle.deviceMetadata(), Map.of("platformName", "Android"));
        Assert.assertEquals(bundle.logMessages().size(), 1);
        Assert.assertEquals(bundle.logErrors().size(), 1);
        Assert.assertEquals(bundle.performanceSamples().size(), 1);
        Assert.assertEquals(bundle.artifacts().size(), 1);
        Assert.assertEquals(bundle.omissions(), Map.of("recording", "no-retained-recording"));

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> bundle.applicationMetadata().put("appActivity", "activity"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> bundle.deviceMetadata().put("orientation", "PORTRAIT"));
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.logMessages().clear());
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.logErrors().clear());
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.performanceSamples().clear());
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.artifacts().clear());
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> bundle.omissions().put("logs", "not-started"));
    }

    @Test
    public void bundleShouldNormalizeNullCollectionsToImmutableEmptySnapshots() {
        MobileEvidenceBundle bundle = new MobileEvidenceBundle(CAPTURED_AT, Path.of("evidence.zip"), "NATIVE_APP",
                null, null, null, null, null, null, null);

        Assert.assertEquals(bundle.applicationMetadata(), Map.of());
        Assert.assertEquals(bundle.deviceMetadata(), Map.of());
        Assert.assertEquals(bundle.logMessages(), List.of());
        Assert.assertEquals(bundle.logErrors(), List.of());
        Assert.assertEquals(bundle.performanceSamples(), List.of());
        Assert.assertEquals(bundle.artifacts(), List.of());
        Assert.assertEquals(bundle.omissions(), Map.of());
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> bundle.applicationMetadata().put("appPackage", "app"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> bundle.deviceMetadata().put("platformName", "Android"));
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.logMessages().clear());
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.logErrors().clear());
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.performanceSamples().clear());
        Assert.expectThrows(UnsupportedOperationException.class, () -> bundle.artifacts().clear());
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> bundle.omissions().put("logs", "empty"));
    }

    @Test
    public void bundleShouldRejectInvalidIdentityAndCollectionMembers() {
        Assert.expectThrows(NullPointerException.class, () -> bundle(null, Path.of("evidence.zip"), "NATIVE_APP",
                Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of()));
        Assert.expectThrows(NullPointerException.class, () -> bundle(CAPTURED_AT, null, "NATIVE_APP",
                Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of()));
        Assert.expectThrows(NullPointerException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"), null,
                Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of()));
        Assert.expectThrows(IllegalArgumentException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"), " ",
                Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of()));

        Map<String, String> nullKey = new LinkedHashMap<>();
        nullKey.put(null, "value");
        Assert.expectThrows(NullPointerException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", nullKey, Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of()));
        Map<String, String> nullValue = new LinkedHashMap<>();
        nullValue.put("appPackage", null);
        Assert.expectThrows(NullPointerException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", nullValue, Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of()));
        List<MobileLogMessage> nullMessage = new ArrayList<>();
        nullMessage.add(null);
        Assert.expectThrows(NullPointerException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", Map.of(), Map.of(), nullMessage, List.of(), List.of(), List.of(), Map.of()));
    }

    @Test
    public void bundleShouldRejectUnsafeMetadataOmissionsAndDuplicateArtifactIds() {
        String secret = "SECRET-REJECTED-EVIDENCE";
        IllegalArgumentException unsafeKey = Assert.expectThrows(IllegalArgumentException.class, () -> bundle(
                CAPTURED_AT, Path.of("evidence.zip"), "NATIVE_APP", Map.of(secret, "value"), Map.of(), List.of(),
                List.of(), List.of(), List.of(), Map.of()));
        Assert.assertFalse(unsafeKey.getMessage().contains(secret));
        Assert.expectThrows(IllegalArgumentException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", Map.of(), Map.of("udid", "secret"), List.of(), List.of(), List.of(), List.of(),
                Map.of()));
        IllegalArgumentException unsafeCode = Assert.expectThrows(IllegalArgumentException.class, () -> bundle(
                CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(),
                Map.of("source", secret)));
        Assert.assertFalse(unsafeCode.getMessage().contains(secret));
        Assert.expectThrows(IllegalArgumentException.class, () -> bundle(CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(),
                Map.of("unknown-component", "unsupported")));
        IllegalArgumentException duplicateId = Assert.expectThrows(IllegalArgumentException.class, () -> bundle(
                CAPTURED_AT, Path.of("evidence.zip"),
                "NATIVE_APP", Map.of(), Map.of(), List.of(), List.of(), List.of(), List.of(
                        artifact(secret, "first.png"), artifact(secret, "second.png")), Map.of()));
        Assert.assertFalse(duplicateId.getMessage().contains(secret));
    }

    @Test
    public void bundleShouldValidateAndRetainTheSameMetadataSnapshot() {
        Map<String, String> changing = new ChangingMap(
                Map.entry("appPackage", "app.example"), Map.entry("sessionId", "secret"));

        MobileEvidenceBundle bundle = bundle(CAPTURED_AT, Path.of("evidence.zip"), "NATIVE_APP", changing,
                Map.of(), List.of(), List.of(), List.of(), List.of(), Map.of());

        Assert.assertEquals(bundle.applicationMetadata(), Map.of("appPackage", "app.example"));
        Assert.assertFalse(bundle.applicationMetadata().containsKey("sessionId"));
    }

    @Test
    public void bundleShouldAcceptEveryDocumentedMetadataAndOmissionValue() {
        Map<String, String> application = Map.of(
                "appPackage", "package", "appActivity", "activity", "bundleId", "bundle",
                "applicationState", "RUNNING_IN_FOREGROUND");
        Map<String, String> device = Map.of(
                "platformName", "Android", "platformVersion", "13", "automationName", "UiAutomator2",
                "orientation", "PORTRAIT", "windowSize", "1080x2400");
        List<String> components = List.of(
                "screenshot", "source", "logs", "logErrors", "performance", "recording", "applicationState");
        List<String> codes = List.of(
                "unsupported", "not-started", "empty", "sensitive", "oversized", "provider-failed",
                "changed-during-capture", "no-retained-recording", "active", "missing", "changed");

        MobileEvidenceBundle metadataBundle = bundle(CAPTURED_AT, Path.of("evidence.zip"), "NATIVE_APP",
                application, device, List.of(), List.of(), List.of(), List.of(), Map.of());
        Assert.assertEquals(metadataBundle.applicationMetadata(), application);
        Assert.assertEquals(metadataBundle.deviceMetadata(), device);
        for (String component : components) {
            for (String code : codes) {
                Assert.assertEquals(bundle(CAPTURED_AT, Path.of("evidence.zip"), "NATIVE_APP", Map.of(), Map.of(),
                        List.of(), List.of(), List.of(), List.of(), Map.of(component, code)).omissions(),
                        Map.of(component, code));
            }
        }
    }

    @Test
    public void bundleStringShouldExposeCountsOnly() {
        String secret = "SECRET-MOBILE-EVIDENCE";
        MobileEvidenceBundle bundle = bundle(CAPTURED_AT, Path.of(secret + ".zip"), secret,
                Map.of("appPackage", secret), Map.of("platformName", secret),
                List.of(new MobileLogMessage(CAPTURED_AT, "logcat", secret)),
                List.of(new MobileLogError(CAPTURED_AT, "syslog", "type", secret)),
                List.of(new MobilePerformanceSample(CAPTURED_AT, secret, "memoryinfo", List.of("value"),
                        List.of(List.of(secret)))),
                List.of(new TraceArtifactReference("screenshot", "screenshot", "screenshot.png", "image/png",
                        true, Map.of("reason", secret))), Map.of("source", "provider-failed"));

        String rendered = bundle.toString();

        Assert.assertEquals(rendered, "MobileEvidenceBundle[capturedAt=2026-08-12T00:00:00Z, "
                + "applicationMetadata=1, deviceMetadata=1, logMessages=1, logErrors=1, "
                + "performanceSamples=1, artifacts=1, omissions=1]");
        Assert.assertFalse(rendered.contains(secret));
    }

    private static MobileEvidenceBundle bundle(Instant capturedAt, Path archive, String context,
                                                Map<String, String> applicationMetadata,
                                                Map<String, String> deviceMetadata,
                                                List<MobileLogMessage> logMessages,
                                                List<MobileLogError> logErrors,
                                                List<MobilePerformanceSample> performanceSamples,
                                                List<TraceArtifactReference> artifacts,
                                                Map<String, String> omissions) {
        return new MobileEvidenceBundle(capturedAt, archive, context, applicationMetadata, deviceMetadata,
                logMessages, logErrors, performanceSamples, artifacts, omissions);
    }

    private static TraceArtifactReference artifact(String id, String path) {
        return new TraceArtifactReference(id, "screenshot", path, "image/png", false, Map.of());
    }

    private static final class ChangingMap extends AbstractMap<String, String> {
        private final Entry<String, String> first;
        private final Entry<String, String> second;
        private int reads;

        private ChangingMap(Entry<String, String> first, Entry<String, String> second) {
            this.first = first;
            this.second = second;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int size() {
            return 1;
        }

        @Override
        public Set<Entry<String, String>> entrySet() {
            return new LinkedHashSet<>(Set.of(reads++ == 0 ? first : second));
        }
    }
}
