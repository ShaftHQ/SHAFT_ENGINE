package com.shaft.capture.generate;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.healing.HealingRequest;
import com.shaft.gui.internal.locator.Locator;
import com.shaft.heal.ShaftHeal;
import com.shaft.heal.internal.ShaftHealingProvider;
import com.shaft.heal.model.HealingDecision;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Issue #4172: shaft-capture must actually call the fingerprint-seeding seam that issue #4161 /
 * PR #4173 shipped into shaft-engine/shaft-heal but left unreachable. This follows the same shape
 * as {@code ShaftHealingProviderTest#seededFingerprintShouldSatisfyHistoryLookupWithoutALiveElementOrDriver}
 * one layer up: a freshly generated test, compiled and replayed from a clean output directory, must
 * leave recorded fingerprint history behind that a later lookup at the SAME absolute
 * {@code healing.history.path} finds -- proven by asserting the lookup does not return NO_HISTORY.
 */
class CaptureGeneratorHealingSeedingTest {
    @TempDir
    Path temp;

    @AfterEach
    void cleanup() {
        com.shaft.properties.internal.Properties.clearForCurrentThread();
        ShaftHeal.clear();
    }

    @Test
    void freshlyGeneratedTestLeavesFingerprintHistoryASubsequentReplayLookupFinds() throws Exception {
        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "healing-seed-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(2),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                RedactionSummary.empty(),
                Map.of());
        Path sessionPath = temp.resolve("capture.json");
        new CaptureJsonCodec().write(sessionPath, session);
        Path output = temp.resolve("gen");

        GeneratedTestValidator fakePassingReplay = new GeneratedTestValidator() {
            @Override
            public CaptureGenerationReport.Validation compile(Path source, Path classesDirectory) {
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED, List.of(), 0);
            }

            @Override
            public CaptureGenerationReport.Validation replay(
                    String fullyQualifiedClassName,
                    Path classesDirectory,
                    Path resourcesDirectory,
                    Path workDirectory,
                    Duration timeout) {
                // Fakes a PASSED replay without spawning a real subprocess/browser: this test proves
                // the seeding wiring, not GeneratedTestValidator's own (already-covered) replay path.
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED, List.of(), 1);
            }
        };

        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), fakePassingReplay, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        sessionPath, output, "generated.capture", "HealingSeedTest", false,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        Path expectedHistoryPath = output.resolve(".shaft-heal/history.json").normalize();
        assertTrue(source.contains(".strategy(\"shaft-heal\")"), source);
        assertTrue(source.contains(".aiTrigger(\"below-threshold\")"), source);
        // Issue #4027: generated tests get the deterministic re-suggestion ladder always-on (60s
        // hard budget) without touching the engine-wide healing.ladder.budgetSeconds default (0).
        assertTrue(source.contains(".ladderBudgetSeconds(60)"), source);
        assertTrue(Files.exists(expectedHistoryPath), "seeded history file should exist at the absolute path");

        // The SAME absolute path a later real run's setUp() would configure -- proves the history
        // lookup succeeds instead of short-circuiting to NO_HISTORY (issue #4161's own proof, one
        // layer up: generation-time evidence must be visible to a later, independent resolve()).
        SHAFT.Properties.healing.set()
                .strategy("shaft-heal")
                .historyPath(expectedHistoryPath.toString());
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.test/form");
        By originalLocator = Locator.hasRole(com.shaft.gui.internal.locator.Role.TEXTBOX)
                .hasNormalizedText("Username").build();
        ShaftHealingProvider provider = new ShaftHealingProvider();

        provider.resolve(new HealingRequest(driver, originalLocator, "TYPE", true, null, null, null));

        assertNotEquals(
                HealingDecision.Status.NO_HISTORY,
                ShaftHeal.lastReport().orElseThrow().decision().status());
    }

    /**
     * Issue #4188 gap B: {@code runtimeLocator(TargetPlan)} seeded history only under the primary
     * candidate's {@code By.toString()}, never the fallback chain. But when {@code
     * request.fallbackLocators()==true}, the generated code calls {@code
     * captureReplayLocator(primary, alt1, ...)}, which can resolve to an alternate at runtime if the
     * primary degrades -- and {@code HealingSupport.locator()} keys its history lookup by exact
     * {@code By.toString()}. This proves seeding now covers that alternate, not just the primary.
     *
     * <p>Issue #4239 P1.4-decision ladder: the fallback alternative must itself clear rung 1 or rung
     * 2 (a {@link CaptureFixtures#target()}'s lower-scored CSS candidate no longer qualifies, since
     * CSS is never ladder-eligible), so this uses a bespoke element with a second, independently
     * ladder-eligible {@code ACCESSIBLE_NAME} candidate carrying its own self-verified
     * {@code replayXpath} instead.
     */
    @Test
    void fallbackCandidateAlsoLeavesFingerprintHistoryASubsequentReplayLookupFinds() throws Exception {
        com.shaft.capture.model.ElementSnapshot usernameInput = new com.shaft.capture.model.ElementSnapshot(
                "username-input",
                "input",
                "textbox",
                "Username",
                "Username",
                Map.of("autocomplete", "username", "name", "username"),
                List.of(
                        new com.shaft.capture.model.LocatorCandidate(
                                com.shaft.capture.model.LocatorCandidate.LocatorStrategy.ROLE,
                                "textbox:Username", 1, true, true,
                                java.util.Set.of(com.shaft.capture.model.LocatorCandidate.LocatorSignal.ACCESSIBLE),
                                "", true),
                        new com.shaft.capture.model.LocatorCandidate(
                                com.shaft.capture.model.LocatorCandidate.LocatorStrategy.ACCESSIBLE_NAME,
                                "Username", 1, true, true,
                                java.util.Set.of(com.shaft.capture.model.LocatorCandidate.LocatorSignal.ACCESSIBLE),
                                "//input[normalize-space(@aria-label)=\"Username\"]")),
                true,
                true,
                false);
        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "healing-seed-fallback-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(2),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), usernameInput,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                RedactionSummary.empty(),
                Map.of());
        Path sessionPath = temp.resolve("capture.json");
        new CaptureJsonCodec().write(sessionPath, session);
        Path output = temp.resolve("gen");

        GeneratedTestValidator fakePassingReplay = new GeneratedTestValidator() {
            @Override
            public CaptureGenerationReport.Validation compile(Path source, Path classesDirectory) {
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED, List.of(), 0);
            }

            @Override
            public CaptureGenerationReport.Validation replay(
                    String fullyQualifiedClassName,
                    Path classesDirectory,
                    Path resourcesDirectory,
                    Path workDirectory,
                    Duration timeout) {
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED, List.of(), 1);
            }
        };

        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), fakePassingReplay, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        sessionPath, output, "generated.capture", "HealingSeedFallbackTest", false,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll(), true));

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        Path expectedHistoryPath = output.resolve(".shaft-heal/history.json").normalize();
        assertTrue(Files.exists(expectedHistoryPath), "seeded history file should exist at the absolute path");

        SHAFT.Properties.healing.set()
                .strategy("shaft-heal")
                .historyPath(expectedHistoryPath.toString());
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.test/form");
        // The lower-scored ACCESSIBLE_NAME candidate -- never the primary (verified ROLE) By, but
        // exactly what captureReplayLocator() falls back to at replay time if the primary degrades.
        // Mirrors runtimeLocator()'s own replayXpath branch: By.xpath(candidate.replayXpath()).
        By alternateLocator = By.xpath("//input[normalize-space(@aria-label)=\"Username\"]");
        ShaftHealingProvider provider = new ShaftHealingProvider();

        provider.resolve(new HealingRequest(driver, alternateLocator, "TYPE", true, null, null, null));

        assertNotEquals(
                HealingDecision.Status.NO_HISTORY,
                ShaftHeal.lastReport().orElseThrow().decision().status());
    }
}
