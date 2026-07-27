package com.shaft.capture.generate;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureGeneratorTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @TempDir
    Path temp;

    @Test
    void representativeSessionGeneratesDeterministicCompilableSourceAndExternalData() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        CaptureGenerationResult first = new CaptureGenerator().generate(request(session, temp.resolve("first")));
        CaptureGenerationResult second = new CaptureGenerator().generate(request(session, temp.resolve("second")));

        assertGeneratedUnconfirmed(first);
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                first.report().compilation().status());
        assertTrue(Files.isRegularFile(first.reviewPath()));
        assertTrue(Files.isRegularFile(first.reviewUiPath()));
        var review = JSON.readTree(first.reviewPath().toFile());
        assertEquals("1.0", review.path("schemaVersion").asText());
        assertEquals(first.report().sessionId(), review.path("sessionId").asText());
        assertTrue(review.path("readinessScore").asInt() >= 0);
        List<String> blockers = new java.util.ArrayList<>();
        review.path("blockers").forEach(blocker -> blockers.add(blocker.asText()));
        assertTrue(blockers.stream()
                .anyMatch(blocker -> blocker.contains("data.password")
                        && blocker.contains("environment variable")));
        assertTrue(blockers.stream()
                .anyMatch(blocker -> blocker.contains("upload.avatar")
                        && blocker.contains("fixture")));
        // Issue #4172: the emitted healing.history.path is an ABSOLUTE path derived from each
        // call's own output directory (by design -- a later real run must resolve it to the SAME
        // location generation-time seeding wrote to), so "first" and "second" legitimately differ
        // on that one line even though every other line is still a pure function of the session.
        assertEquals(
                withStableHealingHistoryPath(Files.readString(first.sourcePath())),
                withStableHealingHistoryPath(Files.readString(second.sourcePath())));
        assertEquals(Files.readString(first.testDataPath()), Files.readString(second.testDataPath()));
        assertEquals(first.report(), second.report());

        String source = Files.readString(first.sourcePath());
        String data = Files.readString(first.testDataPath());
        String golden = Files.readString(Path.of(
                "src/test/resources/fixtures/golden-generated-session-1.java"));
        assertEquals(normalizeLineEndings(golden), normalizeLineEndings(withStableHealingHistoryPath(source)));
        assertTrue(source.contains("@AfterMethod(alwaysRun = true)"));
        assertTrue(source.contains("driver.quit();"));
        assertTrue(source.contains("SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()).text()"));
        assertFalse(source.contains("alice"));
        assertTrue(data.contains("\"username\" : \"alice\""));
        assertFalse(data.toLowerCase().contains("password"));
        String workbench = Files.readString(first.reviewUiPath());
        assertTrue(workbench.contains("Review Summary"));
        assertTrue(workbench.contains("Required inputs"));
        assertTrue(workbench.contains("Locator decisions"));
        assertTrue(workbench.contains("Control-flow suggestions"));
        assertTrue(workbench.contains("Code blocks"));
        assertTrue(workbench.contains("Build record command"));
        assertTrue(workbench.contains("Playwright Codegen Feature Map"));
        assertTrue(workbench.contains("capture checkpoint"));
        assertTrue(workbench.contains("--shaft-primary"));
        assertTrue(workbench.contains("status-chip"));

    }

    @Test
    void generateReportsMilestoneProgressWithoutChangingTheResult() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        List<double[]> fractions = new ArrayList<>();
        List<String> messages = new ArrayList<>();
        CaptureGenerationResult result = new CaptureGenerator().generate(
                request(session, temp.resolve("progress")),
                CaptureGenerator.CodegenBackend.WEBDRIVER,
                (fraction, message) -> {
                    fractions.add(new double[] {fraction});
                    messages.add(message);
                });

        assertGeneratedUnconfirmed(result);
        assertTrue(fractions.size() >= 2,
                "expected at least two progress milestones, got: " + messages);
        assertTrue(messages.stream().anyMatch(message -> !message.isBlank()));
        for (int index = 1; index < fractions.size(); index++) {
            assertTrue(fractions.get(index)[0] >= fractions.get(index - 1)[0],
                    "progress fractions should be non-decreasing: " + messages);
        }
        assertEquals(1.0, fractions.get(fractions.size() - 1)[0], 0.0001,
                "the final milestone should report full completion");
    }

    @Test
    void recorderOverlayResourceUsesShaftUiTheme() throws Exception {
        try (InputStream stream = CaptureGeneratorTest.class
                .getResourceAsStream("/browser/shaft-capture-recorder.js")) {
            assertTrue(stream != null, "Recorder resource should be available on the test classpath.");
            String recorder = new String(stream.readAllBytes(), StandardCharsets.UTF_8);

            assertTrue(recorder.contains("--shaft-primary"));
            assertTrue(recorder.contains("status-chip"));
            assertTrue(recorder.contains("shaft-capture-assert"));
            assertTrue(recorder.contains("shaft-capture-pick"));
            assertTrue(recorder.contains("locator_preference"));
            assertTrue(recorder.contains("shaft-capture-readiness"));
            assertTrue(recorder.contains("readinessState"));
            assertTrue(recorder.contains("viewBox=\"0 0 24 24\""));
            assertTrue(recorder.contains("aria-label=\"Add assertion\""));
            assertTrue(recorder.contains("aria-label=\"Toggle locator picker\""));
            assertTrue(recorder.contains("kind: \"verification\""));
            assertTrue(recorder.contains("kind: \"step_reorder\""));
            assertTrue(recorder.contains("overflow-x: hidden"));
            assertTrue(recorder.contains("pendingSignals"));
            assertTrue(recorder.contains("pagehide"));
            assertTrue(recorder.contains("beforeunload"));
            assertTrue(recorder.contains("shaft-capture-assertion-panel"));
            assertTrue(recorder.contains("ELEMENT_ASSERTIONS"));
            assertTrue(recorder.contains("BROWSER_ASSERTIONS"));
            assertTrue(recorder.contains("assertion-locator-step"));
            assertTrue(recorder.contains("Manual locator (XPath or CSS)"));
            assertTrue(recorder.contains("shaft-capture-dialog"));
            assertFalse(recorder.contains("shaft-capture-checkpoint"));
            assertFalse(recorder.contains("aria-label=\"Add checkpoint\""));
            assertFalse(recorder.contains("prompt("));
        }
    }

    /**
     * Issue #4188 gap A: {@code fingerprintSeed()} reads {@code title} from {@code
     * normalizedAttributes} and folds {@code alt}/{@code aria-labelledby} into {@code
     * semanticAttributes}, but the recorder never preserved those as raw attributes -- only as a
     * name-computation fallback -- so every real capture session left them structurally blank.
     */
    @Test
    void recorderAttributeAllowlistCapturesTitleAltAndAriaLabelledby() throws Exception {
        try (InputStream stream = CaptureGeneratorTest.class
                .getResourceAsStream("/browser/shaft-capture-recorder.js")) {
            assertTrue(stream != null, "Recorder resource should be available on the test classpath.");
            String recorder = new String(stream.readAllBytes(), StandardCharsets.UTF_8);

            assertTrue(recorder.contains(
                    "\"aria-label\", \"role\", \"title\", \"alt\", \"aria-labelledby\"].forEach(name => {"),
                    recorder);
        }
    }

    @Test
    void generatedNamesAndCommentsUseRecorderIntentBeforeOpaqueSessionIds() throws Exception {
        EventContext navigation = new EventContext(
                1,
                CaptureFixtures.STARTED.plusSeconds(1),
                new com.shaft.capture.model.PageContext(
                        "https://shop.example/checkout",
                        "Checkout review",
                        "window-1",
                        List.of(),
                        1440,
                        900),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of());
        EventContext describedClick = new EventContext(
                2,
                CaptureFixtures.STARTED.plusSeconds(2),
                navigation.page(),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of("userDescription", JSON.getNodeFactory().textNode("Click Pay now after entering card data")));
        ElementSnapshot payButton = new ElementSnapshot(
                "pay-button",
                "button",
                "button",
                "Pay now",
                "",
                Map.of("type", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:Pay now", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                        "", true)),
                true,
                true,
                false);
        CaptureSession capture = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "2f6e9f58-9c65-45c6-bc7a-opaque",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(navigation,
                                CaptureEvent.NavigationAction.OPEN, "https://shop.example/checkout"),
                        new CaptureEvent.ClickEvent(describedClick, payButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(new Checkpoint("checkout-flow", 2, CaptureFixtures.STARTED.plusSeconds(3),
                        Checkpoint.CheckpointKind.USER_MARKER, "checkout happy path")),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session(capture), temp.resolve("intent-names")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(result.sourcePath().getFileName().toString().contains("CheckoutHappyPathTest.java"));
        assertTrue(source.contains("public class CheckoutHappyPathTest"));
        assertTrue(source.contains("public void replayCheckoutHappyPath()"));
        assertTrue(source.contains("// Captured step: Click Pay now after entering card data"));
    }

    @Test
    void generatedSourceIncludesReviewHeaderAndSessionGoal() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession goalSession = new CaptureSession(
                base.schemaVersion(),
                "goal-session",
                base.status(),
                base.startedAt(),
                base.endedAt(),
                base.browser(),
                base.events().subList(0, 2),
                List.of(),
                List.of(CaptureFixtures.ordinary()),
                base.redactionSummary(),
                Map.of("sessionGoal", JSON.getNodeFactory().textNode("record checkout happy path")));
        Path session = session(goalSession);
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("goal-session")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("// Capture review: readiness="));
        assertTrue(source.contains("// Capture goal: record checkout happy path"));
    }

    @Test
    void sessionGoalDrivesGeneratedClassAndMethodNames() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession goalSession = new CaptureSession(
                base.schemaVersion(),
                "goal-naming-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        "https://example.test/login")),
                List.of(),
                List.of(),
                base.redactionSummary(),
                Map.of("sessionGoal", JSON.getNodeFactory().textNode("Log in as a valid user")));
        Path session = session(goalSession);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("goal-naming")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("public class LogInAsAValidUserTest"), source);
        assertTrue(source.contains("public void logInAsAValidUser()"), source);
        assertFalse(source.contains("replayHttps"), source);
    }

    @Test
    void percentEncodedWorkspaceFileUrlIsNotAPrivacyBlocker() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        Path output = temp.resolve("My Files");
        String fixtureUrl = output.resolve("fixtures/login page.html").toUri().toString();
        assertTrue(fixtureUrl.contains("%20"), "Fixture URL should percent-encode spaces: " + fixtureUrl);
        CaptureSession recorded = new CaptureSession(
                base.schemaVersion(),
                "percent-encoded-workspace-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        fixtureUrl)),
                List.of(),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(recorded);

        CaptureGenerationResult result = new CaptureGenerator().generate(request(session, output));

        assertTrue(result.report().unsupportedEvents().stream()
                        .noneMatch(message -> message.startsWith("privacy:")),
                result.report().unsupportedEvents().toString());
        assertGeneratedUnconfirmed(result);
    }

    @Test
    void externalizedSecretEnvironmentNamesStayStableAcrossReRecordings() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        String firstRun = generateWithSecretReference(base, "data.password-4", "first-secret-run");
        String secondRun = generateWithSecretReference(base, "data.password-7", "second-secret-run");

        assertTrue(firstRun.contains("requiredEnvironment(\"SHAFT_CAPTURE_DATA_PASSWORD\")"), firstRun);
        assertFalse(firstRun.contains("SHAFT_CAPTURE_DATA_PASSWORD_4"), firstRun);
        assertEquals(
                extractRequiredEnvironmentNames(firstRun),
                extractRequiredEnvironmentNames(secondRun),
                "Re-recording the same journey must not shift externalized-secret environment names");
    }

    private String generateWithSecretReference(CaptureSession base, String referenceId, String outputName)
            throws Exception {
        ExternalTestDataReference secret = new ExternalTestDataReference(
                referenceId,
                referenceId.substring("data.".length()),
                ExternalTestDataReference.DataSource.ENVIRONMENT,
                "",
                "",
                ExternalTestDataReference.DataClassification.SECRET);
        CaptureSession recorded = new CaptureSession(
                base.schemaVersion(),
                outputName,
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.TypeEvent(
                        CaptureFixtures.context(1), CaptureFixtures.target(), secret)),
                List.of(),
                List.of(secret),
                base.redactionSummary(),
                base.extensions());
        Path path = temp.resolve(outputName + ".json");
        new CaptureJsonCodec().write(path, recorded);
        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(path, temp.resolve(outputName)));
        assertGeneratedUnconfirmed(result);
        return Files.readString(result.sourcePath());
    }

    private static List<String> extractRequiredEnvironmentNames(String source) {
        List<String> names = new java.util.ArrayList<>();
        java.util.regex.Matcher matcher = java.util.regex.Pattern
                .compile("requiredEnvironment\\(\"([^\"]+)\"\\)")
                .matcher(source);
        while (matcher.find()) {
            names.add(matcher.group(1));
        }
        return names;
    }

    @Test
    void unsupportedStepFailsWithEventIdAndRemediation() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureEvent.ClickEvent click = (CaptureEvent.ClickEvent) base.events().get(1);
        List<CaptureEvent> events = new java.util.ArrayList<>(base.events());
        events.set(1, new CaptureEvent.ClickEvent(
                click.context(), click.target(), CaptureEvent.MouseButton.SECONDARY, 1));
        CaptureSession unsupported = new CaptureSession(
                base.schemaVersion(), base.sessionId(), base.status(), base.startedAt(), base.endedAt(),
                base.browser(), events, base.checkpoints(), base.dataReferences(),
                base.redactionSummary(), base.extensions());
        Path session = session(unsupported);
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("unsupported")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("event-2") && message.contains("primary-button")));
        assertFalse(Files.exists(result.sourcePath()));
    }

    @Test
    void unsupportedAssertionCheckpointKeepsRecorderDescriptionInReport() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession unsupported = new CaptureSession(
                base.schemaVersion(),
                "unsupported-assertion-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(base.events().getFirst()),
                List.of(new Checkpoint(
                        "unsupported-assertion",
                        1,
                        CaptureFixtures.STARTED.plusSeconds(1),
                        Checkpoint.CheckpointKind.ASSERTION,
                        "Unsupported assertion type: CSS matches")),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(unsupported);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("unsupported-assertion")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("Unsupported assertion type: CSS matches")));
    }

    @Test
    void assertionCheckpointWithoutVerificationDoesNotClearMissingAssertionWarning() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession unsupported = new CaptureSession(
                base.schemaVersion(),
                "assertion-checkpoint-only-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        "https://example.test/checkout")),
                List.of(new Checkpoint(
                        "assertion-note",
                        2,
                        CaptureFixtures.STARTED.plusSeconds(2),
                        Checkpoint.CheckpointKind.ASSERTION,
                        "cart total is visible")),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(unsupported);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("assertion-checkpoint-only")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("SHAFT assertion builders only")));
        assertTrue(result.report().warnings().stream()
                .anyMatch(message -> message.contains("review/ASSERTION")
                        && message.contains("Record a verification for the post-action page state.")));
    }

    @Test
    void flowBoundaryCheckpointsGenerateReusableMethods() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        List<Checkpoint> checkpoints = new java.util.ArrayList<>(base.checkpoints());
        checkpoints.add(new Checkpoint("login-flow-start", 1, CaptureFixtures.STARTED.plusSeconds(1),
                Checkpoint.CheckpointKind.FLOW_START, "login as admin"));
        checkpoints.add(new Checkpoint("login-flow-end", 3, CaptureFixtures.STARTED.plusSeconds(3),
                Checkpoint.CheckpointKind.FLOW_END, "login as admin"));
        CaptureSession segmented = new CaptureSession(
                base.schemaVersion(),
                "segmented-session",
                base.status(),
                base.startedAt(),
                base.endedAt(),
                base.browser(),
                base.events(),
                checkpoints,
                base.dataReferences(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(segmented);
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("segmented")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("        loginAsAdmin();"));
        assertTrue(source.contains("    private void loginAsAdmin() throws Exception {"));
        assertEquals(1, count(source, "driver.element().type(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()"));
        assertTrue(source.indexOf("        loginAsAdmin();")
                < source.indexOf("    private void loginAsAdmin() throws Exception {"));
        assertFalse(source.contains("FLOW_START"));
        assertFalse(source.contains("FLOW_END"));
    }

    @Test
    void playwrightBackendGeneratesCompilableShaftPlaywrightSource() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession simple = new CaptureSession(
                base.schemaVersion(),
                "playwright-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                base.startedAt().plusSeconds(5),
                base.browser(),
                base.events().subList(0, 4),
                List.of(),
                List.of(CaptureFixtures.ordinary()),
                base.redactionSummary(),
                base.extensions());
        Path session = session(simple);
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator().generate(
                request(session, temp.resolve("playwright")),
                CaptureGenerator.CodegenBackend.PLAYWRIGHT);

        assertGeneratedUnconfirmed(result);
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                result.report().compilation().status());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("private SHAFT.GUI.Playwright driver;"));
        assertTrue(source.contains("driver = new SHAFT.GUI.Playwright();"));
        assertTrue(source.contains("driver.element().click(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build());"));
        assertFalse(source.contains("DriverFactory"));
        assertFalse(source.contains("ExpectedConditions"));
    }

    @Test
    void generatorRendersBrowserTextTitleAndImageVerificationCheckpoints() throws Exception {
        ExternalTestDataReference expected = CaptureFixtures.ordinary();
        CaptureSession verificationSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "verification-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(2),
                                CaptureEvent.VerificationKind.TITLE_CONTAINS, null, expected, false),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(3),
                                CaptureEvent.VerificationKind.PAGE_TEXT_CONTAINS, null, expected, false),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(4),
                                CaptureEvent.VerificationKind.ELEMENT_IMAGE_MATCHES,
                                CaptureFixtures.target(), null, false),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(5),
                                CaptureEvent.VerificationKind.ELEMENT_IMAGE_MATCHES,
                                CaptureFixtures.target(), null, true)),
                List.of(),
                List.of(expected),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(verificationSession);
        writeCaptureData("Welcome");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("verification")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("driver.browser().assertThat().title().contains(requiredData(\"username\"));"));
        assertTrue(source.contains("driver.browser().assertThat().text().contains(requiredData(\"username\"));"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()).matchesReferenceImage();"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()).doesNotMatchReferenceImage();"));
    }

    @Test
    void generatorRendersAriaSnapshotAndScreenshotVerificationCheckpoints() throws Exception {
        ExternalTestDataReference expected = CaptureFixtures.ordinary();
        CaptureSession verificationSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "aria-screenshot-verification-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(2),
                                CaptureEvent.VerificationKind.ARIA_SNAPSHOT_MATCHES,
                                CaptureFixtures.target(), expected, false),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(3),
                                CaptureEvent.VerificationKind.SCREENSHOT_MATCHES,
                                CaptureFixtures.target(), null, false)),
                List.of(),
                List.of(expected),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(verificationSession);
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("aria-screenshot-verification")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains(
                "driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build())"
                        + ".matchesAriaSnapshot(requiredData(\"username\"));"));
        assertTrue(source.contains(
                "driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()).matchesScreenshot();"));
    }

    @Test
    void negatedAriaSnapshotAndScreenshotVerificationsAreUnsupported() throws Exception {
        ExternalTestDataReference expected = CaptureFixtures.ordinary();
        CaptureSession verificationSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "negated-aria-screenshot-verification-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(2),
                                CaptureEvent.VerificationKind.ARIA_SNAPSHOT_MATCHES,
                                CaptureFixtures.target(), expected, true),
                        new CaptureEvent.VerificationEvent(CaptureFixtures.context(3),
                                CaptureEvent.VerificationKind.SCREENSHOT_MATCHES,
                                CaptureFixtures.target(), null, true)),
                List.of(),
                List.of(expected),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(verificationSession);
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("negated-aria-screenshot")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(unsupported -> unsupported.contains("ARIA_SNAPSHOT_MATCHES")
                        && unsupported.contains("does not support negated verification")));
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(unsupported -> unsupported.contains("SCREENSHOT_MATCHES")
                        && unsupported.contains("does not support negated verification")));
    }

    @Test
    void recordedElementAssertionWithChosenLocatorAndTestDataGeneratesMatchingValidationsCall() throws Exception {
        ExternalTestDataReference expected = CaptureFixtures.ordinary();
        ElementSnapshot target = CaptureFixtures.target();
        EventContext context = new EventContext(
                2,
                CaptureFixtures.STARTED.plusSeconds(2),
                CaptureFixtures.page(),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of("attributeName", JSON.getNodeFactory().textNode("autocomplete")));
        CaptureSession attributeAssertionSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "attribute-assertion-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.VerificationEvent(context,
                                CaptureEvent.VerificationKind.ATTRIBUTE_EQUALS, target, expected, false)),
                List.of(),
                List.of(expected),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(attributeAssertionSession);
        writeCaptureData("alice");

        // Round-trip the recorded session JSON exactly as CaptureGenerator would read it from disk.
        CaptureSession recorded = new CaptureJsonCodec().read(session);
        CaptureEvent.VerificationEvent recordedAssertion =
                (CaptureEvent.VerificationEvent) recorded.events().get(1);
        assertEquals(CaptureEvent.VerificationKind.ATTRIBUTE_EQUALS, recordedAssertion.verification());
        assertEquals("username-input", recordedAssertion.target().logicalElementId());
        assertEquals("data.username", recordedAssertion.expected().id());

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("attribute-assertion")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains(
                "driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()).attribute(\"autocomplete\")"
                        + ".isEqualTo(requiredData(\"username\"));"));
    }

    /**
     * Issue #4239 P1.4-decision ladder: a fallback ALTERNATIVE must clear rung 1 or rung 2 exactly
     * like the primary candidate does -- the ladder filters {@code target.locatorCandidates()} down
     * to only eligible entries before ranking, so a genuinely fallback-worthy element needs a SECOND
     * independently ladder-eligible candidate (here, a distinct {@code ACCESSIBLE_NAME} candidate
     * carrying its own self-verified {@code replayXpath}), not merely a second recorded strategy.
     */
    @Test
    void fallbackLocatorReplayOptionEmitsCompactHelperOnlyWhenFallbacksExist() throws Exception {
        ElementSnapshot usernameInput = new ElementSnapshot(
                "username-input",
                "input",
                "textbox",
                "Username",
                "Username",
                Map.of("autocomplete", "username", "name", "username"),
                List.of(
                        new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                                "textbox:Username", 1, true, true,
                                java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true),
                        new LocatorCandidate(LocatorCandidate.LocatorStrategy.ACCESSIBLE_NAME,
                                "Username", 1, true, true,
                                java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                                "//input[normalize-space(@aria-label)=\"Username\"]")),
                true,
                true,
                false);
        CaptureSession fallbackSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "fallback-locator-session",
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
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(fallbackSession);

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("fallback-replay"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll(), true));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("import org.openqa.selenium.By;"));
        assertTrue(source.contains("private By captureReplayLocator(By primary, By... alternatives)"));
        assertTrue(source.contains("captureReplayLocator(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()"),
                source);
        assertTrue(result.report().fallbackLocators().stream()
                .anyMatch(fallback -> fallback.contains("username-input")));
    }

    /**
     * Issue #3905: a ROLE-strategy locator (the highest-scoring {@link LocatorCandidate.LocatorStrategy},
     * always preferred when present -- see the nightly "Guided Workflows Live E2E" login fixture, where
     * every field resolves to ROLE) renders {@code Role.BUTTON}/{@code Role.TEXTBOX} literals into the
     * generated source ({@code semanticLocator}) without ever adding the matching
     * {@code import com.shaft.gui.internal.locator.Role;} -- so real compilation fails with
     * "cannot find symbol: variable Role" every time a ROLE locator is chosen and generation reaches
     * the compile step. Reproduced deterministically here (no live browser needed): a session with a
     * single ROLE-only-candidate button, real javac compilation requested via {@link #request}.
     */
    @Test
    void roleStrategyLocatorGeneratesAnImportForTheRoleEnumSoCompilationSucceeds() throws Exception {
        Path session = session(roleLocatorSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator().generate(request(session, temp.resolve("role")));

        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("import com.shaft.gui.internal.locator.Role;"),
                "generated source uses Role.BUTTON but never imports Role: " + source);
        assertTrue(source.contains("Role.BUTTON"), source);
        assertGeneratedUnconfirmed(result);
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                result.report().compilation().status(),
                result.report().compilation().diagnostics().toString());
    }

    /**
     * Issue #TBD: the ROLE branch of {@code semanticLocator} computed the recorded accessible name
     * into a local {@code semanticName} and then discarded it, emitting the bare
     * {@code SHAFT.GUI.Locator.hasRole(Role.BUTTON).build()} -- which {@link com.shaft.gui.internal.locator.LocatorBuilder#byRole}
     * expands into a union of every button-shaped element on the page. Since ROLE is the
     * highest-scoring strategy and therefore almost always selected, every recorded step generated a
     * locator that matched many elements instead of the one that was clicked, so replay hit the wrong
     * element or failed.
     *
     * <p>The fix chains the recorded name as a predicate onto the role locator so the union narrows
     * back down to the one recorded element. It must use
     * {@link com.shaft.gui.internal.locator.LocatorBuilder#hasNormalizedText}, not {@code hasText}:
     * the browser capture recorder whitespace-collapses and trims every recorded name before
     * persisting it ({@code shaft-capture-recorder.js}'s {@code text()} helper), so an exact raw
     * string-value comparison ({@code hasText}'s {@code [.="..."]}) fails against ordinary markup
     * whose text carries surrounding/internal whitespace -- see
     * {@code LocatorBuilderExtendedUnitTest#recordedNamePredicateShouldResolveRealisticWhitespacePaddedMarkup}
     * in shaft-engine for the real-markup-resolution proof of that mismatch.
     */
    @Test
    void roleStrategyLocatorChainsRecordedAccessibleNameAsHasNormalizedTextPredicate() throws Exception {
        Path session = session(roleLocatorSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("role-name")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasNormalizedText(\"Log in\").build()"),
                "generated ROLE locator must chain the recorded accessible name via hasNormalizedText so "
                        + "replay does not match every button on the page and still resolves against "
                        + "whitespace-padded real markup: " + source);
        assertFalse(source.contains("SHAFT.GUI.Locator.hasRole(Role.BUTTON).build()"), source);
        assertFalse(source.contains(".hasText(\"Log in\")"), source);
    }

    /**
     * When no accessible name was recorded for the target (e.g. an icon-only control), there is
     * nothing to narrow the ROLE union with, so generation must fall back to the bare
     * {@code hasRole(...).build()} form rather than emitting an empty/blank predicate.
     */
    @Test
    void roleStrategyLocatorWithoutAccessibleNameEmitsBareHasRole() throws Exception {
        Path session = session(roleLocatorSessionWithoutAccessibleName());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("role-blank-name")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasRole(Role.BUTTON).build()"), source);
    }

    /**
     * Issue #4271: a unique, stable, human-authored id is first-class emittable evidence (tier 1).
     * Before the unified policy it was doubly excluded -- the recorder never attaches a
     * {@code replayXpath} to an ID candidate so it failed the rung-2 gate, and had it passed, the
     * renderer emitted the literal {@code SHAFT.GUI.Locator.id(...)} form that the unconditional
     * {@code NON_ARIA_LOCATOR} guardrail rejects. It now renders through the SHAFT locator builder's
     * {@code hasId}, which is policy-clean and resolves to the optimal native XPath.
     */
    @Test
    void uniqueStableIdGeneratesTheShaftLocatorBuilderIdFormAndClearsEveryGuardrail() throws Exception {
        Path session = session(uniqueIdLocatorSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("unique-id")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasAnyTagName().hasId(\"login-btn\").build()"), source);
        assertFalse(source.contains("SHAFT.GUI.Locator.id("), source);
        assertTrue(CaptureGenerator.guardrailUnsupportedFindings(source).isEmpty(),
                CaptureGenerator.guardrailUnsupportedFindings(source).toString());
    }

    /**
     * Issue #4271 review finding 2: {@code LocatorRanker} ranks candidates lexicographically by
     * (tier, score) <em>within one element snapshot</em>, but when the same logical element is seen
     * again on a later event, {@code CaptureGenerator} merged the two selections by raw additive
     * score alone with the tier ignored. A {@code USER_PROVIDED} signal is worth +1000, so a tier-3
     * XPath selection on the second sighting displaced the tier-1 unique-id selection from the first
     * -- issue #4239's F2 defect shape, alive in the one code path the tier redesign claimed had
     * made it impossible. The merge must use the same lexicographic ordering as the ranker.
     */
    @Test
    void aLaterHigherScoringLowerTierSightingCannotDisplaceTheWinningTier() throws Exception {
        Path session = session(sameElementTwoSightingsSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("tier-merge")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasAnyTagName().hasId(\"login-btn\").build()"),
                "the tier-1 unique id must survive a later, higher-scoring tier-3 sighting: " + source);
        assertFalse(source.contains("By.xpath(\"//div[normalize-space(.)=\\\"Log in\\\"]\")"),
                "a USER_PROVIDED score boost must not promote a tier-3 selection over tier 1: " + source);
    }

    /**
     * Two events against one logical element. The first sighting carries a unique stable id (tier 1,
     * lower additive score); the second carries a self-verified XPath pinned with USER_PROVIDED
     * (tier 3, +1000 score).
     */
    private static CaptureSession sameElementTwoSightingsSession() {
        ElementSnapshot tierOneSighting = new ElementSnapshot(
                "login-button", "button", "", "", "", Map.of("id", "login-btn"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID,
                        "login-btn", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE))),
                true, true, false);
        ElementSnapshot tierThreeSighting = new ElementSnapshot(
                "login-button", "div", "", "Log in", "", Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.LABEL,
                        "Log in", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.USER_PROVIDED,
                                LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE),
                        "//div[normalize-space(.)=\"Log in\"]")),
                true, true, false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "same-element-two-sightings-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(4),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), tierOneSighting,
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(3), tierThreeSighting,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    /**
     * Issue #4271 review finding 3: {@code brittleLocatorWarning} inspected
     * {@code candidate.expression()}, but a tier-3 selection ships its {@code replayXpath} -- so an
     * indexed, positional XPath was emitted with no brittleness warning whenever the candidate's own
     * expression happened to look clean. The warning must inspect what is actually shipped.
     */
    @Test
    void brittlenessWarningInspectsTheRenderedLocatorNotTheRawCandidateExpression() throws Exception {
        Path session = session(indexedReplayXpathSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("brittle-replay-xpath")));

        assertGeneratedUnconfirmed(result);
        assertTrue(result.report().warnings().stream()
                        .anyMatch(warning -> warning.startsWith("review/LOCATOR/")
                                && warning.contains("login-button")
                                && warning.contains("//div[3]/button[2]")),
                "an indexed replayXpath must raise a brittleness warning naming what was actually "
                        + "shipped: " + result.report().warnings());
    }

    private static CaptureSession indexedReplayXpathSession() {
        ElementSnapshot loginButton = new ElementSnapshot(
                "login-button", "button", "", "Log in", "", Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.LABEL,
                        "Log in", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                        "//div[3]/button[2]")),
                true, true, false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "indexed-replay-xpath-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), loginButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    /**
     * Issue #4271 review finding 4: rendering a candidate the policy refused used to fall through to
     * {@code By.xpath("//*")} -- a locator matching the first element in the document, which is also
     * fed to {@code HealingManager.observeFingerprint} and would poison SHAFT Heal with a garbage
     * fingerprint. The generation gate makes this unreachable, so if it is ever reached the
     * invariant is broken and it must say so loudly rather than silently ship a wrong locator.
     */
    @Test
    void renderingAnUnplannableCandidateFailsLoudlyInsteadOfEmittingAMatchAnythingXpath() {
        ElementSnapshot target = new ElementSnapshot(
                "orphan", "div", "", "", "", Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "div:nth-child(2)", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.GENERATED))),
                true, true, false);
        LocatorCandidate unplannable = target.locatorCandidates().getFirst();

        assertThrows(IllegalStateException.class,
                () -> CaptureGenerator.locatorExpression(target, unplannable));
        assertThrows(IllegalStateException.class,
                () -> CaptureGenerator.runtimeLocator(target, unplannable));
    }

    private static CaptureSession uniqueIdLocatorSession() {
        ElementSnapshot loginButton = new ElementSnapshot(
                "login-button",
                "button",
                "",
                "",
                "",
                Map.of("id", "login-btn"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID,
                        "login-btn", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE))),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "unique-id-locator-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), loginButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    private static CaptureSession roleLocatorSession() {
        ElementSnapshot loginButton = new ElementSnapshot(
                "login-button",
                "button",
                "button",
                "Log in",
                "",
                Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:Log in", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true)),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "role-locator-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), loginButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    /**
     * Issue #4026: when the ROLE candidate's ARIA role has no {@link com.shaft.gui.internal.locator.Role}
     * enum equivalent (here {@code "alert"}, absent from {@code CaptureGenerator#ariaRole}'s 14-entry
     * table), {@code semanticLocator} used to reconstruct a predicate blind in Java --
     * {@code .hasAttribute("aria-label", name)} -- even when the recorded name actually came from the
     * element's own (whitespace-padded) inner text, not an {@code aria-label} attribute the element
     * doesn't have. That reconstruction cannot know which DOM signal produced the name and silently
     * emits a locator matching nothing. The in-page recorder knows the true signal and already
     * self-verified an XPath against the live DOM, using {@code normalize-space(.)} so internal
     * whitespace cannot break the match; when that {@code replayXpath} is present, generation must
     * emit it verbatim via {@code By.xpath(...)} instead of re-deriving a predicate.
     */
    @Test
    void unmappedRoleStrategyLocatorUsesRecordedReplayXpathVerbatimInsteadOfReconstructingAPredicate()
            throws Exception {
        Path session = session(unmappedRoleReplayXpathSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("unmapped-role-replay-xpath")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains(
                        "By.xpath(\"//div[normalize-space(.)=\\\"Something  went wrong\\\"]\")"),
                "generated locator must use the recorded, self-verified replayXpath verbatim: " + source);
        assertFalse(source.contains(".hasAttribute(\"aria-label\""),
                "must not fall back to the blind Java-side aria-label reconstruction "
                        + "when a replayXpath was recorded: " + source);
        assertTrue(source.contains("import org.openqa.selenium.By;"));
    }

    private static CaptureSession unmappedRoleReplayXpathSession() {
        ElementSnapshot alert = new ElementSnapshot(
                "alert-banner",
                "div",
                "alert",
                "Something  went wrong",
                "",
                Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "alert:Something  went wrong", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                        "//div[normalize-space(.)=\"Something  went wrong\"]")),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "unmapped-role-replay-xpath-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), alert,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    /**
     * Issue #4264: {@code isLadderEligible} admits a TEST_ID/CSS/ID/NAME candidate as rung-2
     * eligible whenever it carries a non-blank self-verified {@code replayXpath}, exactly like any
     * other strategy -- but {@code locatorExpression} used to switch on {@code strategy()} alone
     * for these four and always render the literal {@code SHAFT.GUI.Locator.id/name/cssSelector(...)}
     * form, which the unconditional {@code NON_ARIA_LOCATOR} guardrail bans regardless of ladder
     * eligibility: the two gates disagreed.
     *
     * <p>Issue #4271 kept the invariant this case exists to protect -- an eligible ID candidate must
     * never render the banned literal builder call -- but changed which form it renders. This id is
     * unique and stable, so it is now tier 1 and renders through {@code hasId}, the SHAFT locator
     * builder form the owner policy asks for, rather than the raw XPath. The original
     * {@code By.xpath(...)} expectation is preserved verbatim in
     * {@link #idStrategyCandidateThatIsNotUniqueRendersItsSelfVerifiedReplayXpath()}, which pins the
     * case where the id genuinely cannot be trusted on its own.
     */
    @Test
    void idStrategyCandidateWithSelfVerifiedReplayXpathRendersAsByXpathNotNonAriaLocator() throws Exception {
        Path session = session(idStrategyReplayXpathSession(1));
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("id-strategy-replay-xpath")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasAnyTagName().hasId(\"login-button\").build()"),
                "a unique stable id is tier 1 and renders through the SHAFT locator builder: " + source);
        assertFalse(source.contains("SHAFT.GUI.Locator.id("),
                "must not fall back to the NON_ARIA_LOCATOR-banned literal ID builder: " + source);
        assertTrue(CaptureGenerator.guardrailUnsupportedFindings(source).isEmpty(),
                CaptureGenerator.guardrailUnsupportedFindings(source).toString());
    }

    /**
     * Issue #4264's original scenario, preserved end-to-end: an ID candidate the recorder measured as
     * matching three elements cannot be trusted on its own, so it falls to the self-verified
     * replayXpath tier and must render as {@code By.xpath(...)} -- never the banned literal builder
     * call. Behaviour driven out at unit level by
     * {@code LocatorPolicyTest#nonUniqueIdWithAVerifiedReplayXpathRendersAsNativeXpathNotTheBannedIdBuilderCall};
     * this case keeps #4276's original assertions exercised through a full generation run.
     */
    @Test
    void idStrategyCandidateThatIsNotUniqueRendersItsSelfVerifiedReplayXpath() throws Exception {
        Path session = session(idStrategyReplayXpathSession(3));
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("id-strategy-replay-xpath-ambiguous")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("By.xpath(\"//button[@id=\\\"login-button\\\"]\")"),
                "an ID candidate that is not unique must render its self-verified replayXpath: " + source);
        assertFalse(source.contains("SHAFT.GUI.Locator.id("),
                "must not fall back to the NON_ARIA_LOCATOR-banned literal ID builder once "
                        + "self-verified XPath evidence is present: " + source);
        assertTrue(source.contains("import org.openqa.selenium.By;"));
    }

    private static CaptureSession idStrategyReplayXpathSession(int uniquenessCount) {
        ElementSnapshot loginButton = new ElementSnapshot(
                "login-button",
                "button",
                "button",
                "",
                "",
                Map.of("id", "login-button"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID,
                        "login-button", uniquenessCount, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.USER_PROVIDED),
                        "//button[@id=\"login-button\"]")),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "id-strategy-replay-xpath-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), loginButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    /**
     * Issue #4264 follow-up: the TEST_ID and CSS strategies share one switch arm with ID/NAME in
     * {@code locatorExpression} -- this covers that arm too, so the fix is not accidentally scoped
     * to only the ID case.
     */
    @Test
    void cssStrategyCandidateWithSelfVerifiedReplayXpathRendersAsByXpathNotNonAriaLocator() throws Exception {
        Path session = session(cssStrategyReplayXpathSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("css-strategy-replay-xpath")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("By.xpath(\"//button[@data-testid=\\\"submit\\\"]\")"),
                "a rung-2-eligible CSS candidate must render its self-verified replayXpath: " + source);
        assertFalse(source.contains("SHAFT.GUI.Locator.cssSelector("),
                "must not fall back to the NON_ARIA_LOCATOR-banned literal cssSelector builder once "
                        + "rung-2 evidence is present: " + source);
    }

    private static CaptureSession cssStrategyReplayXpathSession() {
        ElementSnapshot submitButton = new ElementSnapshot(
                "submit-button",
                "button",
                "button",
                "",
                "",
                Map.of("data-testid", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "[data-testid=\"submit\"]", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.TEST_ATTRIBUTE),
                        "//button[@data-testid=\"submit\"]")),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "css-strategy-replay-xpath-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), submitButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    private static CaptureSession roleLocatorSessionWithoutAccessibleName() {
        ElementSnapshot iconButton = new ElementSnapshot(
                "icon-button",
                "button",
                "button",
                "",
                "",
                Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true)),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "role-locator-session-blank-name",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), iconButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    @Test
    void controlFlowPreviewReportsSuggestionsWithoutChangingLinearReplay() throws Exception {
        Path preview = temp.resolve("control-flow-preview.json");
        Path session = session(controlFlowSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("control-flow-preview"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll(), false,
                CaptureGenerationRequest.ControlFlowMode.PREVIEW, preview));

        assertGeneratedUnconfirmed(result);
        assertTrue(Files.isRegularFile(preview));
        List<CaptureGenerationReport.ControlFlowKind> kinds = result.report().controlFlowSuggestions().stream()
                .map(CaptureGenerationReport.ControlFlowSuggestion::kind)
                .toList();
        assertTrue(kinds.contains(CaptureGenerationReport.ControlFlowKind.REPEATED_GROUP), kinds.toString());
        assertTrue(kinds.contains(CaptureGenerationReport.ControlFlowKind.OPTIONAL_GUARD), kinds.toString());
        assertTrue(kinds.contains(CaptureGenerationReport.ControlFlowKind.RECOVERY_REVIEW), kinds.toString());
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("review/CONTROL_FLOW")), result.report().warnings().toString());

        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains(
                "driver.element().click(SHAFT.GUI.Locator.hasRole(Role.BUTTON)"
                        + ".hasNormalizedText(\"Close cookie banner\").build());"));
        assertFalse(source.contains(
                "if (driver.element().getElementsCount(SHAFT.GUI.Locator.hasRole(Role.BUTTON)"
                        + ".hasNormalizedText(\"Close cookie banner\").build()) > 0)"));
    }

    @Test
    void approvedControlFlowPreviewGeneratesOptionalGuard() throws Exception {
        Path preview = temp.resolve("approved-control-flow-preview.json");
        Path session = session(controlFlowSession());
        writeCaptureData("alice");
        new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("control-flow-approval"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll(), false,
                CaptureGenerationRequest.ControlFlowMode.PREVIEW, preview));

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("control-flow-applied"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll(), false,
                CaptureGenerationRequest.ControlFlowMode.APPLY, preview));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains(
                "if (driver.element().getElementsCount(SHAFT.GUI.Locator.hasRole(Role.BUTTON)"
                        + ".hasNormalizedText(\"Close cookie banner\").build()) > 0)"));
        assertFalse(source.contains("private boolean isCaptureElementDisplayed(By locator)"));
        assertTrue(result.report().controlFlowSuggestions().stream()
                .anyMatch(suggestion -> suggestion.kind() == CaptureGenerationReport.ControlFlowKind.OPTIONAL_GUARD
                        && suggestion.applied()));
    }

    @Test
    void deterministicReviewFlagsGeneratedCodeRisks() throws Exception {
        ExternalTestDataReference card = new ExternalTestDataReference(
                "data.card",
                "card number",
                ExternalTestDataReference.DataSource.JSON,
                "capture-data.json",
                "/values/data.card",
                ExternalTestDataReference.DataClassification.SENSITIVE);
        ElementSnapshot cardInput = new ElementSnapshot(
                "card-input",
                "input",
                "textbox",
                "Card number",
                "Card number",
                Map.of("name", "card"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.XPATH,
                        "//div[3]/form/input[1]", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.POSITIONAL),
                        "//div[3]/form/input[1]")),
                true,
                true,
                false);
        ElementSnapshot payButton = new ElementSnapshot(
                "pay-button",
                "button",
                "button",
                "Pay now",
                "",
                Map.of("type", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.XPATH,
                        "//div[3]/form/button[2]", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.POSITIONAL),
                        "//div[3]/form/button[2]")),
                true,
                true,
                false);
        CaptureSession reviewSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "review-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://shop.example/checkout"),
                        new CaptureEvent.TypeEvent(CaptureFixtures.context(2), cardInput, card),
                        new CaptureEvent.WaitEvent(CaptureFixtures.context(3),
                                CaptureEvent.WaitCondition.FIXED_DURATION, Duration.ofSeconds(2), null, null),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(4), payButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(card),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(reviewSession);
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put("data.card", "test-card-value");
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("review")));

        assertGeneratedUnconfirmed(result);
        var review = JSON.readTree(result.reviewPath().toFile());
        List<String> categories = new java.util.ArrayList<>();
        review.path("findings").forEach(finding -> categories.add(finding.path("category").asText()));
        assertTrue(categories.contains("LOCATOR"), review.toString());
        assertTrue(categories.contains("ASSERTION"), review.toString());
        assertTrue(categories.contains("WAIT"), review.toString());
        assertTrue(categories.contains("TEST_DATA"), review.toString());
        assertTrue(result.report().warnings().stream().anyMatch(warning -> warning.contains("review/LOCATOR")));
        assertEquals(CaptureReadiness.State.RISKY, result.report().readiness());
        assertTrue(result.report().readinessWarnings().stream()
                .anyMatch(warning -> warning.contains("positional XPATH")));
    }

    @Test
    void deterministicReviewMapsReplayFailureTraceAndNetworkDependency() throws Exception {
        Path session = session(new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "trace-review-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://shop.example/checkout"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of()));
        GeneratedTestValidator validator = new GeneratedTestValidator() {
            @Override
            public CaptureGenerationReport.Validation compile(Path source, Path classesDirectory) {
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                        List.of(),
                        0);
            }

            @Override
            public CaptureGenerationReport.Validation replay(
                    String fullyQualifiedClassName,
                    Path classesDirectory,
                    Path resourcesDirectory,
                    Path workDirectory,
                    Duration timeout) {
                Path trace = workDirectory.resolve("target/shaft-traces/trace-review/shaft-trace.json");
                try {
                    Files.createDirectories(trace.getParent());
                    Files.writeString(trace, """
                            {
                              "schemaVersion": "1.0",
                              "source": {
                                "file": "src/test/java/generated/capture/TraceReviewTest.java",
                                "line": "31",
                                "snippet": "driver.element().click(USERNAME_INPUT_LOCATOR);"
                              },
                              "actions": [
                                {"id": "action-2", "category": "element", "name": "CLICK", "status": "failed",
                                 "locator": "By.xpath: /html/body/div[3]/form/button[2]",
                                 "url": "https://shop.example/checkout", "message": "Click failed",
                                 "exception": {"type": "org.openqa.selenium.NoSuchElementException", "message": "missing"},
                                 "attachments": [], "metadata": {}}
                              ],
                              "network": [
                                {"url": "https://shop.example/api/pay", "status": 500, "method": "POST"}
                              ]
                            }
                            """, StandardCharsets.UTF_8);
                } catch (java.io.IOException exception) {
                    throw new IllegalStateException(exception);
                }
                return new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("Replay produced 1 non-passing Allure result file(s)."),
                        1);
            }
        };

        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), validator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session, temp.resolve("trace-review"), "generated.capture", "TraceReviewTest", false,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        var review = JSON.readTree(result.reviewPath().toFile());
        List<String> categories = new java.util.ArrayList<>();
        review.path("findings").forEach(finding -> categories.add(finding.path("category").asText()));
        assertTrue(categories.contains("REPLAY_TRACE"), review.toString());
        assertTrue(categories.contains("NETWORK_DEPENDENCY"), review.toString());
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("trace action action-2")), result.report().warnings().toString());
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("#3065")), result.report().warnings().toString());
    }

    // Issue #4166: a failed compile/replay must never destroy a previously-good generated test
    // file already sitting at the same output path -- only a validated regeneration may be
    // promoted to paths.source().
    @Test
    void failedReplayLeavesPreviouslyGeneratedSourceUnchanged() throws Exception {
        CaptureSession baseline = CaptureFixtures.representativeSession();
        writeCaptureData("alice");
        Path output = temp.resolve("overwrite-protection");

        CaptureGenerationResult first = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session(baseline), output, "generated.capture", "OverwriteProtectionTest", true,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll()));
        assertGeneratedUnconfirmed(first);
        String originalSource = Files.readString(first.sourcePath());

        // Same events (so analysis/compile would still be valid) but a different sessionGoal, so
        // the deterministically-rendered source is provably different text from the original --
        // otherwise a byte-identical regeneration could pass even with the bug still present.
        CaptureSession regenerated = new CaptureSession(
                baseline.schemaVersion(), baseline.sessionId(), baseline.status(), baseline.startedAt(),
                baseline.endedAt(), baseline.browser(), baseline.events(), baseline.checkpoints(),
                baseline.dataReferences(), baseline.redactionSummary(),
                Map.of("sessionGoal", JSON.getNodeFactory().textNode("Regenerated after a forced replay failure")));

        GeneratedTestValidator failingReplayValidator = new GeneratedTestValidator() {
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
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("Simulated replay failure for regression coverage."), 1);
            }
        };
        CaptureGenerationResult second = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), failingReplayValidator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session(regenerated), output, "generated.capture", "OverwriteProtectionTest", true,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(second.successful());
        assertEquals(first.sourcePath(), second.sourcePath());
        assertEquals(originalSource, Files.readString(second.sourcePath()));
        assertTrue(second.report().warnings().stream()
                        .anyMatch(warning -> warning.contains("left unchanged")),
                second.report().warnings().toString());
    }

    // Issue #4202: the same overwrite-before-validate exposure #4166/PR #4201 fixed for the
    // generated .java source is also present for the generated test-data JSON -- a failed
    // compile/replay must never leave unvalidated data at paths.data() either. Staging under a
    // temp name (as #4166 did for source) is not viable here: the replay subprocess loads test
    // data from paths.data() by the fixed SHAFT.TestData.JSON(dataFileName(className)) convention
    // baked into the generated test itself, so the fix is snapshot/restore instead.
    @Test
    void failedReplayLeavesPreviouslyGeneratedDataUnchanged() throws Exception {
        CaptureSession baseline = CaptureFixtures.representativeSession();
        writeCaptureData("alice");
        Path output = temp.resolve("data-overwrite-protection");

        CaptureGenerationResult first = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session(baseline), output, "generated.capture", "DataOverwriteProtectionTest", true,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll()));
        assertGeneratedUnconfirmed(first);
        String originalDataJson = Files.readString(first.testDataPath());

        // Same session (so analysis/compile would still be valid), but the referenced
        // capture-data.json now has a different username value, so the regenerated dataJson is
        // provably different text from the original -- otherwise a byte-identical regeneration
        // could pass even with the bug still present.
        writeCaptureData("bob");

        GeneratedTestValidator failingReplayValidator = new GeneratedTestValidator() {
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
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("Simulated replay failure for regression coverage."), 1);
            }
        };
        CaptureGenerationResult second = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), failingReplayValidator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session(baseline), output, "generated.capture", "DataOverwriteProtectionTest", true,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(second.successful());
        assertEquals(first.testDataPath(), second.testDataPath());
        assertEquals(originalDataJson, Files.readString(second.testDataPath()));
        assertTrue(second.report().warnings().stream()
                        .anyMatch(warning -> warning.contains("test data") && warning.contains("left unchanged")),
                second.report().warnings().toString());
    }

    // Issue #4202: a FIRST generation that fails compile/replay has no previous data file to
    // restore, so the just-written, unvalidated data file must be removed instead of left in
    // place.
    @Test
    void failedReplayOnFirstGenerationRemovesUnvalidatedDataFile() throws Exception {
        CaptureSession baseline = CaptureFixtures.representativeSession();
        writeCaptureData("alice");
        Path output = temp.resolve("data-first-generation-failure");

        GeneratedTestValidator failingReplayValidator = new GeneratedTestValidator() {
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
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("Simulated replay failure for regression coverage."), 1);
            }
        };
        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), failingReplayValidator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session(baseline), output, "generated.capture", "FirstGenerationFailureTest", true,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        assertFalse(Files.exists(result.testDataPath()),
                "Unvalidated first-generation data file should have been removed.");
        assertTrue(result.report().warnings().stream()
                        .anyMatch(warning -> warning.contains("test data") && warning.contains("removed")),
                result.report().warnings().toString());
    }

    // Issue #4206: deleteStagedSourceQuietly(stagedSource) previously ran only on the normal
    // success/failure control-flow path after compile/replay, never in the outer
    // catch (RuntimeException) block -- so a RuntimeException escaping validator.compile()/
    // validator.replay() (e.g. an unchecked JacksonException from a malformed/partially-flushed
    // Allure result file, as from a killed replay JVM) leaked the staged .java source file under
    // target/shaft-capture/staging/.
    @Test
    void replayThrowingRuntimeExceptionStillDeletesStagedSourceFile() throws Exception {
        CaptureSession baseline = CaptureFixtures.representativeSession();
        writeCaptureData("alice");
        Path output = temp.resolve("staged-source-leak");

        GeneratedTestValidator throwingReplayValidator = new GeneratedTestValidator() {
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
                throw new IllegalStateException("Simulated malformed Allure-results parse failure.");
            }
        };
        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), throwingReplayValidator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session(baseline), output, "generated.capture", "StagedSourceLeakTest", true,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        Path stagedSource = output.toAbsolutePath().normalize()
                .resolve("target/shaft-capture/staging")
                .resolve("generated/capture")
                .resolve("StagedSourceLeakTest.java")
                .normalize();
        assertFalse(Files.exists(stagedSource),
                "staged source file should have been cleaned up after a RuntimeException from replay()");
    }

    // Issue #4217: dataWritePendingValidation must be reset as soon as compile+replay determine
    // `successful`, not after the later, unrelated atomicWrite(paths.source(), source) promotion
    // write -- otherwise an I/O failure on that unrelated write (disk full, permission race) AFTER
    // a fully successful validation reaches the outer catch block with the flag still true, which
    // wrongly restores/removes the just-validated, correct data file as if compile/replay had
    // failed.
    @Test
    void promotionWriteFailureAfterSuccessfulValidationDoesNotDiscardValidatedDataFile() throws Exception {
        CaptureSession baseline = CaptureFixtures.representativeSession();
        writeCaptureData("alice");
        Path output = temp.resolve("promotion-write-failure");

        // Block the directory the validated source would be promoted into, so the atomicWrite()
        // call AFTER a successful compile+replay throws, while compile/replay themselves report
        // PASSED -- isolating the promotion-write failure from an actual validation failure.
        Files.createDirectories(output.resolve("src/test/java"));
        Files.writeString(output.resolve("src/test/java/generated"), "not a directory");

        GeneratedTestValidator passingValidator = new GeneratedTestValidator() {
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
                        CaptureGenerationReport.Validation.ValidationStatus.PASSED, List.of(), 0);
            }
        };
        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), passingValidator, new CaptureEnrichmentService())
                .generate(new CaptureGenerationRequest(
                        session(baseline), output, "generated.capture", "PromotionWriteFailureTest", true,
                        true, true, Duration.ofMinutes(1),
                        CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                        ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        assertTrue(Files.isRegularFile(result.testDataPath()),
                "a validated data file must not be discarded by an unrelated later promotion-write failure");
        assertTrue(Files.readString(result.testDataPath()).contains("\"username\" : \"alice\""),
                Files.readString(result.testDataPath()));
    }

    @Test
    void workspaceContainedFileUrlRecordingIsNotAPrivacyBlocker() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        Path output = temp.resolve("workspace-file-url");
        String fixtureUrl = output.resolve("fixtures/login.html").toUri().toString();
        CaptureSession recorded = new CaptureSession(
                base.schemaVersion(),
                "workspace-file-url-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        fixtureUrl)),
                List.of(),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(recorded);

        CaptureGenerationResult result = new CaptureGenerator().generate(request(session, output));

        assertTrue(result.report().unsupportedEvents().stream()
                        .noneMatch(message -> message.startsWith("privacy:")),
                result.report().unsupportedEvents().toString());
        assertGeneratedUnconfirmed(result);
    }

    @Test
    void projectContainedFileUrlOutsideTheOutputDirectoryIsNotAPrivacyBlocker() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        Path project = temp.resolve("consumer-project");
        Path recordings = Files.createDirectories(project.resolve("recordings"));
        Path output = project.resolve("generated-tests");
        // The recorded page lives in the project but NOT under the generation output directory —
        // the standard layout when a first-time user records a local fixture page.
        String fixtureUrl = project.resolve("src/test/resources/pages/login.html").toUri().toString();
        CaptureSession recorded = new CaptureSession(
                base.schemaVersion(),
                "project-file-url-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        fixtureUrl)),
                List.of(),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = recordings.resolve("project-file-url.json");
        new CaptureJsonCodec().write(session, recorded);

        CaptureGenerationResult result = new CaptureGenerator().generate(request(session, output));

        assertTrue(result.report().unsupportedEvents().stream()
                        .noneMatch(message -> message.startsWith("privacy:")),
                result.report().unsupportedEvents().toString());
        assertGeneratedUnconfirmed(result);
    }

    @Test
    void privacyAllowedRootNeverWidensToTheUserHomeOrAbove() {
        Path home = Path.of(System.getProperty("user.home")).toAbsolutePath().normalize();
        Path session = home.resolve("project-a/recordings/session.json");
        Path output = home.resolve("project-b/generated-tests").toAbsolutePath().normalize();

        assertEquals(output, CaptureGenerator.privacyAllowedRoot(session, output));
        assertEquals(home.resolve("project-a"),
                CaptureGenerator.privacyAllowedRoot(
                        home.resolve("project-a/recordings/session.json"),
                        home.resolve("project-a/generated-tests")));
    }

    @Test
    void personalPathsOutsideTheWorkspaceStillBlockGeneration() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        CaptureSession recorded = new CaptureSession(
                base.schemaVersion(),
                "external-personal-path-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(3),
                base.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(
                                CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN,
                                "file:///C:/Users/stranger/private/page.html"),
                        new CaptureEvent.NavigationEvent(
                                CaptureFixtures.context(2),
                                CaptureEvent.NavigationAction.OPEN,
                                "file:///home/stranger/private/page.html")),
                List.of(),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path session = session(recorded);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("external-personal-path")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                        .anyMatch(message -> message.contains("personal Windows path")),
                result.report().unsupportedEvents().toString());
        assertTrue(result.report().unsupportedEvents().stream()
                        .anyMatch(message -> message.contains("personal POSIX path")),
                result.report().unsupportedEvents().toString());
    }

    @Test
    void failedAttemptLeftoversDoNotBlockRetryWithoutOverwrite() throws Exception {
        CaptureSession base = CaptureFixtures.representativeSession();
        Path output = temp.resolve("retry");
        CaptureSession failing = new CaptureSession(
                base.schemaVersion(),
                "retry-failing-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        "file:///C:/Users/stranger/private/page.html")),
                List.of(),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path failingSession = temp.resolve("retry-failing.json");
        new CaptureJsonCodec().write(failingSession, failing);
        CaptureGenerationResult failed =
                new CaptureGenerator().generate(request(failingSession, output));
        assertFalse(failed.successful());
        assertTrue(Files.isRegularFile(failed.reportPath()), "Failed attempt should write its report");

        CaptureSession healthy = new CaptureSession(
                base.schemaVersion(),
                "retry-healthy-session",
                CaptureSession.SessionStatus.COMPLETED,
                base.startedAt(),
                CaptureFixtures.STARTED.plusSeconds(2),
                base.browser(),
                List.of(new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        "https://example.test/form")),
                List.of(),
                List.of(),
                base.redactionSummary(),
                base.extensions());
        Path healthySession = temp.resolve("retry-healthy.json");
        new CaptureJsonCodec().write(healthySession, healthy);
        CaptureGenerationResult retried =
                new CaptureGenerator().generate(request(healthySession, output));

        assertGeneratedUnconfirmed(retried);
        assertEquals("UNCONFIRMED", JSON.readTree(retried.reportPath().toFile()).path("status").asText(),
                "Retry must refresh the status report");
    }

    @Test
    void secretCanaryInExternalDataIsRejectedWithoutLeakingIntoReport() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        String canary = "sk-secret-canary-123456789";
        writeCaptureData(canary);

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("secret")));

        assertFalse(result.successful());
        String report = Files.readString(result.reportPath());
        assertFalse(report.contains(canary));
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.startsWith("privacy:")));
    }

    @Test
    void approvedEnrichmentIsAppliedThenRecompiled() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");
        ObjectNode payload = JSON.createObjectNode();
        payload.put("className", "EnrichedJourneyTest");
        payload.put("methodName", "completeCheckout");
        payload.putObject("elementNames").put("username-input", "USERNAME_FIELD");
        payload.putArray("assertions").addObject()
                .put("eventSequence", 2)
                .put("verification", "ELEMENT_VISIBLE")
                .put("negated", false);
        CaptureEnrichmentService enrichment = new CaptureEnrichmentService(request ->
                AiResponse.success("mock", "mock-model", payload, Duration.ZERO,
                        AiUsage.empty(), request.deterministicFallback()));
        CaptureGenerator generator = new CaptureGenerator(
                new CaptureJsonCodec(), new LocatorRanker(), new GeneratedTestValidator(), enrichment);
        Path preview = temp.resolve("preview.json");

        CaptureGenerationResult previewResult = generator.generate(new CaptureGenerationRequest(
                session, temp.resolve("preview-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.PREVIEW, preview, false,
                new ApprovalPolicy(true, true, java.util.Set.of(com.shaft.pilot.ai.EvidenceCategory.TEXT))));
        assertGeneratedUnconfirmed(previewResult);
        assertTrue(Files.readString(preview).contains("EnrichedJourneyTest"));

        CaptureGenerationResult applied = generator.generate(new CaptureGenerationRequest(
                session, temp.resolve("applied-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.APPLY, preview, true,
                ApprovalPolicy.denyAll()));

        assertGeneratedUnconfirmed(applied);
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.APPLIED,
                applied.report().enrichment().status());
        String source = Files.readString(applied.sourcePath());
        assertTrue(source.contains("public class EnrichedJourneyTest"));
        assertTrue(source.contains("public void completeCheckout()"));
        assertFalse(source.contains("private static final By USERNAME_FIELD"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).build()).isVisible();"));
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                applied.report().compilation().status());
    }

    @Test
    void staleOrInvalidEnrichmentPreviewIsRejected() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");
        Path preview = temp.resolve("invalid-preview.json");
        Files.writeString(preview, """
                {
                  "schemaVersion": "1.0",
                  "deterministicFingerprint": "stale",
                  "provider": "mock",
                  "proposal": {
                    "className": "not-valid!",
                    "methodName": "",
                    "elementNames": {},
                    "assertions": []
                  },
                  "diff": []
                }
                """, StandardCharsets.UTF_8);

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("invalid-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.APPLY, preview, true,
                ApprovalPolicy.denyAll()));

        assertFalse(result.successful());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.REJECTED,
                result.report().enrichment().status());
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("does not match")));
        assertTrue(result.report().unsupportedEvents().stream()
                .anyMatch(message -> message.contains("not a Java identifier")));
    }

    @Test
    void invalidProviderProposalIsRejectedBeforePreviewPersistence() {
        ObjectNode payload = JSON.createObjectNode();
        payload.put("className", "not-valid!");
        payload.put("methodName", "");
        payload.putObject("elementNames");
        payload.putArray("assertions");
        CaptureEnrichmentService enrichment = new CaptureEnrichmentService(request ->
                AiResponse.success("mock", "mock-model", payload, Duration.ZERO,
                        AiUsage.empty(), request.deterministicFallback()));

        assertThrows(IllegalStateException.class, () -> enrichment.preview(
                CaptureFixtures.representativeSession(),
                "1234567890abcdef",
                "Session1Test",
                "replaySession1",
                Map.of("username-input", "USERNAME_INPUT_LOCATOR"),
                new ApprovalPolicy(true, true,
                        java.util.Set.of(com.shaft.pilot.ai.EvidenceCategory.TEXT))));
    }

    /**
     * Issue #4029: {@code CaptureGenerator} must never report {@code successful=true} when replay
     * was never requested -- a compiled-but-unreplayed generation is a distinct, honest
     * {@code UNCONFIRMED} status, not folded into bare {@code SUCCESS}.
     */
    @Test
    void generateWithSkippedReplayIsNotReportedSuccessful() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        CaptureGenerationResult result =
                new CaptureGenerator().generate(request(session, temp.resolve("unconfirmed")));

        assertFalse(result.successful(), "a skipped replay must never report successful=true");
        assertEquals(CaptureGenerationReport.Status.UNCONFIRMED, result.report().status());
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                result.report().compilation().status());
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.SKIPPED,
                result.report().replay().status());
        assertTrue(Files.isRegularFile(result.sourcePath()),
                "compiled-but-unconfirmed generation must still write the generated source");
    }

    private static void assertGeneratedUnconfirmed(CaptureGenerationResult result) {
        assertEquals(CaptureGenerationReport.Status.UNCONFIRMED, result.report().status(),
                result.report().unsupportedEvents().toString());
    }

    private CaptureGenerationRequest request(Path session, Path output) {
        return new CaptureGenerationRequest(
                session, output, "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll());
    }

    private Path session(CaptureSession captureSession) {
        Path path = temp.resolve("capture.json");
        new CaptureJsonCodec().write(path, captureSession);
        return path;
    }

    private static String normalizeLineEndings(String value) {
        return value.replace("\r\n", "\n").replace('\r', '\n');
    }

    /**
     * Redacts the ONE line of generated source that is legitimately output-directory-dependent
     * (issue #4172's absolute {@code healing.history.path}), so byte-for-byte comparisons across
     * different output directories -- or against a static golden fixture -- still hold for every
     * other line, which remains a pure function of the captured session.
     */
    private static String withStableHealingHistoryPath(String source) {
        return source.replaceAll(
                "(?m)^(\\s*\\.historyPath\\(\")[^\"]*(\"\\)\\r?)$",
                "$1PLACEHOLDER$2");
    }

    private static long count(String value, String needle) {
        return value.split(java.util.regex.Pattern.quote(needle), -1).length - 1L;
    }

    private void writeCaptureData(String username) throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put("data.username", username);
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);
    }

    private static CaptureSession controlFlowSession() {
        ElementSnapshot close = new ElementSnapshot(
                "cookie-close-button",
                "button",
                "button",
                "Close cookie banner",
                "Close",
                Map.of("aria-label", "Close cookie banner", "class", "cookie-banner-close"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:Close cookie banner", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true)),
                true,
                true,
                false);
        ElementSnapshot submit = new ElementSnapshot(
                "submit-button",
                "button",
                "button",
                "Submit",
                "",
                Map.of("type", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:Submit", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true)),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "control-flow-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(8),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), close,
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(3), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.TypeEvent(CaptureFixtures.context(4), CaptureFixtures.target(),
                                CaptureFixtures.ordinary()),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(5), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.TypeEvent(CaptureFixtures.context(6), CaptureFixtures.target(),
                                CaptureFixtures.ordinary()),
                        new CaptureEvent.ClickEvent(context(7, EventContext.ReplayStatus.FAILED), submit,
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(8), close,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(CaptureFixtures.ordinary()),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    private static com.shaft.capture.model.EventContext context(
            long sequence,
            com.shaft.capture.model.EventContext.ReplayStatus status) {
        return new com.shaft.capture.model.EventContext(
                sequence,
                CaptureFixtures.STARTED.plusSeconds(sequence),
                CaptureFixtures.page(),
                status,
                List.of(),
                Map.of());
    }

    /**
     * Issue #4029: a recording broken deep enough to trip an unexpected {@link RuntimeException}
     * (here, a malformed capture file that fails schema validation on its THIRD event) must not
     * collapse to a generic, stage-less "Generation failed." string -- the failure must name which
     * pipeline stage was reached.
     */
    @Test
    void malformedRecordingReportsWhichPipelineStageFailed() throws Exception {
        CaptureSession valid = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "broken-recording-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(3), CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        String json = new CaptureJsonCodec().write(valid);
        var tree = JSON.readTree(json);
        // Strip the required "context" object from the THIRD recorded event (index 2) -- schema
        // validation names it "$.events[2].context is required.", so the corruption is scoped to
        // one specific recorded step rather than the whole file.
        ((tools.jackson.databind.node.ObjectNode) tree.path("events").get(2)).remove("context");
        Path brokenSession = temp.resolve("broken-recording.json");
        Files.writeString(brokenSession,
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(tree), StandardCharsets.UTF_8);

        CaptureGenerationResult result = new CaptureGenerator().generate(request(brokenSession, temp.resolve("out")));

        assertFalse(result.successful());
        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
        assertTrue(result.report().unsupportedEvents().stream()
                        .anyMatch(message -> message.contains("reading the capture session")
                                && message.contains("events[2]")),
                result.report().unsupportedEvents().toString());
    }

    /**
     * Issue #4029: when a recorded step's element evidence is corrupt enough to trip an unexpected
     * exception during locator analysis, the failure must identify WHICH recorded step (event)
     * broke -- not just re-surface a bare, step-less exception message.
     */
    @Test
    void brokenLocatorEvidenceReportsWhichRecordedStepFailed() throws Exception {
        ElementSnapshot workingTarget = CaptureFixtures.target();
        ElementSnapshot corruptTarget = new ElementSnapshot(
                "checkout-button",
                "button",
                "button",
                "Checkout",
                "Checkout",
                Map.of(),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "button.checkout", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE))),
                true,
                true,
                false);
        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "broken-locator-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), workingTarget,
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(3), corruptTarget,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path sessionPath = session(session);
        LocatorRanker realRanker = new LocatorRanker();
        LocatorRanker ranker = org.mockito.Mockito.mock(LocatorRanker.class);
        org.mockito.Mockito.when(ranker.select(
                        org.mockito.ArgumentMatchers.any(),
                        org.mockito.ArgumentMatchers.any(),
                        org.mockito.ArgumentMatchers.anyBoolean()))
                .thenAnswer(invocation -> {
                    ElementSnapshot target = invocation.getArgument(0);
                    if ("checkout-button".equals(target.logicalElementId())) {
                        throw new IllegalStateException("simulated corrupted locator evidence");
                    }
                    return realRanker.select(
                            invocation.getArgument(0), invocation.getArgument(1), invocation.getArgument(2));
                });

        CaptureGenerationResult result = new CaptureGenerator(
                new CaptureJsonCodec(), ranker, new GeneratedTestValidator(), new CaptureEnrichmentService())
                .generate(request(sessionPath, temp.resolve("out")));

        assertFalse(result.successful());
        assertTrue(result.report().unsupportedEvents().stream()
                        .anyMatch(message -> message.contains("event-3") && message.contains("checkout-button")),
                result.report().unsupportedEvents().toString());
        assertTrue(result.report().unsupportedEvents().stream()
                        .noneMatch(message -> message.contains("event-2")),
                result.report().unsupportedEvents().toString());
    }

    /**
     * Issue #4239 P1.4-decision ladder, rung 1 refusal: {@code inferredRole()} prefers an explicit
     * {@code role="button"} attribute over tag shape, so a {@code <div role="button">} produces a
     * ROLE candidate whose {@code uniquenessCount} (re-derived via {@code inferredRole}) truthfully
     * reports 1 -- but {@code SHAFT.GUI.Locator.hasRole(Role.BUTTON)}'s fixed XPath union
     * ({@code //button | //input[@type='button'] | ...}) never inspects the {@code role} attribute
     * and matches ZERO {@code <div>} elements. {@code roleXpathVerified=false} is the recorder's
     * signal that this mismatch was caught, so rung 1 must be refused and generation must fall
     * through to the self-verified {@code replayXpath} instead of emitting the broken
     * {@code hasRole(...)}.
     *
     * <p>Issue #4271 reversed one clause of this case. It previously also asserted that the unique
     * stable ID candidate must never be used; under the unified policy a unique stable id is tier 1
     * and therefore outranks the tier-3 XPath. The rung-1 refusal itself is unchanged and still
     * asserted here, and the XPath-fallthrough behaviour it originally covered is preserved verbatim
     * by {@link #customRoleAttributeDivWithNoIdStillFallsThroughToTheSelfVerifiedXpath()}.
     */
    @Test
    void customRoleAttributeDivIsRefusedForRungOneAndSelectsTheUniqueStableId() throws Exception {
        Path session = session(customRoleButtonSession(true));
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("custom-role-button")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasAnyTagName().hasId(\"custom-button\").build()"),
                "a unique stable id is tier 1 and outranks the tier-3 self-verified XPath: " + source);
        assertFalse(source.contains("hasRole(Role.BUTTON)"),
                "must never emit hasRole(...) for a ROLE candidate that failed self-verification: " + source);
        assertFalse(source.contains("SHAFT.GUI.Locator.id(\"custom-button\")"),
                "must never emit the guardrail-banned literal id(...) form: " + source);
    }

    /**
     * The rung-1 refusal / XPath-fallthrough behaviour of the case above, with the competing id
     * removed so the self-verified XPath is the best remaining evidence.
     */
    @Test
    void customRoleAttributeDivWithNoIdStillFallsThroughToTheSelfVerifiedXpath() throws Exception {
        Path session = session(customRoleButtonSession(false));
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("custom-role-button-no-id")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("By.xpath(\"//div[normalize-space(.)=\\\"Custom Button\\\"]\")"),
                "a role=\"button\" div whose hasRole(...) union does not verify must fall through to "
                        + "the self-verified recorded XPath: " + source);
        assertFalse(source.contains("hasRole(Role.BUTTON)"), source);
    }

    private static CaptureSession customRoleButtonSession(boolean withUniqueId) {
        List<LocatorCandidate> candidates = new java.util.ArrayList<>(List.of(
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:Custom Button", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                        "//div[normalize-space(.)=\"Custom Button\"]", false)));
        if (withUniqueId) {
            candidates.add(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID,
                    "custom-button", 1, true, true,
                    java.util.Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE)));
        }
        ElementSnapshot customButton = new ElementSnapshot(
                "custom-button",
                "div",
                "button",
                "Custom Button",
                "",
                Map.of("role", "button"),
                candidates,
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "custom-role-button-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), customButton,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
    }

    /**
     * Issue #4271 reversed this case. A role-less {@code <div>} with no accessible name has no ROLE
     * candidate and no self-verified {@code replayXpath} -- only an ID candidate. Under the old hard
     * ban that meant generation FAILED outright, which is precisely the behaviour the owner policy
     * "keep unique ids first" replaces: a <em>unique, stable</em> id is now tier 1 and generates.
     *
     * <p>What must still fail is an id carrying no trustworthy evidence, so this case now pins the
     * boundary: the same element with a <em>non-unique</em> id (three matches on the page) is still
     * refused with an actionable message rather than silently emitting an ambiguous locator.
     */
    @Test
    void roleLessUnlabeledDivWithANonUniqueIdStillFailsGenerationInsteadOfFallingBackToIt()
            throws Exception {
        ElementSnapshot plainDiv = new ElementSnapshot(
                "plain-div",
                "div",
                "",
                "",
                "",
                Map.of("id", "x"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID,
                        "x", 3, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE))),
                true,
                true,
                false);
        CaptureSession captureSession = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "role-less-div-session",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(3),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(CaptureFixtures.context(1),
                                CaptureEvent.NavigationAction.OPEN, "https://example.test/form"),
                        new CaptureEvent.ClickEvent(CaptureFixtures.context(2), plainDiv,
                                CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                com.shaft.capture.model.RedactionSummary.empty(),
                Map.of());
        Path session = session(captureSession);

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("role-less-div")));

        assertFalse(result.successful());
        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
        assertTrue(result.report().unsupportedEvents().stream()
                        .anyMatch(message -> message.contains("plain-div")),
                result.report().unsupportedEvents().toString());
    }

    /**
     * Issue #4239 P1.3: {@code GeneratedCodeGuardrails} (the P1.2 extraction) is wired into
     * {@code CaptureGenerator.generate}'s existing {@code state.unsupported()} gate as
     * belt-and-braces. The P1.4-decision ladder makes locator-strategy guardrail violations
     * unreachable by construction (rung 3 refuses generation before any {@code .id/.name/.cssSelector}
     * could ever be rendered), so this is a regression-catcher for the OTHER rules the shared
     * guardrail library also enforces (e.g. {@code Thread.sleep}), tested here directly against the
     * wiring helper rather than through a full {@code generate()} call that no legitimate input can
     * reach a violation through.
     */
    @Test
    void guardrailUnsupportedFindingsFlagsErrorSeverityViolationsOnly() {
        List<String> errorFindings = CaptureGenerator.guardrailUnsupportedFindings("""
                class GeneratedTest {
                    void step() throws InterruptedException {
                        Thread.sleep(1000);
                    }
                }
                """);
        assertFalse(errorFindings.isEmpty(), errorFindings.toString());
        assertTrue(errorFindings.stream().anyMatch(message -> message.contains("THREAD_SLEEP")), errorFindings.toString());

        List<String> warningOnlyFindings = CaptureGenerator.guardrailUnsupportedFindings("""
                class GeneratedTest {
                    void step(org.openqa.selenium.WebDriver driver) {
                        driver.manage().timeouts().implicitlyWait(java.time.Duration.ofSeconds(5));
                    }
                }
                """);
        assertTrue(warningOnlyFindings.isEmpty(), warningOnlyFindings.toString());
    }

    /**
     * Issue #4239 P1.5 (F12): once a target degrades to rung 2 (self-verified XPath, no verified
     * ARIA role), the generated line must disclose that -- "no verified ARIA role, using recorded
     * XPath" -- since the F12 finding was that this information existed only in
     * {@code report.flakySteps()}, never where a human reading the generated code would see it.
     * Triggers on the XPath-fallback tier itself (a ROLE candidate that failed self-verification,
     * per the {@link #customRoleAttributeDivWithNoIdStillFallsThroughToTheSelfVerifiedXpath()}
     * fixture), not on a role+index composition -- that fallback no longer exists in this design.
     */
    @Test
    void xpathFallbackSelectionEmitsLowTrustMarkerCommentDisclosingNoVerifiedAriaRole() throws Exception {
        Path session = session(customRoleButtonSession(false));
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("rung-two-marker")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("// SHAFT: no verified ARIA role, using recorded XPath"), source);
    }

    /**
     * Issue #4271: the low-trust marker discloses a fall back to a raw recorded XPath. A tier-1
     * unique stable id is the most-preferred evidence there is, not a degradation, and it does not
     * use a recorded XPath at all -- so labelling it "using recorded XPath" would be plainly false
     * and would turn the marker into noise on ordinary generated lines.
     */
    @Test
    void uniqueIdSelectionEmitsNoLowTrustMarker() throws Exception {
        Path session = session(uniqueIdLocatorSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("unique-id-marker")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertFalse(source.contains("// SHAFT: no verified ARIA role, using recorded XPath"), source);
    }

    /**
     * Issue #4239 P1.5: a rung-1 (verified ARIA role) selection with a healthy score must NOT carry
     * the rung-2 disclosure -- the marker is signal, not noise on every generated line.
     */
    @Test
    void rungOneSelectionWithHealthyScoreEmitsNoLowTrustMarker() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator()
                .generate(request(session, temp.resolve("rung-one-no-marker")));

        assertGeneratedUnconfirmed(result);
        String source = Files.readString(result.sourcePath());
        assertFalse(source.contains("// SHAFT: no verified ARIA role"), source);
    }
}
