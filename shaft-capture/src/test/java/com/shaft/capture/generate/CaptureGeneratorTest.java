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

        assertTrue(first.successful(), first.report().unsupportedEvents().toString());
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
        assertEquals(Files.readString(first.sourcePath()), Files.readString(second.sourcePath()));
        assertEquals(Files.readString(first.testDataPath()), Files.readString(second.testDataPath()));
        assertEquals(first.report(), second.report());

        String source = Files.readString(first.sourcePath());
        String data = Files.readString(first.testDataPath());
        String golden = Files.readString(Path.of(
                "src/test/resources/fixtures/golden-generated-session-1.java"));
        assertEquals(normalizeLineEndings(golden), normalizeLineEndings(source));
        assertTrue(source.contains("@AfterMethod(alwaysRun = true)"));
        assertTrue(source.contains("driver.quit();"));
        assertTrue(source.contains("SHAFT.GUI.Locator.inputField(\"Username\")"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.inputField(\"Username\")).text()"));
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
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "button[type='submit']", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE))),
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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
        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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
        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("        loginAsAdmin();"));
        assertTrue(source.contains("    private void loginAsAdmin() throws Exception {"));
        assertEquals(1, count(source, "driver.element().type(SHAFT.GUI.Locator.inputField(\"Username\")"));
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                result.report().compilation().status());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("private SHAFT.GUI.Playwright driver;"));
        assertTrue(source.contains("driver = new SHAFT.GUI.Playwright();"));
        assertTrue(source.contains("driver.element().click(SHAFT.GUI.Locator.inputField(\"Username\"));"));
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("driver.browser().assertThat().title().contains(requiredData(\"username\"));"));
        assertTrue(source.contains("driver.browser().assertThat().text().contains(requiredData(\"username\"));"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.inputField(\"Username\")).matchesReferenceImage();"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.inputField(\"Username\")).doesNotMatchReferenceImage();"));
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains(
                "driver.element().assertThat(SHAFT.GUI.Locator.inputField(\"Username\")).attribute(\"autocomplete\")"
                        + ".isEqualTo(requiredData(\"username\"));"));
    }

    @Test
    void fallbackLocatorReplayOptionEmitsCompactHelperOnlyWhenFallbacksExist() throws Exception {
        Path session = session(CaptureFixtures.representativeSession());
        writeCaptureData("alice");

        CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                session, temp.resolve("fallback-replay"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll(), true));

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("import org.openqa.selenium.By;"));
        assertTrue(source.contains("private By captureReplayLocator(By primary, By... alternatives)"));
        assertTrue(source.contains("captureReplayLocator(SHAFT.GUI.Locator.inputField(\"Username\")"));
        assertTrue(result.report().fallbackLocators().stream()
                .anyMatch(fallback -> fallback.contains("username-input")));
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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
        assertTrue(source.contains("driver.element().click(SHAFT.GUI.Locator.cssSelector(\"[aria-label='Close cookie banner']\"));"));
        assertFalse(source.contains("if (driver.element().getElementsCount(SHAFT.GUI.Locator.cssSelector(\"[aria-label='Close cookie banner']\")) > 0)"));
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("if (driver.element().getElementsCount(SHAFT.GUI.Locator.cssSelector(\"[aria-label='Close cookie banner']\")) > 0)"));
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
                        "/html/body/div[3]/form/input[1]", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.POSITIONAL))),
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
                        "/html/body/div[3]/form/button[2]", 1, true, false,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.POSITIONAL))),
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

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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
        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
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

        assertTrue(retried.successful(), retried.report().unsupportedEvents().toString());
        assertEquals("SUCCESS", JSON.readTree(retried.reportPath().toFile()).path("status").asText(),
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
        assertTrue(previewResult.successful());
        assertTrue(Files.readString(preview).contains("EnrichedJourneyTest"));

        CaptureGenerationResult applied = generator.generate(new CaptureGenerationRequest(
                session, temp.resolve("applied-output"), "generated.capture", "", false,
                true, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.APPLY, preview, true,
                ApprovalPolicy.denyAll()));

        assertTrue(applied.successful(), applied.report().unsupportedEvents().toString());
        assertEquals(CaptureGenerationReport.Enrichment.EnrichmentStatus.APPLIED,
                applied.report().enrichment().status());
        String source = Files.readString(applied.sourcePath());
        assertTrue(source.contains("public class EnrichedJourneyTest"));
        assertTrue(source.contains("public void completeCheckout()"));
        assertFalse(source.contains("private static final By USERNAME_FIELD"));
        assertTrue(source.contains("driver.element().assertThat(SHAFT.GUI.Locator.inputField(\"Username\")).isVisible();"));
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
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "[aria-label='Close cookie banner']", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE))),
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
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "button[type='submit']", 1, true, true,
                        java.util.Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE))),
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
}
