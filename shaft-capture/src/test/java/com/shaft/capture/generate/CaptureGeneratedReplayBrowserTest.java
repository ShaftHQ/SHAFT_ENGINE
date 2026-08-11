package com.shaft.capture.generate;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Tag("external-e2e")
class CaptureGeneratedReplayBrowserTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @TempDir
    Path temp;

    @Test
    void generatedJourneyReplaysAgainstLocalPageAndPopulatesAllureResults() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> {
            byte[] response = """
                    <!doctype html>
                    <html>
                    <body>
                      <label for="username">Username</label>
                      <input id="username" name="username">
                      <button id="submit" onclick="message.textContent=username.value">Submit</button>
                      <p id="message"></p>
                    </body>
                    </html>
                    """.getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "text/html; charset=utf-8");
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();
        try {
            String url = "http://127.0.0.1:" + server.getAddress().getPort() + "/";
            Path sessionPath = temp.resolve("capture.json");
            new CaptureJsonCodec().write(sessionPath, session(url));
            writeData();

            CaptureGenerationResult result = new CaptureGenerator().generate(new CaptureGenerationRequest(
                    sessionPath,
                    temp.resolve("generated"),
                    "generated.capture",
                    "LocalReplayTest",
                    false,
                    true,
                    true,
                    Duration.ofMinutes(2),
                    CaptureGenerationRequest.EnrichmentMode.NONE,
                    null,
                    false,
                    ApprovalPolicy.denyAll()));

            assertTrue(result.successful(), result.report().replay().diagnostics().toString());
            assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                    result.report().replay().status());
            assertTrue(result.report().replay().allureResultCount() > 0);
        } finally {
            server.stop(0);
        }
    }

    @Test
    void selectedTestIdReplaysAcrossEquivalentResultTitleMarkup() throws Exception {
        AtomicReference<String> resultMarkup = new AtomicReference<>(
                "<h2><a data-testid=\"result-title-a\" href=\"/detail\">ShaftHQ result</a></h2>");
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/results", exchange -> {
            byte[] response = ("<!doctype html><html><body><article>"
                    + resultMarkup.get() + "</article></body></html>").getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "text/html; charset=utf-8");
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();
        try {
            String url = "http://127.0.0.1:" + server.getAddress().getPort() + "/results";
            Path sessionPath = temp.resolve("selected-test-id-capture.json");
            new CaptureJsonCodec().write(sessionPath, selectedTestIdSession(url));
            writeExpected("data.expected-title", "ShaftHQ");

            CaptureGenerationResult linkResult = new CaptureGenerator().generate(replayRequest(
                    sessionPath, temp.resolve("selected-test-id-link"), "SelectedTestIdLinkReplayTest"));
            assertTrue(linkResult.successful(), linkResult.report().replay().diagnostics().toString());

            resultMarkup.set(
                    "<h2><span data-testid=\"result-title-a\">ShaftHQ result</span></h2>");
            CaptureGenerationResult spanResult = new CaptureGenerator().generate(replayRequest(
                    sessionPath, temp.resolve("selected-test-id-span"), "SelectedTestIdSpanReplayTest"));

            assertTrue(spanResult.successful(), spanResult.report().replay().diagnostics().toString());
            assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                    linkResult.report().replay().status());
            assertEquals(CaptureGenerationReport.Validation.ValidationStatus.PASSED,
                    spanResult.report().replay().status());
            String linkSource = Files.readString(linkResult.sourcePath());
            String spanSource = Files.readString(spanResult.sourcePath());
            assertTrue(linkSource.contains("By.xpath(\"//*[@data-testid=\\\"result-title-a\\\"]\")"),
                    linkSource);
            assertTrue(spanSource.contains("By.xpath(\"//*[@data-testid=\\\"result-title-a\\\"]\")"),
                    spanSource);
        } finally {
            server.stop(0);
        }
    }

    private CaptureGenerationRequest replayRequest(Path sessionPath, Path output, String className) {
        return new CaptureGenerationRequest(
                sessionPath,
                output,
                "generated.capture",
                className,
                false,
                true,
                true,
                Duration.ofMinutes(2),
                CaptureGenerationRequest.EnrichmentMode.NONE,
                null,
                false,
                ApprovalPolicy.denyAll());
    }

    private CaptureSession selectedTestIdSession(String url) {
        ExternalTestDataReference expected = reference("data.expected-title", "expected-title");
        String stableXpath = "//*[@data-testid=\"result-title-a\"]";
        ElementSnapshot resultTitle = new ElementSnapshot(
                "result-title",
                "a",
                "link",
                "ShaftHQ result",
                "ShaftHQ result",
                Map.of("data-testid", "result-title-a"),
                List.of(new LocatorCandidate(
                        LocatorCandidate.LocatorStrategy.TEST_ID,
                        "[data-testid=\"result-title-a\"]",
                        1,
                        true,
                        true,
                        Set.of(
                                LocatorCandidate.LocatorSignal.TEST_ATTRIBUTE,
                                LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE,
                                LocatorCandidate.LocatorSignal.USER_PROVIDED),
                        stableXpath)),
                true,
                true,
                false);
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "selected-test-id-replay",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(5),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.NavigationEvent(
                                CaptureFixtures.context(1), CaptureEvent.NavigationAction.OPEN, url),
                        new CaptureEvent.VerificationEvent(
                                CaptureFixtures.context(2), CaptureEvent.VerificationKind.TEXT_CONTAINS,
                                resultTitle, expected, false)),
                List.of(),
                List.of(expected),
                RedactionSummary.empty(),
                Map.of());
    }

    private CaptureSession session(String url) {
        ExternalTestDataReference username = reference("data.username", "username");
        ExternalTestDataReference expected = reference("data.expected-message", "expected-message");
        // Issue #4239 P1.4-decision ladder: this fixture drives a REAL generate+replay against the
        // HTML above, so each ElementSnapshot's locator evidence must mirror what the real recorder
        // would actually capture for that markup, not just an ID candidate the ladder now refuses to
        // emit. <input id="username"> has a genuine <label for="username">, so a real recorder infers
        // role=textbox and self-verifies SHAFT.GUI.Locator.hasRole(Role.TEXTBOX) resolves uniquely
        // (rung 1); <button id="submit">Submit</button> similarly self-verifies role=button (rung 1).
        // <p id="message"> has no ARIA role and no label, but by verification time its text content is
        // the replayed username ("Alice", per writeData() below) -- a real recorder would self-verify
        // a text-based XPath fallback (rung 2) exactly like this.
        ElementSnapshot input = target("username", "input", "textbox", "Username",
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "textbox:Username", 1, true, true,
                        Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE), "", true));
        ElementSnapshot button = target("submit", "button", "button", "Submit",
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.ROLE,
                        "button:Submit", 1, true, true,
                        Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                        "//button[normalize-space(.)=\"Submit\"]", true));
        ElementSnapshot message = target("message", "p", "", "",
                new LocatorCandidate(LocatorCandidate.LocatorStrategy.XPATH,
                        "//p[normalize-space(.)=\"Alice\"]", 1, true, true,
                        Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE),
                        "//p[normalize-space(.)=\"Alice\"]"));
        List<CaptureEvent> events = List.of(
                new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1),
                        CaptureEvent.NavigationAction.OPEN,
                        url),
                new CaptureEvent.TypeEvent(CaptureFixtures.context(2), input, username),
                new CaptureEvent.ClickEvent(
                        CaptureFixtures.context(3),
                        button,
                        CaptureEvent.MouseButton.PRIMARY,
                        1),
                new CaptureEvent.VerificationEvent(
                        CaptureFixtures.context(4),
                        CaptureEvent.VerificationKind.TEXT_EQUALS,
                        message,
                        expected,
                        false));
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "local-replay",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(10),
                CaptureFixtures.browser(),
                events,
                List.of(new Checkpoint(
                        "assert-message",
                        4,
                        CaptureFixtures.STARTED.plusSeconds(5),
                        Checkpoint.CheckpointKind.ASSERTION,
                        "Message equals typed value")),
                List.of(username, expected),
                RedactionSummary.empty(),
                Map.of());
    }

    private static ElementSnapshot target(
            String id,
            String tag,
            String role,
            String accessibleName,
            LocatorCandidate primaryCandidate) {
        return new ElementSnapshot(
                id,
                tag,
                role,
                accessibleName,
                accessibleName,
                Map.of("id", id),
                List.of(primaryCandidate, new LocatorCandidate(
                        LocatorCandidate.LocatorStrategy.ID,
                        id,
                        1,
                        true,
                        true,
                        Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE))),
                true,
                true,
                false);
    }

    private static ExternalTestDataReference reference(String id, String logicalName) {
        return new ExternalTestDataReference(
                id,
                logicalName,
                ExternalTestDataReference.DataSource.JSON,
                "capture-data.json",
                "/values/" + id,
                ExternalTestDataReference.DataClassification.ORDINARY);
    }

    private void writeData() throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        ObjectNode values = root.putObject("values");
        values.put("data.username", "Alice");
        values.put("data.expected-message", "Alice");
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);
    }

    private void writeExpected(String id, String value) throws Exception {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        root.putObject("values").put(id, value);
        Files.writeString(temp.resolve("capture-data.json"),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root), StandardCharsets.UTF_8);
    }
}
