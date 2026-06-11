package com.shaft.capture.generate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
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

    private CaptureSession session(String url) {
        ExternalTestDataReference username = reference("data.username", "username");
        ExternalTestDataReference expected = reference("data.expected-message", "expected-message");
        ElementSnapshot input = target("username", "input", "textbox", "Username");
        ElementSnapshot button = target("submit", "button", "button", "Submit");
        ElementSnapshot message = target("message", "p", "", "");
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
            String accessibleName) {
        return new ElementSnapshot(
                id,
                tag,
                role,
                accessibleName,
                accessibleName,
                Map.of("id", id),
                List.of(new LocatorCandidate(
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
}
