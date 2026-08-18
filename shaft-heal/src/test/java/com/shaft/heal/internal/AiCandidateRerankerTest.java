package com.shaft.heal.internal;

import tools.jackson.databind.node.JsonNodeFactory;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import com.shaft.driver.SHAFT;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingScore;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.EvidenceReference;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AiCandidateRerankerTest {
    private HttpServer server;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (server != null) {
            server.stop(0);
            server = null;
        }
        com.shaft.properties.internal.Properties.clearForCurrentThread();
    }

    @Test
    public void capturedRerankRequestMustOmitProposedLocator() {
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model");
        String proposedLocator = By.cssSelector("#unique-heal-locator-canary[data-token='abc']").toString();
        HealingCandidate report = new HealingCandidate(
                "candidate-1",
                proposedLocator,
                DeterministicScorerTest.fingerprint("candidate-1", "Username"),
                new HealingScore(0.9, null, null, 0.9, Map.of("accessibility", 1.0)),
                List.of("accessibility=1.000"),
                true,
                true,
                true,
                true);
        RankedCandidate candidate = new RankedCandidate(
                mock(WebElement.class), By.id("candidate-1"), report);
        AtomicReference<AiRequest> captured = new AtomicReference<>();
        AiExecutionService service = mock(AiExecutionService.class);
        when(service.execute(any())).thenAnswer(invocation -> {
            captured.set(invocation.getArgument(0));
            var payload = JsonNodeFactory.instance.objectNode();
            payload.putArray("ranking").addObject()
                    .put("candidateId", "candidate-1")
                    .put("confidence", 0.98)
                    .putArray("citedFeatures").add("accessibility");
            return AiResponse.success(
                    "ollama", "test-model", payload, Duration.ofMillis(1), AiUsage.empty(),
                    JsonNodeFactory.instance.objectNode());
        });

        new AiCandidateReranker(configuration(), service).apply(List.of(candidate));

        Assert.assertNotNull(captured.get(), "Reranker must submit a request through AiExecutionService.");
        String serialized = captured.get().evidence().stream()
                .map(EvidenceReference::content)
                .reduce("", (left, right) -> left + "\n" + right);
        Assert.assertFalse(
                serialized.contains(proposedLocator),
                "Rerank evidence must not contain the proposed locator.");
        Assert.assertFalse(
                serialized.contains("unique-heal-locator-canary"),
                "Rerank evidence must not contain raw locator text.");
        Assert.assertTrue(serialized.contains("\"candidateId\""), serialized);
        Assert.assertTrue(serialized.contains("\"deterministicScore\""), serialized);
        Assert.assertTrue(serialized.contains("\"featureScores\""), serialized);
        Assert.assertTrue(serialized.contains("\"unique\""), serialized);
        Assert.assertTrue(serialized.contains("failure-context")
                        || captured.get().evidence().stream()
                        .anyMatch(item -> "failure-context".equals(item.id())),
                "Rerank request must include one minimized failure-context item.");
    }

    @Test
    public void failedProviderStatusesReturnTheExactDeterministicList() {
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model");
        RankedCandidate first = candidate("candidate-2", 0.90);
        RankedCandidate second = candidate("candidate-1", 0.80);
        List<RankedCandidate> deterministicOrder = List.of(first, second);
        AiResponseStatus[] failures = {
                AiResponseStatus.TIMEOUT,
                AiResponseStatus.CIRCUIT_OPEN,
                AiResponseStatus.PROVIDER_UNAVAILABLE,
                AiResponseStatus.INVALID_RESPONSE
        };
        for (AiResponseStatus status : failures) {
            AiExecutionService service = mock(AiExecutionService.class);
            when(service.execute(any())).thenReturn(AiResponse.failure(
                    status, "ollama", "test-model", status.name(), Duration.ofMillis(1),
                    JsonNodeFactory.instance.objectNode()));

            AiCandidateReranker.RerankResult result = new AiCandidateReranker(configuration(), service)
                    .apply(deterministicOrder);

            Assert.assertSame(result.candidates(), deterministicOrder, status.name());
            Assert.assertNull(result.candidates().getFirst().report().score().providerScore(), status.name());
        }
    }

    @Test
    public void invalidSchemaReturnsTheExactDeterministicList() {
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model");
        RankedCandidate first = candidate("candidate-2", 0.90);
        RankedCandidate second = candidate("candidate-1", 0.80);
        List<RankedCandidate> deterministicOrder = List.of(first, second);
        var payload = JsonNodeFactory.instance.objectNode();
        payload.putArray("ranking").addObject()
                .put("candidateId", "candidate-2")
                .put("confidence", 2.5)
                .putArray("citedFeatures").add("accessibility");
        AiExecutionService service = mock(AiExecutionService.class);
        when(service.execute(any())).thenReturn(AiResponse.success(
                "ollama", "test-model", payload, Duration.ofMillis(1), AiUsage.empty(),
                JsonNodeFactory.instance.objectNode()));

        AiCandidateReranker.RerankResult result = new AiCandidateReranker(configuration(), service)
                .apply(deterministicOrder);

        Assert.assertEquals(result.metadata().status(), "REJECTED");
        Assert.assertSame(result.candidates(), deterministicOrder);
    }

    @Test
    public void uncitedProviderRankingReturnsExactDeterministicOrdering() {
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model");
        RankedCandidate first = candidate("candidate-2", 0.90);
        RankedCandidate second = candidate("candidate-1", 0.80);
        List<RankedCandidate> deterministicOrder = List.of(first, second);
        var payload = JsonNodeFactory.instance.objectNode();
        payload.putArray("ranking").addObject()
                .put("candidateId", "candidate-2")
                .put("score", 0.98);
        AiExecutionService service = mock(AiExecutionService.class);
        when(service.execute(any())).thenReturn(AiResponse.success(
                "ollama", "test-model", payload, Duration.ofMillis(1), AiUsage.empty(),
                JsonNodeFactory.instance.objectNode()));

        AiCandidateReranker.RerankResult result = new AiCandidateReranker(configuration(), service)
                .apply(deterministicOrder);

        Assert.assertEquals(result.metadata().status(), "REJECTED");
        Assert.assertSame(result.candidates(), deterministicOrder);
        Assert.assertNull(result.candidates().getFirst().report().score().providerScore());
    }

    @Test
    public void inventedProviderCandidateShouldBeRejected() {
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(false)
                .onPremConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model")
                .ollamaProcessingLocation("on-prem");
        HealingConfiguration configuration = new HealingConfiguration(
                0.75,
                0.10,
                java.util.Set.of("accessibility"),
                List.of("data-testid"),
                false,
                java.nio.file.Path.of("target", "unused.json"),
                10,
                Duration.ofDays(30),
                false,
                true,
                false);
        HealingCandidate report = new HealingCandidate(
                "candidate-1",
                By.id("candidate-1").toString(),
                DeterministicScorerTest.fingerprint("candidate-1", "Username"),
                new HealingScore(0.9, null, null, 0.9, Map.of("accessibility", 1.0)),
                List.of("accessibility=1.000"),
                true,
                true,
                true,
                true);
        RankedCandidate candidate = new RankedCandidate(
                mock(WebElement.class), By.id("candidate-1"), report);
        var payload = JsonNodeFactory.instance.objectNode();
        payload.putArray("ranking").addObject()
                .put("candidateId", "invented")
                .put("confidence", 1.0)
                .putArray("citedFeatures").add("accessibility");
        AiExecutionService service = mock(AiExecutionService.class);
        when(service.execute(any())).thenReturn(AiResponse.success(
                "ollama", "test-model", payload, Duration.ofMillis(1), AiUsage.empty(),
                JsonNodeFactory.instance.objectNode()));

        AiCandidateReranker.RerankResult result = new AiCandidateReranker(configuration, service)
                .apply(List.of(candidate));

        Assert.assertEquals(result.metadata().status(), "REJECTED");
        Assert.assertNull(result.candidates().getFirst().report().score().providerScore());
        Assert.assertTrue(result.remoteEvidenceSent());
    }

    @Test
    public void serviceLoadedOllamaShouldRerankRedactedEvidenceLocally() throws Exception {
        AtomicReference<String> capturedBody = new AtomicReference<>("");
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/api/chat", exchange -> {
            capturedBody.set(new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
            respond(exchange, """
                    {
                      "model": "heal-test-model",
                      "message": {
                        "role": "assistant",
                        "content": "{\\"ranking\\":[{\\"candidateId\\":\\"candidate-1\\",\\"confidence\\":0.98,\\"citedFeatures\\":[\\"accessibility\\"]}]}"
                      },
                      "prompt_eval_count": 20,
                      "eval_count": 8
                    }
                    """);
        });
        server.start();
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .remoteConsent(false)
                .allowedEvidenceCategories("DOM,TEXT")
                .retryMaxAttempts(1)
                .ollamaEndpoint("http://127.0.0.1:" + server.getAddress().getPort() + "/api/chat")
                .ollamaModel("heal-test-model")
                .ollamaProcessingLocation("local");
        HealingCandidate report = new HealingCandidate(
                "candidate-1",
                By.id("candidate-1").toString(),
                DeterministicScorerTest.fingerprint("candidate-1", "Username"),
                new HealingScore(0.9, null, null, 0.9, Map.of("accessibility", 1.0)),
                List.of("authorization=Bearer do-not-transmit"),
                true,
                true,
                true,
                true);
        RankedCandidate candidate = new RankedCandidate(
                mock(WebElement.class), By.id("candidate-1"), report);

        AiCandidateReranker.RerankResult result =
                new AiCandidateReranker(configuration()).apply(List.of(candidate));

        Assert.assertEquals(result.metadata().provider(), "ollama");
        Assert.assertEquals(result.metadata().processingLocation(), "LOCAL");
        Assert.assertFalse(result.remoteEvidenceSent());
        Assert.assertEquals(result.candidates().getFirst().report().score().providerScore(), 0.98);
        Assert.assertFalse(capturedBody.get().contains("do-not-transmit"));
        Assert.assertFalse(capturedBody.get().contains("authorization"));
        Assert.assertFalse(capturedBody.get().contains("By.id"));
        Assert.assertFalse(capturedBody.get().contains("proposedLocator"));
    }

    @Test
    public void rerankCorpusRecordsMetricsWithoutEnablingDefaults() throws Exception {
        Assert.assertFalse(SHAFT.Properties.healing.aiEnabled());
        Assert.assertFalse(SHAFT.Properties.healing.visualEnabled());
        var corpus = JsonNodeFactory.instance.objectNode();
        try (var stream = getClass().getResourceAsStream("/fixtures/heal-rerank-corpus.json")) {
            Assert.assertNotNull(stream, "Heal rerank corpus fixture must exist.");
            corpus = (tools.jackson.databind.node.ObjectNode) new tools.jackson.databind.ObjectMapper()
                    .readTree(stream);
        }
        Assert.assertFalse(corpus.path("defaults").path("healingAiEnabled").asBoolean(true));
        Assert.assertTrue(corpus.path("cases").isArray());
        Assert.assertFalse(corpus.path("cases").isEmpty());
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model");
        for (var caseNode : corpus.path("cases")) {
            RankedCandidate first = candidate("candidate-1", 0.90);
            RankedCandidate second = candidate("candidate-2", 0.80);
            List<RankedCandidate> input = List.of(first, second);
            var payload = JsonNodeFactory.instance.objectNode();
            if ("cited".equals(caseNode.path("providerOutcome").asText())) {
                payload.putArray("ranking").addObject()
                        .put("candidateId", "candidate-1")
                        .put("confidence", 0.98)
                        .putArray("citedFeatures").add("accessibility");
            } else {
                payload.putArray("ranking").addObject()
                        .put("candidateId", "invented")
                        .put("confidence", 1.0)
                        .putArray("citedFeatures").add("accessibility");
            }
            AiExecutionService service = mock(AiExecutionService.class);
            when(service.execute(any())).thenReturn(AiResponse.success(
                    "ollama", "test-model", payload, Duration.ofMillis(caseNode.path("latencyMs").asInt(1)),
                    AiUsage.empty(), JsonNodeFactory.instance.objectNode()));
            long started = System.nanoTime();
            AiCandidateReranker.RerankResult result = new AiCandidateReranker(configuration(), service)
                    .apply(input);
            long latencyMs = Duration.ofNanos(System.nanoTime() - started).toMillis();
            String top1 = result.candidates().stream()
                    .max((left, right) -> Double.compare(
                            left.report().score().finalScore(),
                            right.report().score().finalScore()))
                    .orElseThrow()
                    .report()
                    .candidateId();
            int inventedCount = "cited".equals(caseNode.path("providerOutcome").asText()) ? 0 : 1;
            Assert.assertEquals(top1, caseNode.path("expectedTop1").asText(), caseNode.path("id").asText());
            Assert.assertEquals(inventedCount, caseNode.path("inventedCount").asInt(-1), caseNode.path("id").asText());
            Assert.assertFalse(caseNode.path("regression").asBoolean(true), caseNode.path("id").asText());
            Assert.assertTrue(latencyMs >= 0, caseNode.path("id").asText());
            if (inventedCount > 0) {
                Assert.assertSame(result.candidates(), input, caseNode.path("id").asText());
            }
        }
        Assert.assertFalse(SHAFT.Properties.healing.aiEnabled());
    }

    private static HealingConfiguration configuration() {
        return new HealingConfiguration(
                0.75,
                0.10,
                java.util.Set.of("accessibility"),
                List.of("data-testid"),
                false,
                java.nio.file.Path.of("target", "unused.json"),
                10,
                Duration.ofDays(30),
                false,
                true,
                false);
    }

    private static RankedCandidate candidate(String candidateId, double deterministicScore) {
        HealingCandidate report = new HealingCandidate(
                candidateId,
                By.id(candidateId).toString(),
                DeterministicScorerTest.fingerprint(candidateId, "Username"),
                new HealingScore(deterministicScore, null, null, deterministicScore,
                        Map.of("accessibility", deterministicScore)),
                List.of("accessibility=" + deterministicScore),
                true,
                true,
                true,
                true);
        return new RankedCandidate(mock(WebElement.class), By.id(candidateId), report);
    }

    private static void respond(HttpExchange exchange, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }
}
