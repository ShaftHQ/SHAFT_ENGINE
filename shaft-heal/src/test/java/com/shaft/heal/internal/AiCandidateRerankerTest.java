package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import com.shaft.driver.SHAFT;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingScore;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
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
                .put("score", 1.0);
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
                        "content": "{\\"ranking\\":[{\\"candidateId\\":\\"candidate-1\\",\\"score\\":0.98}]}"
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
        Assert.assertTrue(capturedBody.get().contains("[REDACTED]"));
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

    private static void respond(HttpExchange exchange, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }
}
