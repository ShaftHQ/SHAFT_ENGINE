package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
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

import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AiCandidateRerankerTest {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        com.shaft.properties.internal.Properties.clearForCurrentThread();
    }

    @Test
    public void inventedProviderCandidateShouldBeRejected() {
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(true)
                .allowedEvidenceCategories("DOM,TEXT")
                .ollamaModel("test-model");
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
    }
}
