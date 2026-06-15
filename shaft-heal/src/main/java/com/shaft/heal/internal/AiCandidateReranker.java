package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingReport;
import com.shaft.heal.model.HealingScore;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.EvidenceReference;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.config.PilotConfiguration;

import java.time.Duration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class AiCandidateReranker {
    private static final ObjectMapper JSON = new ObjectMapper();
    private final HealingConfiguration configuration;
    private final AiExecutionService executionService;

    AiCandidateReranker(HealingConfiguration configuration) {
        this(configuration, new AiExecutionService());
    }

    AiCandidateReranker(HealingConfiguration configuration, AiExecutionService executionService) {
        this.configuration = configuration;
        this.executionService = executionService;
    }

    RerankResult apply(List<RankedCandidate> candidates) {
        if (!configuration.aiEnabled() || candidates.isEmpty()) {
            return new RerankResult(candidates, HealingReport.ProviderMetadata.disabled(), false);
        }
        PilotConfiguration pilotConfiguration;
        try {
            pilotConfiguration = PilotConfiguration.current();
        } catch (RuntimeException exception) {
            return fallback(candidates, "AI configuration is invalid.");
        }
        ObjectNode responseSchema = responseSchema();
        ObjectNode deterministicFallback = rankingPayload(candidates, false);
        AiRequest.Builder builder = AiRequest.builder("shaft-heal-candidate-rerank", responseSchema)
                .text("""
                        Rerank only the supplied SHAFT Heal candidates. Never invent a candidate ID or locator.
                        Scores must be between 0 and 1 and must be based only on the minimized evidence.
                        """)
                .timeout(Duration.ofSeconds(Math.max(1, pilotConfiguration.timeout().toSeconds())))
                .approvalPolicy(pilotConfiguration.approvalPolicy())
                .deterministicFallback(deterministicFallback);
        for (RankedCandidate candidate : candidates) {
            try {
                builder.evidence(new EvidenceReference(
                        candidate.report().candidateId(),
                        EvidenceCategory.DOM,
                        "application/json",
                        JSON.writeValueAsString(candidate.report())));
            } catch (com.fasterxml.jackson.core.JsonProcessingException exception) {
                return fallback(candidates, "Candidate evidence could not be serialized.");
            }
        }

        AiResponse response = executionService.execute(builder.build());
        ProcessingLocation location = processingLocation(pilotConfiguration, response.provider());
        HealingReport.ProviderMetadata metadata = new HealingReport.ProviderMetadata(
                true,
                response.provider(),
                response.model(),
                response.status().name(),
                HealingSupport.sanitize(response.fallbackReason()),
                location.name(),
                location.name().toLowerCase(java.util.Locale.ROOT).replace('_', '-'),
                "Pilot redaction policy applied before provider execution.");
        if (!response.successful()) {
            return new RerankResult(candidates, metadata, evidenceLeavesProcess(location));
        }
        Map<String, Double> providerScores;
        try {
            providerScores = parse(response.structuredPayload(), candidates);
        } catch (IllegalArgumentException exception) {
            HealingReport.ProviderMetadata rejected = new HealingReport.ProviderMetadata(
                    true,
                    response.provider(),
                    response.model(),
                    "REJECTED",
                    HealingSupport.sanitize(exception.getMessage()),
                    location.name(),
                    location.name().toLowerCase(java.util.Locale.ROOT).replace('_', '-'),
                    "Pilot redaction policy applied before provider execution.");
            return new RerankResult(candidates, rejected, evidenceLeavesProcess(location));
        }

        List<RankedCandidate> updated = candidates.stream().map(candidate -> {
            Double providerScore = providerScores.get(candidate.report().candidateId());
            if (providerScore == null) {
                return candidate;
            }
            HealingScore old = candidate.report().score();
            double finalScore = old.finalScore() * 0.85 + providerScore * 0.15;
            HealingScore updatedScore = new HealingScore(
                    old.deterministicScore(),
                    old.visualScore(),
                    providerScore,
                    finalScore,
                    old.evidenceScores());
            HealingCandidate oldReport = candidate.report();
            HealingCandidate updatedReport = new HealingCandidate(
                    oldReport.candidateId(),
                    oldReport.proposedLocator(),
                    oldReport.fingerprint(),
                    updatedScore,
                    oldReport.evidence(),
                    oldReport.unique(),
                    oldReport.visible(),
                    oldReport.interactable(),
                    oldReport.contextMatched());
            return new RankedCandidate(candidate.element(), candidate.locator(), updatedReport);
        }).toList();
        return new RerankResult(updated, metadata, evidenceLeavesProcess(location));
    }

    private static Map<String, Double> parse(JsonNode payload, List<RankedCandidate> candidates) {
        Set<String> allowed = candidates.stream()
                .map(candidate -> candidate.report().candidateId())
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        Map<String, Double> scores = new HashMap<>();
        for (JsonNode item : payload.path("ranking")) {
            String candidateId = item.path("candidateId").asText();
            double score = item.path("score").asDouble(Double.NaN);
            if (!allowed.contains(candidateId)) {
                throw new IllegalArgumentException("Provider referenced an unknown candidate.");
            }
            if (!Double.isFinite(score) || score < 0 || score > 1) {
                throw new IllegalArgumentException("Provider returned an invalid score.");
            }
            if (scores.put(candidateId, score) != null) {
                throw new IllegalArgumentException("Provider returned a duplicate candidate.");
            }
        }
        if (scores.isEmpty()) {
            throw new IllegalArgumentException("Provider returned no candidate scores.");
        }
        return Map.copyOf(scores);
    }

    private static ObjectNode responseSchema() {
        ObjectNode root = JSON.createObjectNode();
        root.put("type", "object");
        ObjectNode properties = root.putObject("properties");
        ObjectNode ranking = properties.putObject("ranking");
        ranking.put("type", "array");
        ObjectNode item = ranking.putObject("items");
        item.put("type", "object");
        ObjectNode itemProperties = item.putObject("properties");
        itemProperties.putObject("candidateId").put("type", "string");
        ObjectNode score = itemProperties.putObject("score");
        score.put("type", "number");
        score.put("minimum", 0);
        score.put("maximum", 1);
        ArrayNode requiredItem = item.putArray("required");
        requiredItem.add("candidateId");
        requiredItem.add("score");
        item.put("additionalProperties", false);
        root.putArray("required").add("ranking");
        root.put("additionalProperties", false);
        return root;
    }

    private static ObjectNode rankingPayload(List<RankedCandidate> candidates, boolean providerScore) {
        ObjectNode root = JSON.createObjectNode();
        ArrayNode ranking = root.putArray("ranking");
        candidates.forEach(candidate -> {
            ObjectNode item = ranking.addObject();
            item.put("candidateId", candidate.report().candidateId());
            item.put("score", providerScore && candidate.report().score().providerScore() != null
                    ? candidate.report().score().providerScore()
                    : candidate.report().score().deterministicScore());
        });
        return root;
    }

    private static RerankResult fallback(List<RankedCandidate> candidates, String reason) {
        return new RerankResult(
                candidates,
                new HealingReport.ProviderMetadata(true, "none", "", "FALLBACK", reason),
                false);
    }

    private static ProcessingLocation processingLocation(
            PilotConfiguration configuration,
            String provider) {
        try {
            return configuration.provider(provider).processingLocation();
        } catch (IllegalArgumentException exception) {
            return ProcessingLocation.NONE;
        }
    }

    private static boolean evidenceLeavesProcess(ProcessingLocation location) {
        return location == ProcessingLocation.ON_PREM
                || location == ProcessingLocation.REMOTE;
    }

    record RerankResult(
            List<RankedCandidate> candidates,
            HealingReport.ProviderMetadata metadata,
            boolean remoteEvidenceSent) {
    }
}
