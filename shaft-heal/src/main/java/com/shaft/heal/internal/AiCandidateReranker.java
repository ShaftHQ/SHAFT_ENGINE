package com.shaft.heal.internal;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
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
            return new RerankResult(candidates, HealingReport.ProviderMetadata.disabled(), false, false);
        }
        PilotConfiguration pilotConfiguration;
        try {
            pilotConfiguration = PilotConfiguration.current();
        } catch (RuntimeException exception) {
            return fallback(candidates, "AI configuration is invalid.");
        }
        ObjectNode responseSchema = responseSchema(candidates);
        ObjectNode deterministicFallback = rankingPayload(candidates, false);
        AiRequest.Builder builder = AiRequest.builder("shaft-heal-candidate-rerank", responseSchema)
                .text("""
                        Rerank only the supplied SHAFT Heal candidates. Never invent a candidate ID or locator.
                        Confidence must be between 0 and 1 and must be based only on the minimized evidence.
                        Cite one or more feature names supplied for that candidate.
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
            } catch (tools.jackson.core.JacksonException exception) {
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
            return new RerankResult(candidates, metadata, evidenceLeavesProcess(location), true);
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
            return new RerankResult(candidates, rejected, evidenceLeavesProcess(location), true);
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
        return new RerankResult(updated, metadata, evidenceLeavesProcess(location), true);
    }

    private static Map<String, Double> parse(JsonNode payload, List<RankedCandidate> candidates) {
        Map<String, RankedCandidate> allowed = candidates.stream()
                .collect(java.util.stream.Collectors.toUnmodifiableMap(
                        candidate -> candidate.report().candidateId(),
                        candidate -> candidate));
        Map<String, Double> scores = new HashMap<>();
        for (JsonNode item : payload.path("ranking")) {
            String candidateId = item.path("candidateId").asText();
            double score = item.path("confidence").asDouble(Double.NaN);
            RankedCandidate candidate = allowed.get(candidateId);
            if (candidate == null) {
                throw new IllegalArgumentException("Provider referenced an unknown candidate.");
            }
            if (!Double.isFinite(score) || score < 0 || score > 1) {
                throw new IllegalArgumentException("Provider returned an invalid confidence.");
            }
            Set<String> citedFeatures = new HashSet<>();
            for (JsonNode citedFeature : item.path("citedFeatures")) {
                String feature = citedFeature.asText();
                if (!allowedFeatures(candidate).contains(feature)) {
                    throw new IllegalArgumentException("Provider cited an unknown feature.");
                }
                if (!citedFeatures.add(feature)) {
                    throw new IllegalArgumentException("Provider cited a duplicate feature.");
                }
            }
            if (citedFeatures.isEmpty()) {
                throw new IllegalArgumentException("Provider returned no cited features.");
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

    private static Set<String> allowedFeatures(RankedCandidate candidate) {
        Set<String> features = new HashSet<>(candidate.report().score().evidenceScores().keySet());
        features.add("deterministicScore");
        features.add("unique");
        features.add("visible");
        features.add("interactable");
        features.add("contextMatched");
        if (candidate.report().score().visualScore() != null) {
            features.add("visualScore");
        }
        return Set.copyOf(features);
    }

    private static ObjectNode responseSchema(List<RankedCandidate> candidates) {
        ObjectNode root = JSON.createObjectNode();
        root.put("type", "object");
        ObjectNode properties = root.putObject("properties");
        ObjectNode ranking = properties.putObject("ranking");
        ranking.put("type", "array");
        ranking.put("minItems", 1);
        ranking.put("maxItems", candidates.size());
        ObjectNode item = ranking.putObject("items");
        item.put("type", "object");
        ObjectNode itemProperties = item.putObject("properties");
        ObjectNode candidateId = itemProperties.putObject("candidateId");
        candidateId.put("type", "string");
        ArrayNode candidateIds = candidateId.putArray("enum");
        candidates.forEach(candidate -> candidateIds.add(candidate.report().candidateId()));
        ObjectNode score = itemProperties.putObject("confidence");
        score.put("type", "number");
        score.put("minimum", 0);
        score.put("maximum", 1);
        ObjectNode citedFeatures = itemProperties.putObject("citedFeatures");
        citedFeatures.put("type", "array");
        citedFeatures.put("minItems", 1);
        citedFeatures.put("uniqueItems", true);
        ObjectNode citedFeature = citedFeatures.putObject("items");
        citedFeature.put("type", "string");
        ArrayNode featureNames = citedFeature.putArray("enum");
        candidates.stream()
                .flatMap(candidate -> allowedFeatures(candidate).stream())
                .distinct()
                .sorted()
                .forEach(featureNames::add);
        ArrayNode requiredItem = item.putArray("required");
        requiredItem.add("candidateId");
        requiredItem.add("confidence");
        requiredItem.add("citedFeatures");
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
            item.put("confidence", providerScore && candidate.report().score().providerScore() != null
                    ? candidate.report().score().providerScore()
                    : candidate.report().score().deterministicScore());
            item.putArray("citedFeatures").add("deterministicScore");
        });
        return root;
    }

    private static RerankResult fallback(List<RankedCandidate> candidates, String reason) {
        return new RerankResult(
                candidates,
                new HealingReport.ProviderMetadata(true, "none", "", "FALLBACK", reason),
                false,
                true);
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
            boolean remoteEvidenceSent,
            boolean applied) {
    }
}
