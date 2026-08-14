package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;
import tools.jackson.core.StreamReadFeature;
import tools.jackson.databind.DeserializationFeature;

/** Stable, strict JSON codec for setup-plan schemas 2 through 4. */
public final class SetupPlanJson {
    private static final JsonMapper JSON = JsonMapper.builder()
            .enable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
            .build();

    private SetupPlanJson() { }

    public static String write(SetupPlan plan) {
        tools.jackson.databind.node.ObjectNode tree = JSON.valueToTree(plan);
        if (plan.schemaVersion() == 2) tree.remove("executionPolicyDigest");
        if (plan.schemaVersion() < 4) {
            tree.withArray("actions").forEach(action -> ((tools.jackson.databind.node.ObjectNode) action)
                    .remove("artifactBytes"));
        }
        return JSON.writerWithDefaultPrettyPrinter().writeValueAsString(tree) + System.lineSeparator();
    }

    public static SetupPlan read(String json) {
        try {
            tools.jackson.databind.JsonNode tree = JSON.readTree(json);
            int schema = tree.path("schemaVersion").asInt(-1);
            boolean hasPolicy = tree.has("executionPolicyDigest");
            if (schema == 2 && hasPolicy) {
                throw new IllegalArgumentException("Schema 2 plans must not contain executionPolicyDigest.");
            }
            if ((schema == 3 || schema == 4) && !hasPolicy) {
                throw new IllegalArgumentException("Schema " + schema + " plans require executionPolicyDigest.");
            }
            for (tools.jackson.databind.JsonNode action : tree.path("actions")) {
                if (schema < 4 && action.has("artifactBytes")) {
                    throw new IllegalArgumentException("Schemas 2 and 3 must not contain artifactBytes.");
                }
                if (schema == 4 && !action.has("artifactBytes")) {
                    throw new IllegalArgumentException("Schema 4 plans require artifactBytes for every action.");
                }
            }
            return JSON.readValue(json, SetupPlan.class);
        } catch (tools.jackson.databind.DatabindException invalid) {
            if (invalid.getCause() instanceof IllegalArgumentException rejected) throw rejected;
            throw invalid;
        }
    }
}
