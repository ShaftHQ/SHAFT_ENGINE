package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;
import tools.jackson.core.StreamReadFeature;
import tools.jackson.databind.DeserializationFeature;

/** Stable JSON codec for setup-plan schema version 2. */
public final class SetupPlanJson {
    private static final JsonMapper JSON = JsonMapper.builder()
            .enable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
            .build();

    private SetupPlanJson() { }

    public static String write(SetupPlan plan) {
        return JSON.writerWithDefaultPrettyPrinter().writeValueAsString(plan) + System.lineSeparator();
    }

    public static SetupPlan read(String json) {
        try {
            return JSON.readValue(json, SetupPlan.class);
        } catch (tools.jackson.databind.DatabindException invalid) {
            if (invalid.getCause() instanceof IllegalArgumentException rejected) throw rejected;
            throw invalid;
        }
    }
}
