package com.shaft.mcp;

import com.shaft.infrastructure.SetupPlan;

import java.util.Objects;

/** Reviewed setup plan plus its stable JSON representation and approval digest. */
public record McpSetupPlanResult(SetupPlan plan, String planJson, String digest) {
    public McpSetupPlanResult {
        Objects.requireNonNull(plan, "plan");
        if (planJson == null || planJson.isBlank()) throw new IllegalArgumentException("planJson must not be blank.");
        if (!plan.digest().equals(digest)) throw new IllegalArgumentException("digest must identify plan.");
    }
}
