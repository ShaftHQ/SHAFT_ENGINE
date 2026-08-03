package com.shaft.pilot.ai;

import java.util.List;

/** Safe result of a provider model-discovery request. */
public record AiModelDiscovery(Status status, List<String> models) {
    /** Model-discovery outcome without provider response details. */
    public enum Status { AVAILABLE, EMPTY, UNAVAILABLE, AUTHENTICATION_FAILED, FAILED }

    public AiModelDiscovery {
        status = status == null ? Status.FAILED : status;
        models = models == null ? List.of() : models.stream().filter(model -> model != null && !model.isBlank())
                .map(String::trim).distinct().sorted().toList();
    }

    public static AiModelDiscovery unavailable() {
        return new AiModelDiscovery(Status.UNAVAILABLE, List.of());
    }
}
