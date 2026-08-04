package com.shaft.pilot.ai;

import java.util.List;
import java.util.regex.Pattern;

/** Safe result of a provider model-discovery request. */
public record AiModelDiscovery(Status status, List<String> models) {
    private static final Pattern SAFE_MODEL_ID = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._:/@+\\-]{0,199}");
    private static final Pattern SECRET_LIKE_MODEL_ID = Pattern.compile(
            "(?i)(authorization|bearer|token|api[-_]?key|secret|password|(^|[-_:/])key($|[-_:/]))");
    private static final Pattern BARE_CREDENTIAL_MODEL_ID = Pattern.compile(
            "(?i)^(?:sk(?:-proj)?-|sk_(?:live|test)_|AIza|github_pat_|gh[oprsu]_|glpat-|hf_|npm_|xox[baprs]-|xapp-|xoxc-|xoxa-|rk_(?:live|test)_|pk_(?:live|test)_|whsec_)");
    private static final Pattern ENDPOINT_MODEL_ID = Pattern.compile(
            "(?i)^(?:localhost|(?:\\d{1,3}\\.){3}\\d{1,3}|[a-z0-9-]+(?:\\.[a-z0-9-]+)*):\\d{1,5}(?:/.*)?$");
    private static final Pattern AWS_ACCESS_KEY_ID = Pattern.compile("(?:AKIA|ASIA)[A-Z0-9]{16}");
    private static final Pattern BASE64URL_TOKEN = Pattern.compile("(?:[A-Za-z0-9_-]{8,}\\.){2}[A-Za-z0-9_-]{8,}");
    /** Model-discovery outcome without provider response details. */
    public enum Status { AVAILABLE, EMPTY, UNAVAILABLE, AUTHENTICATION_FAILED, FAILED }

    public AiModelDiscovery {
        status = status == null ? Status.FAILED : status;
        models = models == null ? List.of() : models.stream().filter(AiModelDiscovery::isSafeModelId)
                .map(String::trim).distinct().sorted().toList();
    }

    public static AiModelDiscovery unavailable() {
        return new AiModelDiscovery(Status.UNAVAILABLE, List.of());
    }

    /** Returns whether a provider-supplied model identifier is safe to expose to clients. */
    public static boolean isSafeModelId(String value) {
        if (value == null) {
            return false;
        }
        String modelId = value.trim();
        return modelId.length() == value.length() && SAFE_MODEL_ID.matcher(modelId).matches()
                && !modelId.contains("://") && !SECRET_LIKE_MODEL_ID.matcher(modelId).find()
                && !BARE_CREDENTIAL_MODEL_ID.matcher(modelId).find()
                && !ENDPOINT_MODEL_ID.matcher(modelId).matches()
                && !AWS_ACCESS_KEY_ID.matcher(modelId).matches()
                && !BASE64URL_TOKEN.matcher(modelId).matches();
    }
}
