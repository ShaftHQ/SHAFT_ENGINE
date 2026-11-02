package com.shaft.capture.generate.api;

import com.shaft.tools.io.internal.HttpContractRecorder;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.util.ArrayList;
import java.util.List;

/**
 * Classifies each scalar leaf of a recorded JSON response body as {@link LeafClassification#STABLE},
 * {@link LeafClassification#VOLATILE}, or {@link LeafClassification#SENSITIVE}, reusing the exact
 * same key/value volatility and sensitivity detection {@link HttpContractRecorder} uses for HTTP
 * contract normalization, so API codegen and contract capture agree on what counts as noise vs. a
 * stable, assertable value.
 *
 * <p>{@link LeafClassification#CORRELATED} is never assigned here -- {@code TransactionCorrelator}
 * upgrades a subset of {@code VOLATILE} leaves to {@code CORRELATED} after finding that a later
 * request in the same session actually reuses their value.
 */
public final class ResponseNormalizer {
    private static final ObjectMapper MAPPER = new JsonMapper();

    private ResponseNormalizer() {
    }

    /**
     * Parses a JSON response body and classifies every scalar leaf, in encounter order.
     *
     * @param json raw JSON response body text
     * @return classified leaves, or an empty list if the body is blank or not valid JSON
     */
    public static List<ResponseLeaf> classify(String json) {
        if (json == null || json.isBlank()) {
            return List.of();
        }
        JsonNode root;
        try {
            root = MAPPER.readTree(json);
        } catch (RuntimeException malformed) {
            return List.of();
        }
        List<ResponseLeaf> leaves = new ArrayList<>();
        walk("$", "", root, leaves);
        return List.copyOf(leaves);
    }

    private static void walk(String path, String key, JsonNode node, List<ResponseLeaf> leaves) {
        if (node == null || node.isNull() || node.isMissingNode()) {
            return;
        }
        if (node.isObject()) {
            node.forEachEntry((childKey, childValue) ->
                    walk(path + "." + childKey, childKey, childValue, leaves));
            return;
        }
        if (node.isArray()) {
            int index = 0;
            for (JsonNode element : node) {
                walk(path + "[" + index + "]", key, element, leaves);
                index++;
            }
            return;
        }
        String textValue = node.asText("");
        leaves.add(new ResponseLeaf(path, key, textValue, classifyLeaf(key, textValue)));
    }

    private static LeafClassification classifyLeaf(String key, String textValue) {
        if (HttpContractRecorder.isSensitiveKey(key)) {
            return LeafClassification.SENSITIVE;
        }
        if (HttpContractRecorder.isVolatileKey(key) || HttpContractRecorder.isVolatileValue(textValue)) {
            return LeafClassification.VOLATILE;
        }
        return LeafClassification.STABLE;
    }
}
