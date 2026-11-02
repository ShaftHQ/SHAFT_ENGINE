package com.shaft.capture.generate.api;

import java.util.List;
import java.util.Map;

/**
 * Result of rendering one API test class.
 *
 * @param source generated Java source
 * @param testDataArtifacts test-data-relative path (e.g. {@code "api-capture/tx-2-schema.json"})
 *                          to file content, for the caller to write under
 *                          {@code src/test/resources/test-data/}
 * @param skippedTransactionIds transaction IDs that were recorded but not rendered, because
 *                              {@code SHAFT.API} has no request builder for their HTTP method
 */
public record RenderedApiTest(String source, Map<String, String> testDataArtifacts, List<String> skippedTransactionIds) {
    public RenderedApiTest {
        source = source == null ? "" : source;
        testDataArtifacts = testDataArtifacts == null ? Map.of() : Map.copyOf(testDataArtifacts);
        skippedTransactionIds = skippedTransactionIds == null ? List.of() : List.copyOf(skippedTransactionIds);
    }
}
