package com.shaft.capture.generate.api;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ResponseNormalizerTest {

    @Test
    void blankOrMalformedJsonYieldsNoLeaves() {
        assertTrue(ResponseNormalizer.classify(null).isEmpty());
        assertTrue(ResponseNormalizer.classify("").isEmpty());
        assertTrue(ResponseNormalizer.classify("   ").isEmpty());
        assertTrue(ResponseNormalizer.classify("not json").isEmpty());
    }

    @Test
    void stableScalarLeafIsClassifiedStable() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify("{\"status\":\"active\"}");
        assertEquals(1, leaves.size());
        assertEquals("$.status", leaves.get(0).jsonPath());
        assertEquals("active", leaves.get(0).value());
        assertEquals(LeafClassification.STABLE, leaves.get(0).classification());
    }

    @Test
    void uuidValueIsClassifiedVolatileRegardlessOfKeyName() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify(
                "{\"randomField\":\"3fa85f64-5717-4562-b3fc-2c963f66afa6\"}");
        assertEquals(LeafClassification.VOLATILE, byPath(leaves, "$.randomField").classification());
    }

    @Test
    void instantValueIsClassifiedVolatile() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify("{\"randomField\":\"2026-07-06T12:00:00Z\"}");
        assertEquals(LeafClassification.VOLATILE, byPath(leaves, "$.randomField").classification());
    }

    @Test
    void configuredVolatileKeyNameIsClassifiedVolatileEvenWithStableValue() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify("{\"createdAt\":\"not-a-timestamp\"}");
        assertEquals(LeafClassification.VOLATILE, byPath(leaves, "$.createdAt").classification());
    }

    @Test
    void configuredSensitiveKeyNameIsClassifiedSensitive() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify("{\"password\":\"hunter2\"}");
        assertEquals(LeafClassification.SENSITIVE, byPath(leaves, "$.password").classification());
    }

    @Test
    void sensitiveTakesPriorityOverVolatileWhenBothWouldMatch() {
        // A password field whose value happens to look like a UUID must still be SENSITIVE,
        // never VOLATILE -- sensitivity is the stronger guarantee and must never be downgraded.
        List<ResponseLeaf> leaves = ResponseNormalizer.classify(
                "{\"apiKey\":\"3fa85f64-5717-4562-b3fc-2c963f66afa6\"}");
        assertEquals(LeafClassification.SENSITIVE, byPath(leaves, "$.apiKey").classification());
    }

    @Test
    void nestedObjectsAndArraysAreWalkedWithJsonPathPointers() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify(
                "{\"user\":{\"name\":\"Ada\"},\"items\":[{\"sku\":\"A1\"},{\"sku\":\"A2\"}]}");
        assertEquals("Ada", byPath(leaves, "$.user.name").value());
        assertEquals("A1", byPath(leaves, "$.items[0].sku").value());
        assertEquals("A2", byPath(leaves, "$.items[1].sku").value());
    }

    @Test
    void nullLeavesAreSkippedNotClassified() {
        List<ResponseLeaf> leaves = ResponseNormalizer.classify("{\"middleName\":null,\"status\":\"active\"}");
        assertEquals(1, leaves.size());
        assertEquals("status", leaves.get(0).key());
    }

    private static ResponseLeaf byPath(List<ResponseLeaf> leaves, String path) {
        return leaves.stream()
                .filter(leaf -> leaf.jsonPath().equals(path))
                .findFirst()
                .orElseThrow(() -> new AssertionError("No leaf found at path " + path + " in " + leaves));
    }
}
