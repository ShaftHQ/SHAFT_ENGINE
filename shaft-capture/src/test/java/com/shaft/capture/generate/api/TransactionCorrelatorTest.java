package com.shaft.capture.generate.api;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TransactionCorrelatorTest {

    private static final String CREATED_ID = "3fa85f64-5717-4562-b3fc-2c963f66afa6";

    @Test
    void createdIdReusedInLaterUrlIsUpgradedToCorrelated() {
        ApiTransaction createOrder = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\"}");
        ApiTransaction getOrder = transaction("tx-2", "GET",
                "https://api.example.test/orders/" + CREATED_ID,
                Map.of(), "", 200, Map.of(), "{\"status\":\"active\"}");

        List<CorrelatedTransaction> result = TransactionCorrelator.correlate(List.of(createOrder, getOrder));

        assertEquals(2, result.size());
        ResponseLeaf idLeaf = leaf(result.get(0), "$.id");
        assertEquals(LeafClassification.CORRELATED, idLeaf.classification());
        List<CorrelationUse> uses = result.get(0).usesByJsonPath().get("$.id");
        assertEquals(1, uses.size());
        assertEquals("tx-2", uses.get(0).transactionId());
        assertEquals(CorrelationUse.Location.URL, uses.get(0).location());
    }

    @Test
    void createdIdReusedInLaterRequestBodyIsUpgradedToCorrelated() {
        ApiTransaction createOrder = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\"}");
        ApiTransaction patchOrder = transaction("tx-2", "PATCH", "https://api.example.test/orders/current",
                Map.of(), "{\"orderId\":\"" + CREATED_ID + "\"}", 204, Map.of(), "");

        List<CorrelatedTransaction> result = TransactionCorrelator.correlate(List.of(createOrder, patchOrder));

        ResponseLeaf idLeaf = leaf(result.get(0), "$.id");
        assertEquals(LeafClassification.CORRELATED, idLeaf.classification());
        assertEquals(CorrelationUse.Location.REQUEST_BODY,
                result.get(0).usesByJsonPath().get("$.id").get(0).location());
    }

    @Test
    void createdIdReusedInLaterRequestHeaderIsUpgradedToCorrelated() {
        // "referenceId" is neither a configured sensitive nor a configured volatile key name --
        // this exercises value-based (UUID-shaped) volatility detection, not key-based.
        ApiTransaction createSession = transaction("tx-1", "POST", "https://api.example.test/sessions",
                Map.of(), "", 201, Map.of(), "{\"referenceId\":\"" + CREATED_ID + "\"}");
        ApiTransaction useSession = transaction("tx-2", "GET", "https://api.example.test/me",
                Map.of("x-reference", CREATED_ID), "", 200, Map.of(), "{}");

        List<CorrelatedTransaction> result = TransactionCorrelator.correlate(List.of(createSession, useSession));

        ResponseLeaf referenceLeaf = leaf(result.get(0), "$.referenceId");
        assertEquals(LeafClassification.CORRELATED, referenceLeaf.classification());
        CorrelationUse use = result.get(0).usesByJsonPath().get("$.referenceId").get(0);
        assertEquals(CorrelationUse.Location.REQUEST_HEADER, use.location());
        assertEquals("x-reference", use.detail());
    }

    @Test
    void unmatchedVolatileValueFallsBackToLiteralAndIsNotUpgraded() {
        ApiTransaction createOrder = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\"}");
        ApiTransaction unrelated = transaction("tx-2", "GET", "https://api.example.test/health",
                Map.of(), "", 200, Map.of(), "{\"status\":\"ok\"}");

        List<CorrelatedTransaction> result = TransactionCorrelator.correlate(List.of(createOrder, unrelated));

        ResponseLeaf idLeaf = leaf(result.get(0), "$.id");
        assertEquals(LeafClassification.VOLATILE, idLeaf.classification());
        assertTrue(result.get(0).usesByJsonPath().isEmpty());
    }

    @Test
    void stableAndSensitiveLeavesAreNeverCorrelatedRegardlessOfReuse() {
        // A stable status string that happens to reappear verbatim elsewhere must never be
        // "correlated" -- correlation only ever upgrades VOLATILE leaves.
        ApiTransaction first = transaction("tx-1", "GET", "https://api.example.test/a",
                Map.of(), "", 200, Map.of(), "{\"status\":\"processing-state\"}");
        ApiTransaction second = transaction("tx-2", "GET",
                "https://api.example.test/b?state=processing-state", Map.of(), "", 200, Map.of(), "{}");

        List<CorrelatedTransaction> result = TransactionCorrelator.correlate(List.of(first, second));

        assertEquals(LeafClassification.STABLE, leaf(result.get(0), "$.status").classification());
    }

    @Test
    void onlyLaterTransactionsAreSearchedNeverEarlierOnes() {
        // The same volatile value appearing in an EARLIER transaction's request must not count --
        // reuse must be causally forward (create -> read), never backward.
        ApiTransaction earlierUnrelated = transaction("tx-0", "GET",
                "https://api.example.test/lookup/" + CREATED_ID, Map.of(), "", 200, Map.of(), "{}");
        ApiTransaction createOrder = transaction("tx-1", "POST", "https://api.example.test/orders",
                Map.of(), "", 201, Map.of(), "{\"id\":\"" + CREATED_ID + "\"}");

        List<CorrelatedTransaction> result = TransactionCorrelator.correlate(List.of(earlierUnrelated, createOrder));

        ResponseLeaf idLeaf = leaf(result.get(1), "$.id");
        assertEquals(LeafClassification.VOLATILE, idLeaf.classification());
    }

    private static ResponseLeaf leaf(CorrelatedTransaction transaction, String jsonPath) {
        return transaction.leaves().stream()
                .filter(l -> l.jsonPath().equals(jsonPath))
                .findFirst()
                .orElseThrow(() -> new AssertionError("No leaf at " + jsonPath));
    }

    private static ApiTransaction transaction(
            String id, String method, String url, Map<String, String> requestHeaders, String requestBody,
            int statusCode, Map<String, String> responseHeaders, String responseBody) {
        return new ApiTransaction(id, method, url, "https://api.example.test", requestHeaders, requestBody,
                statusCode, responseHeaders, responseBody, ResponseNormalizer.classify(responseBody));
    }
}
