package com.shaft.capture.generate.api;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Finds {@code VOLATILE} response leaves that a later transaction's request reuses (the common
 * create -&gt; read/update case: a generated ID from a POST response reappears in a later GET's
 * URL, or a later PATCH's request body), and upgrades them to
 * {@link LeafClassification#CORRELATED} so codegen chains them through a variable
 * (e.g. {@code String createdOrderId = api.getResponseJSONValue("$.id");}) instead of asserting
 * or replaying a literal. Unmatched {@code VOLATILE} leaves are left as-is; codegen falls back to
 * the recorded literal for those.
 */
public final class TransactionCorrelator {

    /**
     * Volatile values shorter than this are never correlated, even on an exact substring match,
     * since short strings (e.g. a single-digit volatile-but-not-numeric field) risk matching
     * unrelated later request content by coincidence.
     */
    private static final int MIN_CORRELATABLE_VALUE_LENGTH = 3;

    private TransactionCorrelator() {
    }

    /**
     * Correlates volatile response values across transactions, in encounter (chronological) order.
     * Only later transactions (by list index, matching recording order) are searched, matching how
     * a value can only be reused after it was first observed.
     *
     * @param transactions transactions in recorded order
     * @return one {@link CorrelatedTransaction} per input transaction, in the same order
     */
    public static List<CorrelatedTransaction> correlate(List<ApiTransaction> transactions) {
        List<ApiTransaction> ordered = transactions == null ? List.of() : transactions;
        List<CorrelatedTransaction> results = new ArrayList<>(ordered.size());
        for (int i = 0; i < ordered.size(); i++) {
            ApiTransaction source = ordered.get(i);
            List<ResponseLeaf> resultLeaves = new ArrayList<>();
            Map<String, List<CorrelationUse>> usesByPath = new LinkedHashMap<>();
            for (ResponseLeaf leaf : source.responseLeaves()) {
                if (!isCorrelationCandidate(leaf)) {
                    resultLeaves.add(leaf);
                    continue;
                }
                List<CorrelationUse> uses = findLaterUses(leaf.value(), ordered, i + 1);
                if (uses.isEmpty()) {
                    resultLeaves.add(leaf);
                } else {
                    resultLeaves.add(leaf.asCorrelated());
                    usesByPath.put(leaf.jsonPath(), uses);
                }
            }
            results.add(new CorrelatedTransaction(source, resultLeaves, usesByPath));
        }
        return List.copyOf(results);
    }

    private static boolean isCorrelationCandidate(ResponseLeaf leaf) {
        return leaf.classification() == LeafClassification.VOLATILE
                && leaf.value().length() >= MIN_CORRELATABLE_VALUE_LENGTH;
    }

    private static List<CorrelationUse> findLaterUses(String value, List<ApiTransaction> transactions, int fromIndex) {
        List<CorrelationUse> uses = new ArrayList<>();
        for (int j = fromIndex; j < transactions.size(); j++) {
            ApiTransaction candidate = transactions.get(j);
            if (candidate.url().contains(value)) {
                uses.add(new CorrelationUse(candidate.transactionId(), CorrelationUse.Location.URL, ""));
            }
            for (Map.Entry<String, String> header : candidate.requestHeaders().entrySet()) {
                if (header.getValue() != null && header.getValue().contains(value)) {
                    uses.add(new CorrelationUse(
                            candidate.transactionId(), CorrelationUse.Location.REQUEST_HEADER, header.getKey()));
                }
            }
            if (!candidate.requestBody().isBlank() && candidate.requestBody().contains(value)) {
                uses.add(new CorrelationUse(candidate.transactionId(), CorrelationUse.Location.REQUEST_BODY, ""));
            }
        }
        return List.copyOf(uses);
    }
}
