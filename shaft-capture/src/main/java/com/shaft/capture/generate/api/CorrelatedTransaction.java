package com.shaft.capture.generate.api;

import java.util.List;
import java.util.Map;

/**
 * One transaction's response leaves after {@link TransactionCorrelator} has run, with any
 * {@code VOLATILE} leaf found reused by a later request upgraded to
 * {@link LeafClassification#CORRELATED}.
 *
 * @param transaction the source transaction, unchanged
 * @param leaves this transaction's response leaves, with correlated upgrades applied; unmatched
 *               volatile leaves are left as {@code VOLATILE} so codegen falls back to the
 *               recorded literal
 * @param usesByJsonPath for each {@code CORRELATED} leaf's {@code jsonPath}, every later request
 *                        location that reuses its value
 */
public record CorrelatedTransaction(
        ApiTransaction transaction,
        List<ResponseLeaf> leaves,
        Map<String, List<CorrelationUse>> usesByJsonPath) {
    public CorrelatedTransaction {
        leaves = leaves == null ? List.of() : List.copyOf(leaves);
        usesByJsonPath = usesByJsonPath == null ? Map.of() : Map.copyOf(usesByJsonPath);
    }
}
