package com.shaft.mcp;

import java.util.List;

/**
 * Network transaction listing returned by {@code browser_network_requests}.
 *
 * @param transactions matched transactions, bounded by the requested limit
 * @param totalMatched total transactions matching the filter before the limit was applied
 * @param truncated    whether {@code totalMatched} exceeded the requested limit
 * @param warnings     safe warnings, such as no transactions observed yet
 * @param detail       full detail (headers and a truncated, redacted response body preview) for the
 *                     single transaction matching the requested {@code id}; {@code null} unless an
 *                     {@code id} was supplied and matched exactly one transaction. Absorbs the former
 *                     {@code browser_network_request} tool (design doc Decision 2).
 */
public record McpNetworkTransactionList(
        List<McpNetworkTransaction> transactions,
        int totalMatched,
        boolean truncated,
        List<String> warnings,
        McpNetworkTransactionDetail detail) {
}
