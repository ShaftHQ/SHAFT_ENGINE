package com.shaft.mcp;

import java.util.List;

/**
 * Network transaction listing returned by {@code browser_network_requests}.
 *
 * @param transactions matched transactions, bounded by the requested limit
 * @param totalMatched total transactions matching the filter before the limit was applied
 * @param truncated    whether {@code totalMatched} exceeded the requested limit
 * @param warnings     safe warnings, such as no transactions observed yet
 */
public record McpNetworkTransactionList(
        List<McpNetworkTransaction> transactions,
        int totalMatched,
        boolean truncated,
        List<String> warnings) {
}
