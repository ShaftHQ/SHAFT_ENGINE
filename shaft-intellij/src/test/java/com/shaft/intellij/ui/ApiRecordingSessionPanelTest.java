package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the pure parsing/filtering logic behind {@link ApiRecordingSessionPanel} directly,
 * mirroring {@code PickLocatorAtCaretActionTest}'s style: the panel itself needs a live
 * {@code Project} (an {@code Alarm} and MCP polling start in its constructor), so it is not
 * unit-testable, but {@code parseTransactions} and {@code TransactionTableModel} are pure/local
 * state and testable in isolation.
 */
class ApiRecordingSessionPanelTest {
    @Test
    void parseTransactionsReadsTheBareArrayCapture_api_transactionsReturns() {
        // Regression for issue #3548 item 3's front-loaded finding: capture_api_transactions
        // returns a bare JSON array (List<NetworkTransaction>), not an object with a "transactions"
        // key. The old code looked for that key and always got an empty table.
        String output = mcpArrayText("""
                [
                  {"transactionId": "tx-1", "method": "GET", "url": "https://api.example.test/orders",
                   "statusCode": 200, "resourceKind": "XHR",
                   "bodyRefMetadata": {"response": {"sizeBytes": 128}}},
                  {"transactionId": "tx-2", "method": "DELETE", "url": "https://api.example.test/admin",
                   "statusCode": 204, "resourceKind": "FETCH"}
                ]
                """.strip());

        List<ApiRecordingSessionPanel.TransactionRow> rows = ApiRecordingSessionPanel.parseTransactions(output);

        assertEquals(2, rows.size());
        ApiRecordingSessionPanel.TransactionRow first = rows.get(0);
        assertEquals("tx-1", first.id());
        assertEquals("GET", first.method());
        assertEquals("https://api.example.test/orders", first.url());
        assertEquals("200", first.status());
        assertEquals("XHR", first.resourceKind());
        assertEquals("128", first.size());
        assertTrue(first.included());

        ApiRecordingSessionPanel.TransactionRow second = rows.get(1);
        assertEquals("tx-2", second.id());
        assertEquals("204", second.status());
        assertEquals("", second.size());
    }

    @Test
    void parseTransactionsReturnsEmptyForNonArrayOutput() {
        assertEquals(List.of(), ApiRecordingSessionPanel.parseTransactions(mcpArrayText("{\"state\": \"ACTIVE\"}")));
    }

    @Test
    void tableModelExcludedTransactionIdsReturnsOnlyUncheckedRows() {
        ApiRecordingSessionPanel.TransactionTableModel model = new ApiRecordingSessionPanel.TransactionTableModel();
        model.setRows(List.of(
                new ApiRecordingSessionPanel.TransactionRow("tx-1", "GET", "https://a", "200", "XHR", "10", true),
                new ApiRecordingSessionPanel.TransactionRow("tx-2", "POST", "https://b", "201", "FETCH", "20", true)));

        model.setValueAt(false, 1, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN);

        assertEquals(List.of("tx-2"), model.excludedTransactionIds());
    }

    @Test
    void tableModelSetRowsPreservesIncludedStateAcrossPollsByMatchingOnId() {
        // Regression: matching on id (not method+url) is what makes the include checkbox survive a
        // poll cycle now that the URL/status/size can legitimately repeat across transactions.
        ApiRecordingSessionPanel.TransactionTableModel model = new ApiRecordingSessionPanel.TransactionTableModel();
        model.setRows(List.of(
                new ApiRecordingSessionPanel.TransactionRow("tx-1", "GET", "https://a", "200", "XHR", "10", true)));
        model.setValueAt(false, 0, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN);

        // Next poll returns the same transaction again (still recording) plus a new one.
        model.setRows(List.of(
                new ApiRecordingSessionPanel.TransactionRow("tx-1", "GET", "https://a", "200", "XHR", "10", true),
                new ApiRecordingSessionPanel.TransactionRow("tx-2", "GET", "https://a", "200", "XHR", "10", true)));

        assertEquals(List.of("tx-1"), model.excludedTransactionIds());
    }

    private static String mcpArrayText(String text) {
        JsonObject item = new JsonObject();
        item.addProperty("type", "text");
        item.addProperty("text", text);
        JsonArray content = new JsonArray();
        content.add(item);
        JsonObject result = new JsonObject();
        result.add("content", content);
        result.addProperty("isError", false);
        return result.toString();
    }
}
