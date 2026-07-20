package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

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
    void actionRowMnemonicsAreNonConflictingAndAlReachable() {
        // Issue #3637: Stop/Pin Fields/Generate render together in the "actions" row, so their
        // mnemonics must be pairwise distinct. Exercised via the extracted static method rather
        // than the full panel, which needs a live Project to construct (see class javadoc).
        JButton stop = new JButton("Stop");
        JButton pinFields = new JButton("Pin Fields...");
        JButton generate = new JButton("Generate");

        ApiRecordingSessionPanel.assignActionRowMnemonics(stop, pinFields, generate);

        Set<Integer> mnemonics = Set.of(stop.getMnemonic(), pinFields.getMnemonic(), generate.getMnemonic());
        assertEquals(3, mnemonics.size());
        assertTrue(mnemonics.stream().noneMatch(mnemonic -> mnemonic == 0));
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

    @Test
    void webModePollsTheBrowserBasedRecorderTools() {
        assertEquals("capture_api_transactions", ApiRecordingSessionPanel.CaptureMode.WEB.transactionsTool());
        assertEquals("capture_api_stop", ApiRecordingSessionPanel.CaptureMode.WEB.stopTool());
    }

    @Test
    void pureApiModePollsTheNoBrowserMobileProxyTools() {
        // issue #3530 A2: both WEB and PURE_API session panels poll the same capture_api_* tools,
        // which internally dispatch to either the browser-based recorder or the loopback MITM proxy
        // based on active session context, sharing the same transaction-parsing/table-model logic above.
        assertEquals("capture_api_transactions", ApiRecordingSessionPanel.CaptureMode.PURE_API.transactionsTool());
        assertEquals("capture_api_stop", ApiRecordingSessionPanel.CaptureMode.PURE_API.stopTool());
    }

    @Test
    void parsePinnableLeavesReturnsOnlyVolatileNonBlankLeavesAcrossTransactions() {
        // issue #3530 negative-case: a pin can only affect a VOLATILE leaf (STABLE is already
        // asserted automatically; SENSITIVE and blank leaves are never asserted regardless).
        String output = mcpArrayText("""
                [
                  {"transactionId": "tx-1", "method": "GET", "url": "https://api.example.test/orders/42",
                   "leaves": [
                     {"jsonPath": "$.status", "key": "status", "value": "shipped", "classification": "STABLE"},
                     {"jsonPath": "$.orderId", "key": "orderId", "value": "uuid-1", "classification": "VOLATILE"},
                     {"jsonPath": "$.token", "key": "token", "value": "", "classification": "SENSITIVE"},
                     {"jsonPath": "$.note", "key": "note", "value": "", "classification": "VOLATILE"}
                   ]},
                  {"transactionId": "tx-2", "method": "DELETE", "url": "https://api.example.test/orders/42",
                   "leaves": [
                     {"jsonPath": "$.traceId", "key": "traceId", "value": "trace-99", "classification": "VOLATILE"}
                   ]}
                ]
                """.strip());

        List<ApiRecordingSessionPanel.PinnableLeafRow> rows = ApiRecordingSessionPanel.parsePinnableLeaves(output);

        assertEquals(2, rows.size());
        ApiRecordingSessionPanel.PinnableLeafRow first = rows.get(0);
        assertEquals("GET", first.method());
        assertEquals("https://api.example.test/orders/42", first.url());
        assertEquals("$.orderId", first.jsonPath());
        assertEquals("uuid-1", first.value());
        assertEquals("$.traceId", rows.get(1).jsonPath());
    }

    @Test
    void parsePinnableLeavesReturnsEmptyForNonArrayOutput() {
        assertEquals(List.of(),
                ApiRecordingSessionPanel.parsePinnableLeaves(mcpArrayText("{\"state\": \"ACTIVE\"}")));
    }

    @Test
    void pinnableLeafTableModelTracksCheckedJsonPathsAndStartsFromAPriorSelection() {
        ApiRecordingSessionPanel.PinnableLeafTableModel model = new ApiRecordingSessionPanel.PinnableLeafTableModel(
                List.of(
                        new ApiRecordingSessionPanel.PinnableLeafRow("GET", "https://a", "$.orderId", "uuid-1"),
                        new ApiRecordingSessionPanel.PinnableLeafRow("GET", "https://a", "$.note", "n/a")),
                Set.of("$.note"));

        assertEquals(Boolean.FALSE, model.getValueAt(0, ApiRecordingSessionPanel.PinnableLeafTableModel.PIN_COLUMN));
        assertEquals(Boolean.TRUE, model.getValueAt(1, ApiRecordingSessionPanel.PinnableLeafTableModel.PIN_COLUMN));
        assertEquals(Set.of("$.note"), model.pinnedJsonPaths());

        model.setValueAt(true, 0, ApiRecordingSessionPanel.PinnableLeafTableModel.PIN_COLUMN);
        model.setValueAt(false, 1, ApiRecordingSessionPanel.PinnableLeafTableModel.PIN_COLUMN);

        assertEquals(Set.of("$.orderId"), model.pinnedJsonPaths());
    }

    @Test
    void pinFieldsDialogCancelLeavesPinnedJsonPathsUnchanged() throws Exception {
        // issue #3635: DialogWrapper's default Cancel action (Esc) must stay a no-op, matching the
        // old hand-rolled JDialog's cancel button, which only called dialog.dispose(). DialogWrapper
        // asserts its constructor and actions run on the EDT, so this must too (unlike the panel's
        // other tests, which cover pure logic off the EDT).
        Set<String> pinnedJsonPaths = new LinkedHashSet<>(Set.of("$.existing"));
        ApiRecordingSessionPanel.PinnableLeafTableModel pickerModel = new ApiRecordingSessionPanel.PinnableLeafTableModel(
                List.of(new ApiRecordingSessionPanel.PinnableLeafRow("GET", "https://a", "$.orderId", "uuid-1")),
                Set.of());
        pickerModel.setValueAt(true, 0, ApiRecordingSessionPanel.PinnableLeafTableModel.PIN_COLUMN);

        javax.swing.SwingUtilities.invokeAndWait(() -> {
            ApiRecordingSessionPanel.PinFieldsDialog dialog =
                    new ApiRecordingSessionPanel.PinFieldsDialog(null, pickerModel, pinnedJsonPaths);
            dialog.doCancelAction();
        });

        assertEquals(Set.of("$.existing"), pinnedJsonPaths);
    }

    @Test
    void pinFieldsDialogOkReplacesPinnedJsonPathsWithThePickersSelections() throws Exception {
        // OK (DialogWrapper's default action, bound to Enter for free) must replace the panel's
        // pinned set with exactly what's checked in the picker model, same as the old dialog's OK
        // button did before pinnedJsonPaths.clear()/addAll().
        Set<String> pinnedJsonPaths = new LinkedHashSet<>(Set.of("$.stale"));
        ApiRecordingSessionPanel.PinnableLeafTableModel pickerModel = new ApiRecordingSessionPanel.PinnableLeafTableModel(
                List.of(
                        new ApiRecordingSessionPanel.PinnableLeafRow("GET", "https://a", "$.orderId", "uuid-1"),
                        new ApiRecordingSessionPanel.PinnableLeafRow("GET", "https://a", "$.note", "n/a")),
                Set.of());
        pickerModel.setValueAt(true, 0, ApiRecordingSessionPanel.PinnableLeafTableModel.PIN_COLUMN);

        javax.swing.SwingUtilities.invokeAndWait(() -> {
            ApiRecordingSessionPanel.PinFieldsDialog dialog =
                    new ApiRecordingSessionPanel.PinFieldsDialog(null, pickerModel, pinnedJsonPaths);
            dialog.doOKAction();
        });

        assertEquals(Set.of("$.orderId"), pinnedJsonPaths);
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
