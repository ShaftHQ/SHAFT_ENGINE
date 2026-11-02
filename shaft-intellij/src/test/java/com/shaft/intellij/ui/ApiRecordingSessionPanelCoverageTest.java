package com.shaft.intellij.ui;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Extra ApiRecordingSessionPanel parser/table coverage without reflection.
 */
class ApiRecordingSessionPanelCoverageTest {

    @BeforeAll
    static void forceHeadless() {
        System.setProperty("java.awt.headless", "true");
    }

    @Test
    void parseTransactionsCoversEmptyArrayMalformedAndRichRows() {
        assertTrue(ApiRecordingSessionPanel.parseTransactions(null).isEmpty());
        assertTrue(ApiRecordingSessionPanel.parseTransactions("").isEmpty());
        assertTrue(ApiRecordingSessionPanel.parseTransactions("not-json").isEmpty());
        assertTrue(ApiRecordingSessionPanel.parseTransactions("{}").isEmpty());
        assertTrue(ApiRecordingSessionPanel.parseTransactions("[]").isEmpty());

        String rich = """
                [
                  {"transactionId":"t1","method":"GET","url":"https://api.example/v1/orders","statusCode":200,
                   "resourceKind":"xhr","bodyRefMetadata":{"response":{"sizeBytes":128}}},
                  {"transactionId":"t2","method":"POST","url":"https://api.example/v1/cart","statusCode":201,
                   "resourceKind":"fetch"},
                  {"transactionId":"t3"},
                  12,
                  null
                ]
                """;
        List<ApiRecordingSessionPanel.TransactionRow> rows = ApiRecordingSessionPanel.parseTransactions(rich);
        assertTrue(rows.size() >= 2);
        assertTrue(rows.stream().anyMatch(r -> "t1".equals(r.id())));
        assertTrue(rows.stream().anyMatch(r -> "POST".equals(r.method())));
    }

    @Test
    void parsePinnableLeavesCoversEmptyAndMixedPayloads() {
        assertTrue(ApiRecordingSessionPanel.parsePinnableLeaves(null).isEmpty());
        assertTrue(ApiRecordingSessionPanel.parsePinnableLeaves("[]").isEmpty());
        assertTrue(ApiRecordingSessionPanel.parsePinnableLeaves("nope").isEmpty());
        String leaves = """
                [
                  {"method":"GET","url":"https://api.example/orders","leaves":[
                    {"jsonPath":"$.id","value":"42","classification":"VOLATILE"},
                    {"jsonPath":"$.stable","value":"ok","classification":"STABLE"},
                    {"jsonPath":"$.blank","value":"","classification":"VOLATILE"}
                  ]},
                  {"method":"POST","url":"https://api.example/cart","leaves":[
                    {"jsonPath":"$.token","value":"abc","classification":"VOLATILE"}
                  ]},
                  5
                ]
                """;
        List<ApiRecordingSessionPanel.PinnableLeafRow> parsed = ApiRecordingSessionPanel.parsePinnableLeaves(leaves);
        assertFalse(parsed.isEmpty());
        assertTrue(parsed.stream().allMatch(leaf -> leaf.jsonPath() != null && !leaf.jsonPath().isBlank()));
        assertTrue(parsed.stream().anyMatch(leaf -> "$.id".equals(leaf.jsonPath())));
        assertTrue(parsed.stream().anyMatch(leaf -> "$.token".equals(leaf.jsonPath())));
        assertTrue(parsed.stream().noneMatch(leaf -> "$.stable".equals(leaf.jsonPath())));
    }

    @Test
    void assignActionRowMnemonicsAreDistinct() {
        JButton stop = new JButton("Stop");
        JButton pin = new JButton("Pin fields");
        JButton generate = new JButton("Generate");
        ApiRecordingSessionPanel.assignActionRowMnemonics(stop, pin, generate);
        assertAll(
                () -> assertTrue(stop.getMnemonic() != 0),
                () -> assertTrue(pin.getMnemonic() != 0),
                () -> assertTrue(generate.getMnemonic() != 0),
                () -> assertTrue(stop.getMnemonic() != pin.getMnemonic()),
                () -> assertTrue(pin.getMnemonic() != generate.getMnemonic()),
                () -> assertTrue(stop.getMnemonic() != generate.getMnemonic()));
    }

    @Test
    void transactionTableModelCoversColumnsAndIncludeToggle() {
        ApiRecordingSessionPanel.TransactionTableModel model = new ApiRecordingSessionPanel.TransactionTableModel();
        model.setRows(List.of(
                new ApiRecordingSessionPanel.TransactionRow("a", "GET", "https://a", "200", "xhr", "12", true),
                new ApiRecordingSessionPanel.TransactionRow("b", "POST", "https://b", "500", "fetch", "0", true),
                new ApiRecordingSessionPanel.TransactionRow("", "OPTIONS", "https://c", "", "other", "", true)));
        assertEquals(3, model.getRowCount());
        assertEquals(6, model.getColumnCount());
        assertEquals("Method", model.getColumnName(0));
        assertEquals("Include", model.getColumnName(ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN));
        assertEquals(Boolean.class, model.getColumnClass(ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN));
        assertEquals(String.class, model.getColumnClass(0));
        assertTrue(model.isCellEditable(0, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN));
        assertFalse(model.isCellEditable(0, 1));
        assertEquals("GET", model.getValueAt(0, 0));
        assertEquals("https://a", model.getValueAt(0, 1));
        assertEquals("200", model.getValueAt(0, 2));
        assertEquals("xhr", model.getValueAt(0, 3));
        assertEquals("12", model.getValueAt(0, 4));
        assertEquals(Boolean.TRUE, model.getValueAt(0, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN));
        assertEquals("", model.getValueAt(0, 99));
        model.setValueAt("ignore", 0, 1);
        model.setValueAt(false, 0, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN);
        model.setValueAt(false, 2, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN);
        assertEquals(List.of("a"), model.excludedTransactionIds());
        assertEquals(Boolean.FALSE, model.getValueAt(0, ApiRecordingSessionPanel.TransactionTableModel.INCLUDE_COLUMN));
    }

    @Test
    void parseTransactionsUnwrapsMcpContentEnvelopeAndBodyMetadataEdges() {
        String payload = """
                [
                  {"transactionId":"e1","method":"PUT","url":"https://api/x","statusCode":204,"resourceKind":"xhr",
                   "bodyRefMetadata":{"response":{"sizeBytes":null}}},
                  {"transactionId":"e2","method":"PATCH","url":"https://api/y","statusCode":0,
                   "bodyRefMetadata":{"response":{}}},
                  {"transactionId":"e3","method":"DELETE","url":"https://api/z","bodyRefMetadata":"not-object"}
                ]
                """;
        com.google.gson.JsonObject text = new com.google.gson.JsonObject();
        text.addProperty("type", "text");
        text.addProperty("text", payload);
        com.google.gson.JsonArray content = new com.google.gson.JsonArray();
        content.add(text);
        com.google.gson.JsonObject envelopeObj = new com.google.gson.JsonObject();
        envelopeObj.add("content", content);
        List<ApiRecordingSessionPanel.TransactionRow> rows =
                ApiRecordingSessionPanel.parseTransactions(envelopeObj.toString());
        assertTrue(rows.size() >= 3, "rows=" + rows);
        assertTrue(rows.stream().anyMatch(r -> "e1".equals(r.id()) && "PUT".equals(r.method())));
        assertTrue(rows.stream().anyMatch(r -> "e2".equals(r.id())));
        assertTrue(rows.stream().anyMatch(r -> "e3".equals(r.id()) && "DELETE".equals(r.method())));
    }

    @Test
    void pinnableLeafTableModelColumnAccessorsAndToggle() {
        ApiRecordingSessionPanel.PinnableLeafTableModel model = new ApiRecordingSessionPanel.PinnableLeafTableModel(
                List.of(
                        new ApiRecordingSessionPanel.PinnableLeafRow("GET", "https://a", "$.id", "1"),
                        new ApiRecordingSessionPanel.PinnableLeafRow("POST", "https://b", "$.tok", "x")),
                Set.of("$.tok"));
        assertEquals(2, model.getRowCount());
        assertEquals(4, model.getColumnCount());
        assertEquals("Pin", model.getColumnName(0));
        assertEquals("JSON Path", model.getColumnName(2));
        assertEquals(Boolean.class, model.getColumnClass(0));
        assertEquals(String.class, model.getColumnClass(2));
        assertTrue(model.isCellEditable(0, 0));
        assertFalse(model.isCellEditable(0, 2));
        assertEquals("GET https://a", model.getValueAt(0, 1));
        assertEquals("$.id", model.getValueAt(0, 2));
        assertEquals("1", model.getValueAt(0, 3));
        assertEquals("", model.getValueAt(0, 9));
        model.setValueAt("nope", 0, 2);
        model.setValueAt(true, 0, 0);
        model.setValueAt(false, 1, 0);
        assertEquals(Set.of("$.id"), model.pinnedJsonPaths());
    }

}
