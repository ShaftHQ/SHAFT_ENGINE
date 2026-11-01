package com.shaft.intellij.ui;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
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
}
