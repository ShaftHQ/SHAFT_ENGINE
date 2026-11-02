package com.shaft.capture.model;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.ObjectMapper;

import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class NetworkEventTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void roundTripsNetworkEventThroughJacksonCodec() {
        CaptureEvent.NetworkEvent event = networkEvent();

        String serialized = JSON.writeValueAsString(event);
        CaptureEvent deserialized = JSON.readValue(serialized, CaptureEvent.class);

        assertTrue(serialized.contains("\"type\":\"network\""));
        assertEquals(event, deserialized);
        assertTrue(deserialized instanceof CaptureEvent.NetworkEvent);
    }

    @Test
    void uiOnlyEventListRoundTripsUnchanged() {
        List<CaptureEvent> events = CaptureFixtures.allEvents();
        var listType = JSON.getTypeFactory().constructCollectionType(List.class, CaptureEvent.class);

        String serialized = JSON.writerFor(listType).writeValueAsString(events);
        List<CaptureEvent> deserialized = JSON.readValue(serialized, listType);

        assertEquals(events, deserialized);
        assertTrue(serialized.contains("\"type\":\"click\""));
        assertFalse(serialized.contains("\"type\":\"network\""));
    }

    @Test
    void defaultsResourceKindToOtherAndNormalizesNulls() {
        CaptureEvent.NetworkEvent event = new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "txn-1",
                null,
                new HttpRequestRecord("GET", "https://example.test/api", Map.of(), null),
                null,
                null,
                null,
                null,
                null);

        assertEquals(ResourceKind.OTHER, event.resourceKind());
        assertEquals("", event.failureReason());
        assertEquals("", event.initiatorPageUrl());
        assertNull(event.response());
        assertNull(event.timing());
        assertNull(event.correlatedUiSequence());
    }

    @Test
    void requiresContextTransactionIdAndRequest() {
        assertThrows(IllegalArgumentException.class, () -> new CaptureEvent.NetworkEvent(
                null,
                "txn-1",
                ResourceKind.FETCH,
                new HttpRequestRecord("GET", "https://example.test/api", Map.of(), null),
                null,
                null,
                null,
                null,
                null));

        assertThrows(IllegalArgumentException.class, () -> new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "   ",
                ResourceKind.FETCH,
                new HttpRequestRecord("GET", "https://example.test/api", Map.of(), null),
                null,
                null,
                null,
                null,
                null));

        assertThrows(IllegalArgumentException.class, () -> new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "txn-1",
                ResourceKind.FETCH,
                null,
                null,
                null,
                null,
                null,
                null));
    }

    @Test
    void bodyRefHasExactlyTheSpecifiedFieldsAndNoBodyContent() {
        BodyRef ref = new BodyRef("evidence/network/txn-1-response.bin", "abc123", 42L, "gzip", true);

        assertEquals("evidence/network/txn-1-response.bin", ref.ref());
        assertEquals("abc123", ref.sha256());
        assertEquals(42L, ref.sizeBytes());
        assertEquals("gzip", ref.encoding());
        assertTrue(ref.truncated());

        List<String> recordComponentNames = java.util.Arrays.stream(BodyRef.class.getRecordComponents())
                .map(java.lang.reflect.RecordComponent::getName)
                .toList();
        assertEquals(List.of("ref", "sha256", "sizeBytes", "encoding", "truncated"), recordComponentNames);
        for (java.lang.reflect.RecordComponent component : BodyRef.class.getRecordComponents()) {
            assertFalse(component.getType().equals(byte[].class),
                    "BodyRef must not carry a byte[] field: " + component.getName());
        }
    }

    private static CaptureEvent.NetworkEvent networkEvent() {
        return new CaptureEvent.NetworkEvent(
                CaptureFixtures.context(1),
                "txn-42",
                ResourceKind.XHR,
                new HttpRequestRecord(
                        "post",
                        "https://example.test/api/orders",
                        Map.of("Content-Type", "application/json"),
                        new BodyRef("evidence/network/txn-42-request.bin", "req-sha", 128L, "identity", false)),
                new HttpResponseRecord(
                        200,
                        Map.of("Content-Type", "application/json"),
                        new BodyRef("evidence/network/txn-42-response.bin", "res-sha", 256L, "identity", false)),
                new NetworkTiming(
                        Duration.ofMillis(5),
                        Duration.ofMillis(10),
                        Duration.ofMillis(15),
                        Duration.ofMillis(20),
                        Duration.ofMillis(60),
                        Duration.ofMillis(120)),
                "",
                "https://example.test/checkout",
                14L);
    }
}
