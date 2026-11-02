package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class StreamJsonTest {

    @Test
    void parseObjectAcceptsObjectsAndRejectsNoise() {
        assertNull(StreamJson.parseObject("not-json"));
        assertNull(StreamJson.parseObject("[1,2]"));
        assertNull(StreamJson.parseObject("{"));
        JsonObject object = StreamJson.parseObject("{\"type\":\"turn.started\"}");
        assertEquals("turn.started", StreamJson.stringField(object, "type"));
    }

    @Test
    void fieldReadersAreDefensiveAboutMissingAndWrongTypes() {
        JsonObject object = JsonParser.parseString(
                "{\"s\":\"ok\",\"n\":3,\"b\":true,\"o\":{\"k\":1},\"arr\":[1],\"badNum\":true}").getAsJsonObject();
        assertEquals("ok", StreamJson.stringField(object, "s"));
        assertNull(StreamJson.stringField(object, "n"));
        assertNull(StreamJson.stringField(null, "s"));
        assertTrue(StreamJson.booleanField(object, "b"));
        assertFalse(StreamJson.booleanField(object, "s"));
        assertEquals(3, StreamJson.intField(object, "n"));
        assertNull(StreamJson.intField(object, "badNum"));
        assertNull(StreamJson.intField(object, "missing"));
        assertEquals(1, StreamJson.objectField(object, "o").get("k").getAsInt());
        assertNull(StreamJson.objectField(object, "arr"));
        assertEquals(0, StreamJson.orZero(null));
        assertEquals(7, StreamJson.orZero(7));
        assertEquals("a", StreamJson.firstNonBlank(" ", null, "a", "b"));
        assertNull(StreamJson.firstNonBlank(" ", null, ""));
        assertEquals(2, StreamJson.firstNonNull(null, 2));
        assertEquals(1, StreamJson.firstNonNull(1, 2));
    }

    @Test
    void toolInputSummaryPrefersKnownKeysAndCollapsesWhitespace() {
        assertNull(StreamJson.toolInputSummary(null));
        JsonObject empty = JsonParser.parseString("{\"other\":\"x\"}").getAsJsonObject();
        assertNull(StreamJson.toolInputSummary(empty));
        JsonObject command = JsonParser.parseString(
                "{\"description\":\"ignore\",\"command\":\"ls\\n  -la\"}").getAsJsonObject();
        assertEquals("ls -la", StreamJson.toolInputSummary(command));
        JsonObject path = JsonParser.parseString("{\"path\":\"/tmp/a\"}").getAsJsonObject();
        assertEquals("/tmp/a", StreamJson.toolInputSummary(path));
    }
}
