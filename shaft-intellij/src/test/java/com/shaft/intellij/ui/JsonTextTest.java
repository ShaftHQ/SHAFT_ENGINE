package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class JsonTextTest {
    @Test
    void prettyPrintsJsonAndKeepsPlainText() {
        assertEquals("{\n  \"ok\": true\n}", JsonText.prettyOrOriginal("{\"ok\":true}"));
        assertEquals("[\n  \"a\",\n  \"b\"\n]", JsonText.prettyOrOriginal("[\"a\",\"b\"]"));
        assertEquals("\"plain text result\"", JsonText.prettyOrOriginal("\"plain text result\""));
        assertEquals("not json", JsonText.prettyOrOriginal("not json"));
    }

    @Test
    void validatesJsonObjectsOnly() {
        assertEquals("", JsonText.validateObject("{\"tool\":\"capture_start\"}"));
        assertFalse(JsonText.validateObject("[1,2]").isBlank());
        assertFalse(JsonText.validateObject("{").isBlank());
    }
}
