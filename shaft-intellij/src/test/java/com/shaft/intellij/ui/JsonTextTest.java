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

    @Test
    void findErrorLocationReturnsNullForValidJson() {
        JsonText.JsonErrorLocation location = JsonText.findErrorLocation("{}");
        assertEquals(null, location);
    }

    @Test
    void findErrorLocationReturnsNullForBlankJson() {
        JsonText.JsonErrorLocation location = JsonText.findErrorLocation("");
        assertEquals(null, location);
    }

    @Test
    void findErrorLocationDetectsInvalidJson() {
        JsonText.JsonErrorLocation location = JsonText.findErrorLocation("{invalid}");
        assertFalse(location == null);
    }

    @Test
    void errorLocationFormatsToString() {
        JsonText.JsonErrorLocation location = new JsonText.JsonErrorLocation(3, 15);
        assertEquals("line 3, column 15", location.toString());
    }
}
