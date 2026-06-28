package com.shaft.intellij.ui;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParser;

/**
 * Small JSON text helpers for the SHAFT tool window.
 */
final class JsonText {
    private static final Gson PRETTY = new GsonBuilder().setPrettyPrinting().create();

    private JsonText() {
        throw new IllegalStateException("Utility class");
    }

    static String prettyOrOriginal(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        try {
            return PRETTY.toJson(JsonParser.parseString(text));
        } catch (RuntimeException exception) {
            return text;
        }
    }

    static String validateObject(String text) {
        try {
            JsonParser.parseString(text == null || text.isBlank() ? "{}" : text).getAsJsonObject();
            return "";
        } catch (RuntimeException exception) {
            return exception.getMessage();
        }
    }
}
