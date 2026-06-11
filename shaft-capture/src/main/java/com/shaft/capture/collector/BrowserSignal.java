package com.shaft.capture.collector;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.capture.format.CaptureFormatException;

import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Raw browser signal containing an immediate DOM snapshot but no persisted value.
 *
 * @param kind normalized low-level signal kind
 * @param timestamp signal time
 * @param browsingContextId BiDi or fallback browsing-context identifier
 * @param page safe page fields captured with the signal
 * @param target immediate element evidence
 * @param data transient action data awaiting privacy classification
 */
public record BrowserSignal(
        String kind,
        Instant timestamp,
        String browsingContextId,
        Map<String, Object> page,
        Map<String, Object> target,
        Map<String, Object> data) {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Creates an immutable signal.
     */
    public BrowserSignal {
        kind = kind == null ? "" : kind.trim().toLowerCase();
        timestamp = timestamp == null ? Instant.now() : timestamp;
        browsingContextId = browsingContextId == null ? "" : browsingContextId;
        page = copy(page);
        target = copy(target);
        data = copy(data);
    }

    /**
     * Parses a browser-side JSON signal.
     *
     * @param json signal JSON
     * @param browsingContextId source browsing context
     * @return parsed signal
     */
    public static BrowserSignal fromJson(String json, String browsingContextId) {
        try {
            Map<String, Object> payload = MAPPER.readValue(json, new TypeReference<>() {
            });
            long timestamp = longValue(payload.get("timestamp"), System.currentTimeMillis());
            return new BrowserSignal(
                    string(payload.get("kind")),
                    Instant.ofEpochMilli(timestamp),
                    browsingContextId,
                    map(payload.get("page")),
                    map(payload.get("target")),
                    map(payload.get("data")));
        } catch (JsonProcessingException exception) {
            throw new CaptureFormatException("Browser capture signal is malformed.", exception);
        }
    }

    /**
     * Creates a recorder-generated signal.
     *
     * @param kind signal kind
     * @param contextId browsing context
     * @param page page fields
     * @param data action fields
     * @return generated signal
     */
    public static BrowserSignal generated(
            String kind,
            String contextId,
            Map<String, Object> page,
            Map<String, Object> data) {
        return new BrowserSignal(kind, Instant.now(), contextId, page, Map.of(), data);
    }

    /**
     * Reads a string data field.
     *
     * @param key field name
     * @return field value
     */
    public String dataString(String key) {
        return string(data.get(key));
    }

    /**
     * Reads an integer data field.
     *
     * @param key field name
     * @param fallback fallback value
     * @return field value
     */
    public int dataInt(String key, int fallback) {
        return (int) longValue(data.get(key), fallback);
    }

    /**
     * Reads a boolean data field.
     *
     * @param key field name
     * @return field value
     */
    public boolean dataBoolean(String key) {
        Object value = data.get(key);
        return value instanceof Boolean bool ? bool : Boolean.parseBoolean(string(value));
    }

    /**
     * Reads a list data field as strings.
     *
     * @param key field name
     * @return string list
     */
    public List<String> dataStrings(String key) {
        Object value = data.get(key);
        if (!(value instanceof List<?> list)) {
            return List.of();
        }
        return list.stream().map(BrowserSignal::string).filter(item -> !item.isBlank()).toList();
    }

    private static Map<String, Object> copy(Map<String, Object> value) {
        return value == null ? Map.of() : Map.copyOf(new LinkedHashMap<>(value));
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> map(Object value) {
        if (!(value instanceof Map<?, ?> source)) {
            return Map.of();
        }
        Map<String, Object> result = new LinkedHashMap<>();
        source.forEach((key, item) -> result.put(string(key), item));
        return result;
    }

    private static String string(Object value) {
        return value == null ? "" : String.valueOf(value);
    }

    private static long longValue(Object value, long fallback) {
        if (value instanceof Number number) {
            return number.longValue();
        }
        try {
            return Long.parseLong(string(value));
        } catch (NumberFormatException ignored) {
            return fallback;
        }
    }
}
