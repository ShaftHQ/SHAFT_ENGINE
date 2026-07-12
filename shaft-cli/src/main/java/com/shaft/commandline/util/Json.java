package com.shaft.commandline.util;

import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

/**
 * Shared Jackson 3 mapper and small JSON helpers used across the CLI.
 */
public final class Json {

    /**
     * The single shared JSON mapper.
     */
    public static final JsonMapper MAPPER = JsonMapper.builder().build();

    private Json() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * @return a new empty JSON object node
     */
    public static ObjectNode newObject() {
        return MAPPER.createObjectNode();
    }
}
