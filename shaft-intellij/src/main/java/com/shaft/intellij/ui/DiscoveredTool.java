package com.shaft.intellij.ui;

import java.util.Set;

/**
 * Lightweight MCP tool from a discovered {@code tools/list} response.
 */
record DiscoveredTool(String name, String description, Set<String> contextTypes) {
    DiscoveredTool(String name, String description) {
        this(name, description, Set.of("all"));
    }
}
