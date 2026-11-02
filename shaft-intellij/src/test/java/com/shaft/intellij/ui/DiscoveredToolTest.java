package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DiscoveredToolTest {
    @Test
    void defaultsContextTypesToAll() {
        DiscoveredTool tool = new DiscoveredTool("capture_start", "Start capture");
        assertEquals("capture_start", tool.name());
        assertEquals("Start capture", tool.description());
        assertEquals(Set.of("all"), tool.contextTypes());

        DiscoveredTool scoped = new DiscoveredTool("design_analyze", "Analyze", Set.of("design"));
        assertEquals(Set.of("design"), scoped.contextTypes());
    }
}
