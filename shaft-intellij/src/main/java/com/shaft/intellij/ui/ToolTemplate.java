package com.shaft.intellij.ui;

/**
 * Editable MCP tool invocation template.
 *
 * @param label UI label
 * @param toolName MCP tool name
 * @param arguments JSON argument template
 */
record ToolTemplate(String label, String toolName, String arguments) {
    @Override
    public String toString() {
        return label;
    }
}
