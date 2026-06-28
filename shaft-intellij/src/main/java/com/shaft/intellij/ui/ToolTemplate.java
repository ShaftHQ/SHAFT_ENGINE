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

/**
 * Category of editable MCP tool invocation templates.
 *
 * @param label category label
 * @param templates templates in this category
 */
record ToolCategory(String label, java.util.List<ToolTemplate> templates) {
    @Override
    public String toString() {
        return label;
    }
}
