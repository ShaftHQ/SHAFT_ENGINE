package com.shaft.intellij.ui;

/**
 * Editable MCP tool invocation template.
 *
 * @param label UI label
 * @param toolName MCP tool name
 * @param arguments JSON argument template
 * @param description concise purpose and parameter hint
 * @param confirmationRequired whether the tool can change project/source state
 */
record ToolTemplate(
        String label,
        String toolName,
        String arguments,
        String description,
        boolean confirmationRequired) {
    ToolTemplate(String label, String toolName, String arguments) {
        this(label, toolName, arguments, "", false);
    }

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
