package com.shaft.intellij.ui;

import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;

/**
 * Editable MCP tool invocation template.
 *
 * @param label UI label
 * @param toolName MCP tool name
 * @param arguments JSON argument template
 * @param description concise purpose and parameter hint
 * @param confirmationRequired whether the tool can change project/source state
 * @param contextTypes one or more stable context tags used for filtering
 */
record ToolTemplate(
        String label,
        String toolName,
        String arguments,
        String description,
        boolean confirmationRequired,
        Set<String> contextTypes) {
    ToolTemplate(String label, String toolName, String arguments, String description, boolean confirmationRequired) {
        this(label, toolName, arguments, description, confirmationRequired, contextTypes(toolName));
    }

    private static final Set<String> DEFAULT_CONTEXT_TYPES = Set.of("all");

    ToolTemplate(String label, String toolName, String arguments) {
        this(label, toolName, arguments, "", false, contextTypes(toolName));
    }

    ToolTemplate(String label, String toolName, String arguments, Set<String> contextTypes) {
        this(label, toolName, arguments, "", false, contextTypes);
    }

    @Override
    public String toString() {
        return label;
    }

    @Override
    public Set<String> contextTypes() {
        return ToolTemplate.sanitizeContextTypes(contextTypes);
    }

    private static Set<String> sanitizeContextTypes(Set<String> values) {
        if (values == null || values.isEmpty()) {
            return DEFAULT_CONTEXT_TYPES;
        }
        return values.stream().map(ToolTemplate::normalizeContext).collect(java.util.stream.Collectors.toCollection(java.util.LinkedHashSet::new));
    }

    private static Set<String> contextTypes(String toolName) {
        if (toolName == null || toolName.isBlank()) {
            return DEFAULT_CONTEXT_TYPES;
        }
        String name = toolName.toLowerCase(Locale.ROOT);
        Set<String> contexts = new LinkedHashSet<>();
        contexts.add("all");

        if (name.contains("mobile_")) {
            contexts.add("mobile");
            if (name.contains("tap") || name.contains("swipe") || name.contains("type")
                    || name.contains("long_tap") || name.contains("drag") || name.contains("scroll")) {
                contexts.add("mobile-element-action");
            } else if (name.contains("initialize") || name.contains("status") || name.contains("record")) {
                contexts.add("mobile-session-management");
            } else {
                contexts.add("mobile-action");
            }
        }

        if (name.contains("browser")) {
            contexts.add("browser");
            if (name.contains("take_screenshot") || name.contains("get_page_dom")
                    || name.contains("open_intent") || name.contains("open_locator")
                    || name.contains("navigate") || name.contains("driver") && name.contains("click")) {
                contexts.add("browser-action");
            }
            if (name.contains("record") || name.contains("initialize") || name.contains("status")
                    || name.contains("open_intent")) {
                contexts.add("browser-session-management");
            }
        }

        if (name.contains("element_") || name.contains("element")) {
            contexts.add("element-action");
            if (name.startsWith("mobile_")) {
                contexts.add("mobile-element-action");
            }
        }

        if (name.contains("code")) {
            contexts.add("codegen");
        }
        if (name.contains("trace")) {
            contexts.add("traces");
        }
        if (name.contains("doctor") || name.contains("healer") || name.contains("sugg") || name.contains("autobot")) {
            contexts.add("assistant");
            contexts.add("diagnostics");
        }
        if (name.contains("project")) {
            contexts.add("projects");
            contexts.add("assistant");
        }
        if (name.contains("guide") || name.contains("scenario")) {
            contexts.add("assistant");
        }
        if (name.contains("capture") || name.contains("replay")) {
            contexts.add("playback");
        }

        return sanitizeContextTypes(contexts);
    }

    private static String normalizeContext(String value) {
        return value == null || value.isBlank()
                ? "all"
                : value.trim().toLowerCase(Locale.ROOT).replace(' ', '-');
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
