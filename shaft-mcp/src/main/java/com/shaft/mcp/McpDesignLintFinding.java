package com.shaft.mcp;

/**
 * One deterministic Gherkin lint finding (issue #5953).
 *
 * @param id      stable id such as {@code LINT-01}
 * @param level   {@code error} or {@code warning}
 * @param rule    machine rule name
 * @param message human-readable finding
 */
public record McpDesignLintFinding(String id, String level, String rule, String message) {
    public McpDesignLintFinding {
        id = id == null ? "" : id;
        level = level == null ? "error" : level;
        rule = rule == null ? "" : rule;
        message = message == null ? "" : message;
    }
}
