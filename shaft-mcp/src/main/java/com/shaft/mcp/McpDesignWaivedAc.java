package com.shaft.mcp;

/**
 * One waived acceptance criterion with a recorded reason (issue #5952).
 *
 * @param id     AC identifier such as {@code AC-03}
 * @param reason non-blank residual-risk reason
 */
public record McpDesignWaivedAc(String id, String reason) {
    public McpDesignWaivedAc {
        id = id == null ? "" : id;
        reason = reason == null ? "" : reason;
    }
}
