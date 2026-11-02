package com.shaft.mcp;

/**
 * One parameter of a mechanically-dumped tool schema (design doc Decision 4 / amendment A5):
 * name, JSON-schema type, whether it is in the live schema's {@code required} array, and its
 * schema description when the {@code @Tool}/{@code @ToolParam} annotation supplied one.
 *
 * <p>Deliberately excludes a "default" value: the live JSON schema Spring AI generates carries no
 * default-value keyword for these tools (verified by inspection), and several defaults are
 * policy-driven at runtime (for example {@code capture_start}'s {@code headless} resolves from
 * {@code SHAFT.Properties}/{@code .shaft/recorder-policy.json}, not a static constant) -- exactly
 * the "policy-overridable defaults phrased conditionally" amendment A5 calls out. Documenting a
 * concrete default is therefore a curated-overlay concern, merged in by
 * {@code scripts/mcp/generate_tool_index.py}, never a mechanical one.
 */
public record ToolIndexParam(String name, String type, boolean required, String description) {
}
