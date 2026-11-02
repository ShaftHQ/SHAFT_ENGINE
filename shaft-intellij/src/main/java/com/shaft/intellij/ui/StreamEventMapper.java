package com.shaft.intellij.ui;

import com.google.gson.JsonObject;

/**
 * Translates one structured-stream NDJSON event object into a {@link MapResult}: what it means,
 * if anything, to a human reading the local-agent chat transcript. One implementation per CLI
 * protocol ({@link ClaudeStreamEventMapper}, {@link CodexStreamEventMapper}); {@link
 * StructuredStreamParser} owns state accumulation (answer, usage, files touched, permission
 * denials, plan proposal, structured question) and asks its mapper only "what does this JSON
 * object mean" -- the mapper never accumulates state itself, only reports back through {@code
 * StructuredStreamParser}'s package-private accumulator methods.
 */
interface StreamEventMapper {
    MapResult map(JsonObject event);
}
