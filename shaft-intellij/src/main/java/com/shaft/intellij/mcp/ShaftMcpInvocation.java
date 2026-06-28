package com.shaft.intellij.mcp;

import java.util.concurrent.CompletableFuture;

/**
 * Cancellable SHAFT MCP invocation handle.
 *
 * @param future async MCP result
 * @param cancelAction action that stops the underlying process when possible
 */
public record ShaftMcpInvocation(CompletableFuture<ShaftMcpToolResult> future, Runnable cancelAction) {
    /**
     * Cancels the invocation and closes the active MCP process when it has started.
     *
     * @return whether the future accepted cancellation
     */
    public boolean cancel() {
        cancelAction.run();
        return future.cancel(true);
    }
}
