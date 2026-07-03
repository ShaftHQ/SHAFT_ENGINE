package com.shaft.intellij.mcp;

import java.util.concurrent.CompletableFuture;

/**
 * Cancellable SHAFT MCP invocation handle.
 *
 * @param future async MCP result
 * @param cancelAction action that requests a graceful stop when possible
 * @param killAction action that immediately stops the underlying process when possible
 */
public record ShaftMcpInvocation(CompletableFuture<ShaftMcpToolResult> future, Runnable cancelAction, Runnable killAction) {
    public ShaftMcpInvocation(CompletableFuture<ShaftMcpToolResult> future, Runnable cancelAction) {
        this(future, cancelAction, cancelAction);
    }

    /**
     * Requests invocation cancellation.
     *
     * @return whether the cancellation request was sent
     */
    public boolean cancel() {
        cancelAction.run();
        return true;
    }

    /**
     * Immediately stops the invocation.
     *
     * @return whether the future accepted cancellation
     */
    public boolean kill() {
        killAction.run();
        return future.cancel(true);
    }
}
