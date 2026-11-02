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
     * @return whether the future accepted cancellation
     */
    public boolean cancel() {
        // Cancel before running cancelAction, same ordering constraint as kill(): future.cancel(true)
        // does not interrupt a supplyAsync worker thread, so cancelAction is still what actually drives
        // the graceful stop -- but running it first would let the worker's own eventual
        // `throw new CancellationException(...)` complete the future first, and CompletableFuture wraps
        // a thrown exception in a CompletionException that whenComplete hands to observers unwrapped
        // (unlike Future#get()). Every `error instanceof CancellationException` check downstream (e.g.
        // ShaftAssistantPanel#showAgentResult) would then never fire, rendering a soft cancel as
        // "Failed" instead (issue #3768). Cancelling first deterministically leaves the future holding
        // a bare CancellationException.
        boolean acknowledged = future.cancel(true);
        cancelAction.run();
        return acknowledged;
    }

    /**
     * Immediately stops the invocation.
     *
     * <p>Cancels {@link #future} <em>before</em> running {@code killAction}, not after: once
     * {@code killAction} destroys the process it unblocks the background run thread, which then
     * races to complete {@code future} itself by throwing its own {@code CancellationException}
     * (see {@code AssistantLocalAgentRunner#run}). {@code CompletableFuture} wraps an exception
     * thrown by an async-stage task in a {@code CompletionException} when it completes the future,
     * so if that background completion lands first, {@code future.cancel(true)} called afterward
     * finds the future already done and returns {@code false} -- and {@code future.isCancelled()}
     * also reads {@code false} thereafter, because the stored cause is that wrapping
     * {@code CompletionException}, not a bare {@code CancellationException}. Cancelling first closes
     * the window entirely: {@code future.cancel(true)} then always wins the race (the background
     * thread is still blocked, since {@code killAction} hasn't run yet), so the future is
     * deterministically left holding a bare {@code CancellationException}, and any later
     * completion attempt by the background thread is silently discarded as a no-op by the
     * already-completed future. Confirmed by amplifying this race under executor contention: it
     * failed in roughly half of 500 runs with the old ordering (issue #3758).
     *
     * @return whether the future accepted cancellation
     */
    public boolean kill() {
        boolean acknowledged = future.cancel(true);
        killAction.run();
        return acknowledged;
    }
}
