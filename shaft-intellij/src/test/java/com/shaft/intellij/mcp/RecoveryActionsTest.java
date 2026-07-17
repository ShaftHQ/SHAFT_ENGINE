package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Pins {@link RecoveryActions#forCategory(McpInvocationError)}'s mapping from every
 * {@link McpInvocationError} category to the recovery {@link RecoveryActions.Kind} the UI must
 * offer: a retryable failure gets a "Retry" button, a dead/unreachable server gets "Restart MCP
 * server", and a tool-level error (the server is fine, the call itself failed) points at the logs.
 */
class RecoveryActionsTest {

    @Test
    void timeoutMapsToRetry() {
        assertEquals(RecoveryActions.Kind.RETRY, RecoveryActions.forCategory(McpInvocationError.TIMEOUT));
    }

    @Test
    void malformedResponseMapsToRetry() {
        assertEquals(RecoveryActions.Kind.RETRY, RecoveryActions.forCategory(McpInvocationError.MALFORMED_RESPONSE));
    }

    @Test
    void processExitedMapsToRestart() {
        assertEquals(RecoveryActions.Kind.RESTART, RecoveryActions.forCategory(McpInvocationError.PROCESS_EXITED));
    }

    @Test
    void connectionLostMapsToRestart() {
        assertEquals(RecoveryActions.Kind.RESTART, RecoveryActions.forCategory(McpInvocationError.CONNECTION_LOST));
    }

    @Test
    void toolErrorMapsToViewLogs() {
        assertEquals(RecoveryActions.Kind.VIEW_LOGS, RecoveryActions.forCategory(McpInvocationError.TOOL_ERROR));
    }
}
