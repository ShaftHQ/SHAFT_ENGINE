package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.UncheckedIOException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class McpInvocationErrorTest {
    @Test
    void categorizesTimeoutException() {
        IOException timeoutException = new IOException("Timed out waiting for SHAFT MCP response.");
        McpInvocationError category = McpInvocationError.categorize(timeoutException);
        assertEquals(McpInvocationError.TIMEOUT, category);
        assertEquals("Request timed out waiting for MCP response.", category.message());
        assertEquals("Retry", category.recoveryAction());
    }

    @Test
    void categorizesProcessExitException() {
        IOException exitException = new IOException("Process exited with code; process exit code: 1");
        McpInvocationError category = McpInvocationError.categorize(exitException);
        assertEquals(McpInvocationError.PROCESS_EXITED, category);
        assertEquals("MCP server process exited.", category.message());
        assertEquals("Restart MCP server", category.recoveryAction());
    }

    @Test
    void categorizesToolErrorResponse() {
        IOException toolErrorException = new IOException("SHAFT MCP returned an error response.");
        McpInvocationError category = McpInvocationError.categorize(toolErrorException);
        assertEquals(McpInvocationError.TOOL_ERROR, category);
        assertEquals("MCP tool error response.", category.message());
        assertEquals("View logs", category.recoveryAction());
    }

    @Test
    void categorizesConnectionLostException() {
        UncheckedIOException connectionException = new UncheckedIOException(new IOException("Broken pipe"));
        McpInvocationError category = McpInvocationError.categorize(connectionException);
        assertEquals(McpInvocationError.CONNECTION_LOST, category);
        assertEquals("Connection to MCP server lost.", category.message());
        assertEquals("Restart MCP server", category.recoveryAction());
    }

    @Test
    void categorizesReadFailureAsConnectionLost() {
        IOException readException = new IOException("Failed to read SHAFT MCP response.");
        McpInvocationError category = McpInvocationError.categorize(readException);
        assertEquals(McpInvocationError.CONNECTION_LOST, category);
    }

    @Test
    void categorizesInterruptedAsConnectionLost() {
        IOException interruptedException = new IOException("Interrupted while waiting for SHAFT MCP response.");
        McpInvocationError category = McpInvocationError.categorize(interruptedException);
        assertEquals(McpInvocationError.CONNECTION_LOST, category);
    }

    @Test
    void defaultsToToolErrorForUnknownExceptions() {
        IOException unknownException = new IOException("Some other error");
        McpInvocationError category = McpInvocationError.categorize(unknownException);
        assertEquals(McpInvocationError.TOOL_ERROR, category);
    }

    @Test
    void categorizesNullAsToolError() {
        McpInvocationError category = McpInvocationError.categorize(null);
        assertEquals(McpInvocationError.TOOL_ERROR, category);
    }

    @Test
    void categorizesComposedTimeoutAndProcessExitAsProcessExited() {
        // When the process dies mid-call, ShaftMcpStdioClient.requestTimedOut() composes
        // a message with both the timeout phrase and the process exit code detail.
        // This test ensures that PROCESS_EXITED is chosen (not TIMEOUT) for this scenario.
        IOException composedException = new IOException(
                "Timed out waiting for SHAFT MCP response.\nprocess exit code: 143");
        McpInvocationError category = McpInvocationError.categorize(composedException);
        assertEquals(McpInvocationError.PROCESS_EXITED, category,
                "When process exits mid-call, should return PROCESS_EXITED (not TIMEOUT)");
        assertEquals("MCP server process exited.", category.message());
        assertEquals("Restart MCP server", category.recoveryAction());
    }

    @Test
    void detailPrefersTheExceptionsOwnMessageOverTheGenericCategoryMessage() {
        IOException exception = new IOException("SHAFT MCP returned an error response.\n{\"code\":-32000,"
                + "\"message\":\"Allure result path cannot be resolved inside the MCP workspace.\"}");
        String detail = McpInvocationError.detail(exception, McpInvocationError.TOOL_ERROR);
        assertEquals(exception.getMessage(), detail,
                "The real server error text must reach the user instead of the fixed generic category message");
    }

    @Test
    void detailAppendsTheCauseMessageWhenNotAlreadyContained() {
        IOException cause = new IOException("Broken pipe");
        IOException exception = new IOException("Failed to send SHAFT MCP request.", cause);
        String detail = McpInvocationError.detail(exception, McpInvocationError.CONNECTION_LOST);
        assertEquals("Failed to send SHAFT MCP request.\nBroken pipe", detail);
    }

    @Test
    void detailFallsBackToTheCategoryMessageWhenNothingElseIsAvailable() {
        IOException blankException = new IOException((String) null);
        String detail = McpInvocationError.detail(blankException, McpInvocationError.TOOL_ERROR);
        assertEquals(McpInvocationError.TOOL_ERROR.message(), detail);
    }

    @Test
    void detailFallsBackToTheCategoryMessageForANullException() {
        assertEquals(McpInvocationError.TOOL_ERROR.message(),
                McpInvocationError.detail(null, McpInvocationError.TOOL_ERROR));
    }
}
