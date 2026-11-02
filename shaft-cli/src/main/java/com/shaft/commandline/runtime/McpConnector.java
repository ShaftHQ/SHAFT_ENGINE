package com.shaft.commandline.runtime;

import com.shaft.commandline.mcp.McpClient;

/**
 * Supplies a connected {@link McpClient} for a command, applying the HTTP-vs-stdio routing rule.
 * The production implementation is {@link ConnectionFactory}; tests supply a fake.
 */
public interface McpConnector {

    /**
     * @param requiresSession whether the command needs persistent live state
     * @param stdioOk         whether the caller opted in to a throwaway one-shot server
     * @return a connected client the caller must close
     */
    McpClient connect(boolean requiresSession, boolean stdioOk);
}
