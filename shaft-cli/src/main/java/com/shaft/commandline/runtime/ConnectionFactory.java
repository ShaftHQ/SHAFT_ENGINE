package com.shaft.commandline.runtime;

import com.shaft.commandline.mcp.HttpMcpClient;
import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.mcp.StdioMcpClient;
import com.shaft.commandline.session.McpLauncherLocator;
import com.shaft.commandline.session.SessionInfo;
import com.shaft.commandline.session.SessionManager;

import java.net.URI;
import java.util.Optional;

/**
 * Chooses the MCP transport for a command per the routing rule: if a daemon session is live, connect
 * to it over streamable HTTP (so browser/device state persists); otherwise spawn an ephemeral stdio
 * child. Commands that require live state fail fast when no daemon is running unless {@code --stdio-ok}.
 */
public final class ConnectionFactory implements McpConnector {

    private final SessionManager sessionManager;
    private final McpLauncherLocator locator;

    /**
     * Uses the real session manager and launcher locator.
     */
    public ConnectionFactory() {
        this(new SessionManager(), new McpLauncherLocator());
    }

    /**
     * @param sessionManager the session manager
     * @param locator        the launcher locator
     */
    public ConnectionFactory(SessionManager sessionManager, McpLauncherLocator locator) {
        this.sessionManager = sessionManager;
        this.locator = locator;
    }

    /**
     * @param requiresSession whether the command needs persistent live state (browser/element/capture)
     * @param stdioOk         whether the caller opted in to a throwaway one-shot server
     * @return a connected MCP client the caller must close
     */
    @Override
    public McpClient connect(boolean requiresSession, boolean stdioOk) {
        Optional<SessionInfo> live = sessionManager.liveSession();
        if (live.isPresent()) {
            return new HttpMcpClient(URI.create("http://127.0.0.1:" + live.get().port() + "/mcp"));
        }
        if (requiresSession && !stdioOk) {
            throw new McpException("This command needs a live shaft-mcp session so browser/device "
                    + "state persists across calls. Start one with `shaft-cli session start`, or pass "
                    + "--stdio-ok to run a throwaway one-shot server (state is discarded on exit).");
        }
        return StdioMcpClient.spawn(locator.locate().baseCommand());
    }
}
