package com.shaft.commandline.session;

import java.util.List;

/**
 * How to launch shaft-mcp. The {@code baseCommand} is a ready-to-run process command that starts an
 * MCP server in its default (stdio) profile, e.g. {@code [java, @<versionDir>/shaft-mcp.args]} or, for
 * a dev checkout, {@code [java, -cp, <classpath>, com.shaft.mcp.ShaftMcpApplication]}.
 *
 * <p>To start the HTTP daemon, append {@code --spring.profiles.active=http --server.port=<port>} to
 * {@code baseCommand}. The shaft-mcp jar has <b>no {@code Main-Class}</b>, so {@code java -jar} is never used.
 *
 * @param baseCommand the process command that launches a stdio MCP server
 * @param jarPath     the resolved shaft-mcp jar path (for diagnostics / session metadata)
 */
public record LaunchSpec(List<String> baseCommand, String jarPath) {
}
