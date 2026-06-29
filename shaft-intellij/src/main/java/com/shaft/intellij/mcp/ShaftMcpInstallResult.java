package com.shaft.intellij.mcp;

/**
 * Result of a SHAFT MCP installer run started from the IntelliJ plugin.
 *
 * @param success whether installation completed successfully
 * @param commandLine stdio command line to persist in plugin settings
 * @param output installer output or failure details
 */
public record ShaftMcpInstallResult(boolean success, String commandLine, String output) {
    static ShaftMcpInstallResult success(String commandLine, String output) {
        return new ShaftMcpInstallResult(true, commandLine == null ? "" : commandLine, output == null ? "" : output);
    }

    static ShaftMcpInstallResult failure(String output) {
        return new ShaftMcpInstallResult(false, "", output == null ? "" : output);
    }
}
