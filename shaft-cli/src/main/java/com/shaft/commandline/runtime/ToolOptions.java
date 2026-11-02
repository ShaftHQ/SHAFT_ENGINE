package com.shaft.commandline.runtime;

import picocli.CommandLine.Option;

/**
 * Shared options mixed into every tool-invoking command. Argument positional {@code key=value} tokens
 * are declared per command (their index differs between {@code call} and the curated aliases).
 */
public class ToolOptions {

    /**
     * Print the raw JSON-RPC {@code result} instead of rendered text.
     */
    @Option(names = "--json", description = "Print the raw JSON-RPC result.")
    public boolean json;

    /**
     * Tool arguments supplied as a JSON object string.
     */
    @Option(names = "--args", paramLabel = "JSON", description = "Tool arguments as a JSON object, e.g. --args '{\"url\":\"https://x\"}'.")
    public String argsJson;

    /**
     * Allow a throwaway one-shot stdio server when no daemon session is running.
     */
    @Option(names = "--stdio-ok", description = "Allow a throwaway one-shot server when no session is running.")
    public boolean stdioOk;
}
