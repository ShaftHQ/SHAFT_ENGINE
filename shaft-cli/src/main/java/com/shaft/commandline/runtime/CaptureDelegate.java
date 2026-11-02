package com.shaft.commandline.runtime;

import java.util.List;

/**
 * Seam over the standalone {@code CaptureCli} engine (file-to-code codegen, no MCP session), so
 * {@code codegen} can be unit-tested without exercising the real generator. The production
 * implementation is a {@code CaptureCli::run} method reference; tests supply a fake.
 */
@FunctionalInterface
public interface CaptureDelegate {

    /**
     * @param args          the {@code CaptureCli} command-line arguments (e.g. {@code generate --session ...})
     * @param launchPrefix  the daemon relaunch command prefix ({@code generate} does not use it)
     * @return the process-style exit code
     */
    int run(String[] args, List<String> launchPrefix);
}
