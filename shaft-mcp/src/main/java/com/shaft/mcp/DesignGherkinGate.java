package com.shaft.mcp;

/**
 * S1-03 hook: Gherkin draft is allowed only when analysis reports no unaccepted blocking gaps.
 */
final class DesignGherkinGate {
    private DesignGherkinGate() {
    }

    static boolean allowed(McpDesignAnalysis analysis) {
        return analysis != null && analysis.gherkinGenerationAllowed();
    }
}
