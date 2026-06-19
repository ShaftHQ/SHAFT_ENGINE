package com.shaft.capture.generate;

import java.util.List;

/**
 * Current Playwright codegen feature inventory and SHAFT Capture mapping.
 */
public final class CodegenFeatureCatalog {
    private static final List<Feature> FEATURES = List.of(
            feature("Recording UI", "VS Code record new test", "Record new",
                    "SUPPORTED", "Use capture_start or the generated workbench record panel."),
            feature("Recording UI", "Record at cursor", "Record at cursor",
                    "AGENT_MAPPED", "MCP returns method/code blocks for insertion at an agent-selected location."),
            feature("Recording UI", "Inspector record/stop/copy/clear", "Inspector toolbar",
                    "SUPPORTED", "Use capture_start/status/stop and generated code blocks."),
            feature("Recording UI", "Pick locator and refine locator", "Pick Locator",
                    "SUPPORTED", "Captured locator candidates are ranked and returned in generation reports."),
            feature("Recording UI", "Assertions: visibility, text, value", "Assert toolbar",
                    "SUPPORTED", "Use verification events or ASSERTION checkpoints plus generated review guidance."),
            feature("Recording", "Actions: click and fill/type", "browser interaction",
                    "SUPPORTED", "Captured as click, type, clear, keyboard, select, toggle, and upload events."),
            feature("Recording", "Navigation, tabs/windows, frames, alerts, waits", "browser interaction",
                    "SUPPORTED", "Captured as navigation, window, frame, alert, and wait-capable session events."),
            feature("Recording", "Best locator selection: role, text, test id, uniqueness", "locator generator",
                    "SUPPORTED", "LocatorRanker prefers semantic/test-id evidence and reports alternatives."),
            feature("Recording", "Custom setup with pause/inspector", "page.pause()",
                    "MAPPED", "Use the managed SHAFT browser plus MCP/CLI checkpoints."),
            feature("CLI", "Optional start URL", "[url]",
                    "SUPPORTED", "capture_start requires an explicit URL to keep sessions reproducible."),
            feature("CLI", "Output file", "-o, --output",
                    "SUPPORTED", "capture_start outputPath and capture generate outputDirectory."),
            feature("CLI", "Target language", "--target",
                    "MAPPED", "Accepted as metadata; SHAFT generates Java TestNG and MCP insertion blocks."),
            feature("CLI", "Custom test id attribute", "--test-id-attribute",
                    "SUPPORTED", "Passed to the browser recorder locator candidate list."),
            feature("CLI", "Browser family", "-b, --browser",
                    "PARTIAL", "Chrome/Chromium and Edge are supported; Firefox/WebKit are reported unsupported."),
            feature("CLI", "Chromium channel", "--channel",
                    "MAPPED", "Use browser selection; channel is retained as metadata/warning."),
            feature("Emulation", "Viewport size", "--viewport-size",
                    "SUPPORTED", "Applied to the managed browser window."),
            feature("Emulation", "Device preset", "--device",
                    "MAPPED", "Retained as metadata; set viewport/user agent for replay."),
            feature("Emulation", "Color scheme", "--color-scheme",
                    "METADATA", "Retained as metadata until SHAFT exposes browser media emulation."),
            feature("Emulation", "Geolocation", "--geolocation",
                    "METADATA", "Retained as metadata until SHAFT exposes browser geolocation emulation."),
            feature("Emulation", "Language/locale", "--lang",
                    "SUPPORTED", "Applied through Chromium/Edge --lang."),
            feature("Emulation", "Timezone", "--timezone",
                    "METADATA", "Retained as metadata until SHAFT exposes timezone emulation."),
            feature("Context", "Ignore HTTPS errors", "--ignore-https-errors",
                    "SUPPORTED", "Mapped to Selenium acceptInsecureCerts."),
            feature("Context", "Block service workers", "--block-service-workers",
                    "METADATA", "Retained as metadata; Selenium does not provide portable blocking here."),
            feature("Context", "Load storage state", "--load-storage",
                    "MAPPED", "Retained as metadata; use userDataDir for state reuse."),
            feature("Context", "Save storage state", "--save-storage",
                    "MAPPED", "Retained as metadata; use userDataDir for state reuse."),
            feature("Context", "Existing user data dir", "--user-data-dir",
                    "SUPPORTED", "Used as the managed browser profile and not deleted on cleanup."),
            feature("Context", "Timeout", "--timeout",
                    "SUPPORTED", "Mapped to browser page-load timeout."),
            feature("Context", "User agent", "--user-agent",
                    "SUPPORTED", "Applied through Chromium/Edge --user-agent."),
            feature("Network", "Proxy server", "--proxy-server",
                    "SUPPORTED", "Applied through Chromium/Edge --proxy-server."),
            feature("Network", "Proxy bypass", "--proxy-bypass",
                    "SUPPORTED", "Applied through Chromium/Edge --proxy-bypass-list."),
            feature("Network", "Save HAR", "--save-har",
                    "METADATA", "Retained as metadata; SHAFT Capture does not write HAR files."),
            feature("Network", "Save HAR glob", "--save-har-glob",
                    "METADATA", "Retained as metadata; SHAFT Capture does not write HAR files."),
            feature("Agent", "Context-aware insertion into Page Objects", "MCP code blocks",
                    "SUPPORTED", "MCP returns full-class, method, and agent integration blocks for repo-aware insertion."),
            feature("Agent", "Agent LLM fallback without user API key", "MCP provider handoff",
                    "SUPPORTED", "MCP tools treat agent invocation as approval and return safe evidence/code blocks."));

    private CodegenFeatureCatalog() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the feature inventory.
     *
     * @return immutable feature list
     */
    public static List<Feature> features() {
        return FEATURES;
    }

    private static Feature feature(
            String category,
            String name,
            String playwrightControl,
            String shaftSupport,
            String notes) {
        return new Feature(category, name, playwrightControl, shaftSupport, notes);
    }

    /**
     * One Playwright codegen feature and its SHAFT mapping.
     *
     * @param category feature category
     * @param name feature name
     * @param playwrightControl Playwright UI or CLI control
     * @param shaftSupport SHAFT support level
     * @param notes mapping notes
     */
    public record Feature(
            String category,
            String name,
            String playwrightControl,
            String shaftSupport,
            String notes) {
    }
}
