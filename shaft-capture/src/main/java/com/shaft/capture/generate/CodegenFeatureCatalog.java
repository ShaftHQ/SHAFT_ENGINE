package com.shaft.capture.generate;

import java.util.List;

/**
 * Current Playwright codegen feature inventory and SHAFT Capture mapping.
 */
public final class CodegenFeatureCatalog {
    private static final List<Feature> FEATURES = List.of(
            feature("Recording UI", "IntelliJ record new test", "Record new",
                    "SUPPORTED", "Use capture_start or the generated workbench record panel."),
            feature("Recording UI", "Record at cursor", "Record at cursor",
                    "AGENT_MAPPED", "MCP returns method/code blocks for insertion at an agent-selected location."),
            feature("Recording UI", "Inspector record/stop/copy/clear", "Inspector toolbar",
                    "SUPPORTED", "The live browser overlay lists captured steps and exposes pause/resume/stop controls."),
            feature("Recording UI", "Edit captured actions and add checkpoints", "Inspector toolbar",
                    "SUPPORTED", "The live browser overlay records edited action notes and checkpoints into generated code."),
            feature("Recording UI", "Reusable flow segmentation", "checkpoint FLOW_START/FLOW_END",
                    "SUPPORTED", "Marked Capture flow boundaries generate reusable Java helper methods."),
            feature("Recording UI", "Pick locator and refine locator", "Pick Locator",
                    "SUPPORTED", "The live overlay probes, ranks, and pins locator candidates for code generation."),
            feature("Recording UI", "Assertions: visibility, text, value", "Assert toolbar",
                    "SUPPORTED", "The overlay records point-and-click verification events plus generated review guidance."),
            feature("Recording UI", "In-panel assertions and checkpoints", "Assert/checkpoint dialogs",
                    "SUPPORTED", "The overlay captures browser and element checkpoints without native prompt dialogs."),
            feature("Recording UI", "Live readiness scoring", "status/readiness chip",
                    "SUPPORTED", "Capture status reports ready, risky, or blocked states with actionable warnings."),
            feature("Recording UI", "Step inspector with reorder controls", "Inspector action list",
                    "SUPPORTED", "Captured steps expose edit, delete, assertion, and up/down reorder controls."),
            feature("Recording", "Actions: click and fill/type", "browser interaction",
                    "SUPPORTED", "Captured as click, type, clear, keyboard, select, toggle, and upload events."),
            feature("Recording", "Navigation, tabs/windows, frames, alerts, waits", "browser interaction",
                    "SUPPORTED", "Captured as navigation, window, frame, alert, and wait-capable session events."),
            feature("Recording", "Best locator selection: role, text, test id, uniqueness", "locator generator",
                    "SUPPORTED", "LocatorRanker prefers semantic/test-id evidence and reports alternatives."),
            feature("Recording", "Fallback locator replay", "--enable-fallback-locators",
                    "SUPPORTED", "Generated WebDriver replay can try captured ranked alternatives before failing."),
            feature("Recording", "Control-flow suggestions", "--control-flow-preview",
                    "SUPPORTED", "Generation reports deterministic optional-flow suggestions and applies reviewed previews only."),
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
            feature("CLI", "Session goal metadata", "--session-goal",
                    "SUPPORTED", "Stores recording intent in session extensions and generated review comments."),
            feature("CLI", "Browser family", "-b, --browser",
                    "PARTIAL", "Chrome/Chromium and Edge are supported; Firefox/WebKit are reported unsupported."),
            feature("CLI", "Chromium channel", "--channel",
                    "MAPPED", "Use browser selection; channel is retained as metadata/warning."),
            feature("Emulation", "Viewport size", "--viewport-size",
                    "SUPPORTED", "Applied to the managed browser window."),
            feature("Emulation", "Device preset", "--device",
                    "SUPPORTED", "Mapped to bundled Chromium mobile-emulation profiles when available."),
            feature("Emulation", "Color scheme", "--color-scheme",
                    "SUPPORTED", "Applied through Chromium/Edge DevTools media emulation."),
            feature("Emulation", "Geolocation", "--geolocation",
                    "SUPPORTED", "Applied through Chromium/Edge DevTools geolocation override with permission grant."),
            feature("Emulation", "Language/locale", "--lang",
                    "SUPPORTED", "Applied through Chromium/Edge --lang."),
            feature("Emulation", "Timezone", "--timezone",
                    "SUPPORTED", "Applied through Chromium/Edge DevTools timezone override."),
            feature("Context", "Ignore HTTPS errors", "--ignore-https-errors",
                    "SUPPORTED", "Mapped to Selenium acceptInsecureCerts."),
            feature("Context", "Block service workers", "--block-service-workers",
                    "SUPPORTED", "Applied through Chromium/Edge DevTools service-worker bypass."),
            feature("Context", "Load storage state", "--load-storage",
                    "SUPPORTED", "Loads SHAFT browser storage state before the capture navigation."),
            feature("Context", "Save storage state", "--save-storage",
                    "SUPPORTED", "Saves SHAFT browser storage state when the capture stops."),
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
                    "SUPPORTED", "Writes SHAFT observability HAR-like output when the capture stops."),
            feature("Network", "Save HAR glob", "--save-har-glob",
                    "PARTIAL", "Accepted with a warning; SHAFT Capture writes all observed network entries."),
            feature("Agent", "Context-aware insertion into Page Objects", "MCP code blocks",
                    "SUPPORTED", "MCP returns full-class, method, and agent integration blocks for repo-aware insertion."),
            feature("Agent", "Generated review header", "generated source comments",
                    "SUPPORTED", "Generated Java includes readiness, event count, fallback count, and optional session goal."),
            feature("Agent", "MCP Page Object draft", "capture-page-object-draft",
                    "SUPPORTED", "MCP returns a deterministic Page Object draft with locator fields and flow methods."),
            feature("Agent", "Workbench review sections", "Capture Workbench",
                    "SUPPORTED", "The local workbench separates blockers, assertions, locators, Page Object draft, and commands."),
            feature("Agent", "MCP Playwright recording schema convergence", "playwright_recording_code_blocks",
                    "SUPPORTED", "Playwright MCP recordings adapt into Capture sessions for privacy, review, and generation metadata."),
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
