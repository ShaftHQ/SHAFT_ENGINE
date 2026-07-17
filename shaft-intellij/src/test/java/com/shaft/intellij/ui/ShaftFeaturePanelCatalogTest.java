package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JComboBox;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ShaftFeaturePanel}'s "populate the tool catalog automatically on first use"
 * behavior, kept in its own test class rather than added to {@code ShaftPanelSetupTest} (owned by
 * another in-flight change) per this task's file-ownership boundary.
 *
 * <p>The real end-to-end path (construction calling through to a live
 * {@code ShaftMcpInvocationService}) cannot be exercised here: {@code ShaftMcpInvocationService}'s
 * public entry points read {@code ShaftSettingsState.getInstance()}, which needs
 * {@code ApplicationManager.getApplication()} — verified empirically to be {@code null} in this
 * Gradle test JVM (no real IntelliJ platform bootstrap), matching how
 * {@code ShaftPanelSetupTest}'s own {@code refreshCatalogEntersBusyStateThenBlocksReentryViaRealGuard}
 * test documents the same gap for the manual "Refresh tools" button. So this suite instead pins:
 * (a) construction never throws and keeps the curated fallback catalog when the service call fails,
 * for every guard combination (disabled, no project, unconfigured MCP), proven with a "trap" project
 * that fails the test if the service is ever actually reached when it shouldn't be; and (b) the
 * package-private completion handler {@code applyAutoPopulatedCatalog} correctly merges a successful
 * catalog fetch, and is a no-op on failure or when another invocation is already in flight.</p>
 */
class ShaftFeaturePanelCatalogTest {

    @Test
    void autoPopulateNeverThrowsAndKeepsCuratedCatalogWhenServiceIsUnavailable() {
        // catalogRefreshEnabled=true (2-arg constructor), MCP configured, real project: this is
        // exactly the combination that attempts the background warm-up fetch on construction.
        ShaftFeaturePanel panel = new ShaftFeaturePanel(trapProject(new AtomicBoolean()), connectedMcpSettings());

        List<ToolCategory> curated = ToolTemplates.categories();
        assertEquals(curated.size(), categorySelector(panel).getItemCount(),
                "an unavailable service must never throw out of the constructor, leaving the curated "
                        + "fallback catalog exactly as it was");
    }

    @Test
    void autoPopulateNeverCallsTheServiceWhenCatalogRefreshIsDisabled() {
        AtomicBoolean serviceReached = new AtomicBoolean();
        // The explicit-categories constructor sets catalogRefreshEnabled=false (see
        // ShaftFeaturePanel(Project, Settings, List)); the guard must return before ever touching
        // project.getService(...), so the trap must never flip.
        new ShaftFeaturePanel(trapProject(serviceReached), connectedMcpSettings(),
                List.of(new ToolCategory("Recorder", ToolTemplates.recorder())));

        assertFalse(serviceReached.get(), "catalogRefreshEnabled=false must skip the warm-up entirely");
    }

    @Test
    void autoPopulateNeverCallsTheServiceWithoutAProject() {
        AtomicBoolean serviceReached = new AtomicBoolean();
        new ShaftFeaturePanel(null, connectedMcpSettings());

        assertFalse(serviceReached.get(), "a null project must skip the warm-up (nothing to call)");
    }

    @Test
    void autoPopulateNeverCallsTheServiceWhenMcpIsNotConfigured() {
        AtomicBoolean serviceReached = new AtomicBoolean();
        new ShaftFeaturePanel(trapProject(serviceReached), unverifiedMcpSettings());

        assertFalse(serviceReached.get(), "an unready MCP command must skip the warm-up");
    }

    @Test
    void applyAutoPopulatedCatalogMergesASuccessfulFetchIntoTheCategories() throws Exception {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, connectedMcpSettings(),
                List.of(new ToolCategory("MCP", List.of())));

        invokeApplyAutoPopulatedCatalog(panel, ShaftMcpToolResult.success(discoveredToolsListJson()), null);

        assertTrue(hasTemplateNamed(categories(panel), "auto_populated_probe_tool"),
                "a successful background fetch must merge the discovered tool into the catalog");
    }

    @Test
    void applyAutoPopulatedCatalogIsANoOpWhenAnotherInvocationIsAlreadyInFlight() throws Exception {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, connectedMcpSettings(),
                List.of(new ToolCategory("MCP", List.of())));
        List<ToolCategory> before = categories(panel);
        panel.currentInvocation =
                new com.shaft.intellij.mcp.ShaftMcpInvocation(new java.util.concurrent.CompletableFuture<>(), () -> {
                });

        invokeApplyAutoPopulatedCatalog(panel, ShaftMcpToolResult.success(discoveredToolsListJson()), null);

        assertEquals(before, categories(panel),
                "a real user-triggered run/refresh already in flight must not be clobbered by the "
                        + "silent background warm-up completing later");
    }

    @Test
    void applyAutoPopulatedCatalogIsANoOpOnFailureOrError() throws Exception {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, connectedMcpSettings(),
                List.of(new ToolCategory("MCP", List.of())));
        List<ToolCategory> before = categories(panel);

        invokeApplyAutoPopulatedCatalog(panel, ShaftMcpToolResult.failure("boom"), null);
        invokeApplyAutoPopulatedCatalog(panel, null, new IOException("connection lost"));

        assertEquals(before, categories(panel),
                "a failed or errored background warm-up must keep the curated fallback catalog");
    }

    private static boolean hasTemplateNamed(List<ToolCategory> categories, String toolName) {
        return categories.stream()
                .flatMap(category -> category.templates().stream())
                .anyMatch(template -> toolName.equals(template.toolName()));
    }

    private static String discoveredToolsListJson() {
        return "{\"tools\":[{\"name\":\"auto_populated_probe_tool\",\"description\":\"Auto-populate probe\"}]}";
    }

    private static List<ToolCategory> categories(ShaftFeaturePanel panel) {
        return panel.categories;
    }

    private static JComboBox<ToolCategory> categorySelector(ShaftFeaturePanel panel) {
        return panel.categorySelector();
    }

    private static void invokeApplyAutoPopulatedCatalog(
            ShaftFeaturePanel panel, ShaftMcpToolResult result, Throwable error) {
        panel.applyAutoPopulatedCatalog(result, error);
    }

    private static ShaftSettingsState.Settings connectedMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static ShaftSettingsState.Settings unverifiedMcpSettings() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.mcpSetupComplete = false;
        return settings;
    }

    /**
     * A {@link Project} stub whose {@code getService} flips the given flag before returning
     * {@code null} (an unwired service, same as {@code ShaftPanelSetupTest}'s {@code fakeProject()}).
     * Used to prove a guard short-circuits before ever reaching the service call, not merely that it
     * happens to fail once reached. Scoped to {@code ShaftMcpInvocationService} requests specifically
     * (the auto-populate warm-up's own {@code getInstance(project)} calls
     * {@code project.getService(ShaftMcpInvocationService.class)}) rather than any {@code getService}
     * call whatsoever: {@link ShaftFeaturePanel}'s constructor also legitimately looks up
     * {@code PropertiesComponent} for divider-position persistence (issue #3636) regardless of these
     * guards, and that unrelated lookup must not be mistaken for the warm-up being reached.
     */
    private static Project trapProject(AtomicBoolean serviceReached) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return "";
                        case "getName":
                            return "shaft-feature-panel-catalog-test-project";
                        case "getService":
                            if (arguments != null && arguments.length > 0
                                    && arguments[0] == com.shaft.intellij.mcp.ShaftMcpInvocationService.class) {
                                serviceReached.set(true);
                            }
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        if (returnType == boolean.class) {
            return false;
        }
        return 0;
    }
}
