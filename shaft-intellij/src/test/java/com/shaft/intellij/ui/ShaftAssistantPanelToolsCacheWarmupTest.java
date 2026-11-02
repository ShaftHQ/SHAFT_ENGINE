package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Proxy;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3870/#3866 T4 goal 3 ("cache-warm"): {@code ShaftMcpInvocationService}'s
 * {@code tools/list} cache used to be warmed only by {@link ShaftFeaturePanel} (see
 * {@code ShaftFeaturePanelCatalogTest}), never by the chat panel itself -- confirmed by reading
 * {@code ShaftAssistantPanel}'s source before this change: every {@code startTool} call site went
 * straight to dispatch with no prior {@code startListTools()} anywhere in the class. That left
 * {@code ShaftMcpInvocationService.startTool}'s unknown-tool fail-fast + did-you-mean guard
 * (which reads {@code knownToolNames()}, populated only after a successful {@code startListTools})
 * inert for every chat-panel dispatch unless a user happened to have also opened the separate
 * Feature panel first in the same session.
 *
 * <p>Mirrors {@code ShaftFeaturePanelCatalogTest}'s "trap project" pattern: the real end-to-end
 * path needs {@code ApplicationManager}, unavailable in this Gradle test JVM, so this suite instead
 * pins the guard conditions -- warm-up is attempted exactly when a real project and configured MCP
 * are both present, and never otherwise -- proven with a project stub that fails the test if the
 * service is ever reached when it shouldn't be.</p>
 */
class ShaftAssistantPanelToolsCacheWarmupTest {

    @Test
    void constructionWarmsTheToolsCacheWhenMcpIsConfiguredAndAProjectExists() {
        AtomicBoolean serviceReached = new AtomicBoolean();

        new ShaftAssistantPanel(trapProject(serviceReached), connectedMcpSettings(),
                ShaftAssistantChatState.getInstance(null));

        assertTrue(serviceReached.get(),
                "the chat panel must warm the shared tools/list cache on construction so the "
                        + "unknown-tool fail-fast + did-you-mean guard is live for chat dispatches too");
    }

    @Test
    void constructionNeverCallsTheServiceWithoutAProject() {
        // Mirrors ShaftAssistantPanelLayoutTest's null-project construction; nothing to warm.
        new ShaftAssistantPanel(null, connectedMcpSettings(), ShaftAssistantChatState.getInstance(null));
        // No trap possible with a null project -- absence of a thrown exception is the assertion;
        // the real guard-order proof is unconfiguredMcp below plus the positive case above.
    }

    @Test
    void constructionNeverCallsTheServiceWhenMcpIsNotConfigured() {
        AtomicBoolean serviceReached = new AtomicBoolean();

        new ShaftAssistantPanel(trapProject(serviceReached), unverifiedMcpSettings(),
                ShaftAssistantChatState.getInstance(null));

        assertFalse(serviceReached.get(), "an unready MCP command must skip the warm-up entirely");
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
     * {@code null} (an unwired service) -- identical in spirit to
     * {@code ShaftFeaturePanelCatalogTest}'s {@code trapProject}, scoped to
     * {@code ShaftMcpInvocationService} lookups specifically so the panel's other legitimate
     * {@code getService}/{@code PropertiesComponent} calls during construction are not mistaken for
     * the warm-up being reached.
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
                            return "shaft-assistant-panel-cache-warmup-test-project";
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
