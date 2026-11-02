package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.mcp.ToolCatalogIndex;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3895 (deferred from #3883(b)): the explicit "Refresh tools" completion path
 * ({@code showCatalogResult}) must surface a one-line live-vs-bundled tool-catalog diff
 * ({@link com.shaft.intellij.mcp.ToolCatalogDiff}) on the status label when a fresh {@code
 * tools/list} response disagrees with the bundled {@link ToolCatalogIndex#toolNames()}, and stay
 * silent when they match -- exactly the same "own file, reflection-driven completion handler"
 * pattern as {@link ShaftFeaturePanelRecoveryTest} and {@link ShaftFeaturePanelCatalogTest}, since
 * a real {@code ShaftMcpInvocationService} round trip needs {@code ApplicationManager}, unavailable
 * in this Gradle unit test JVM.
 */
class ShaftFeaturePanelCatalogDiffTest {

    @Test
    void showCatalogResultStaysSilentWhenTheLiveCatalogMatchesTheBundledIndexExactly() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        invokeShowCatalogResult(panel, ShaftMcpToolResult.success(toolsListJsonFor(ToolCatalogIndex.toolNames())));

        String status = statusText(panel);
        assertTrue(status.contains("Tools refreshed"), status);
        assertFalse(status.contains("differs from bundled index"), status);
    }

    @Test
    void showCatalogResultSurfacesOneLineWhenTheLiveCatalogHasAToolNotInTheBundledIndex() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        JsonArray tools = new JsonArray();
        JsonObject tool = new JsonObject();
        tool.addProperty("name", "brand_new_unbundled_tool_xyz");
        tool.addProperty("description", "Not in the bundled index");
        tools.add(tool);
        JsonObject payload = new JsonObject();
        payload.add("tools", tools);

        invokeShowCatalogResult(panel, ShaftMcpToolResult.success(payload.toString()));

        String status = statusText(panel);
        assertTrue(status.contains("Tools refreshed"), status);
        assertTrue(status.contains("Live MCP catalog differs from bundled index:"), status);
        assertTrue(status.contains("brand_new_unbundled_tool_xyz"), status);
        // status is a plain JLabel: it never renders "\n" as a line break, so the note must be
        // joined onto the same line, not prefixed with a newline that would just clip/garble.
        assertFalse(status.contains("\n"), status);
    }

    @Test
    void showCatalogResultStaysSilentOnFailure() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        invokeShowCatalogResult(panel, ShaftMcpToolResult.failure("boom"));

        assertFalse(statusText(panel).contains("differs from bundled index"), statusText(panel));
    }

    private static String toolsListJsonFor(Set<String> toolNames) {
        JsonArray tools = new JsonArray();
        for (String name : toolNames) {
            JsonObject tool = new JsonObject();
            tool.addProperty("name", name);
            tool.addProperty("description", "");
            tools.add(tool);
        }
        JsonObject payload = new JsonObject();
        payload.add("tools", tools);
        return payload.toString();
    }

    private static ShaftFeaturePanel newPanel() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return new ShaftFeaturePanel(null, settings, List.of(new ToolCategory("MCP", List.of())));
    }

    private static void invokeShowCatalogResult(ShaftFeaturePanel panel, ShaftMcpToolResult result) throws Exception {
        Method showCatalogResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showCatalogResult", ShaftMcpToolResult.class, Throwable.class,
                com.intellij.openapi.project.Project.class);
        showCatalogResult.setAccessible(true); // NOPMD - reflective test invocation of a private completion handler
        showCatalogResult.invoke(panel, result, null, (com.intellij.openapi.project.Project) null);
    }

    private static String statusText(ShaftFeaturePanel panel) throws Exception {
        Field field = ShaftFeaturePanel.class.getDeclaredField("status");
        field.setAccessible(true); // NOPMD - test-only field inspection, matching the established getField pattern
        return ((javax.swing.JLabel) field.get(panel)).getText();
    }
}
