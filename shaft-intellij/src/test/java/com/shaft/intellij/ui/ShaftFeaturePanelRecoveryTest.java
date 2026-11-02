package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.McpInvocationError;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3626: {@code showResult}/{@code showCatalogResult} must surface a clickable
 * recovery button (not just the existing free-text "Recovery: &lt;action&gt;" line) whenever a
 * result carries an {@link McpInvocationError} category, labeled per
 * {@link com.shaft.intellij.mcp.RecoveryActions#forCategory(McpInvocationError)}.
 *
 * <p>Kept in its own file rather than added to {@link ShaftFeaturePanelCatalogTest}, whose javadoc
 * scopes it specifically to the auto-populate-catalog behavior. As that test class's javadoc
 * documents, a real {@code ShaftMcpInvocationService} click-through (the {@code RESTART} branch
 * calling {@code ShaftMcpInvocationService.getInstance(project).restartConnection()}) needs
 * {@code ApplicationManager.getApplication()}, unavailable in this Gradle unit test JVM; this suite
 * instead drives the private completion handlers directly via reflection (mirroring
 * {@code ShaftFeaturePanelCatalogTest}'s {@code invokeApplyAutoPopulatedCatalog} helper) and pins
 * only the button's visibility/label, not a real restart/retry click.</p>
 */
class ShaftFeaturePanelRecoveryTest {

    @Test
    void showResultShowsRetryLabelForATimeoutCategory() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        invokeShowResult(panel, "some_tool", ShaftMcpToolResult.failure(
                "Request timed out.", McpInvocationError.TIMEOUT, McpInvocationError.TIMEOUT.recoveryAction()),
                null);

        JButton recoveryButton = recoveryButton(panel);
        assertTrue(recoveryButton.isVisible());
        assertEquals("Retry", recoveryButton.getText());
    }

    @Test
    void showResultShowsRestartLabelForAProcessExitedCategory() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        invokeShowResult(panel, "some_tool", ShaftMcpToolResult.failure(
                "MCP server process exited.", McpInvocationError.PROCESS_EXITED,
                McpInvocationError.PROCESS_EXITED.recoveryAction()), null);

        JButton recoveryButton = recoveryButton(panel);
        assertTrue(recoveryButton.isVisible());
        assertEquals("Restart MCP server", recoveryButton.getText());
    }

    @Test
    void showResultHidesTheRecoveryButtonOnSuccess() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        invokeShowResult(panel, "some_tool", ShaftMcpToolResult.success("{}"), null);

        assertFalse(recoveryButton(panel).isVisible());
    }

    @Test
    void showCatalogResultShowsViewLogsLabelForAToolErrorCategory() throws Exception {
        ShaftFeaturePanel panel = newPanel();

        invokeShowCatalogResult(panel, ShaftMcpToolResult.failure(
                "boom", McpInvocationError.TOOL_ERROR, McpInvocationError.TOOL_ERROR.recoveryAction()), null);

        JButton recoveryButton = recoveryButton(panel);
        assertTrue(recoveryButton.isVisible());
        assertEquals("View logs", recoveryButton.getText());
    }

    private static ShaftFeaturePanel newPanel() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return new ShaftFeaturePanel(null, settings, List.of(new ToolCategory("MCP", List.of())));
    }

    private static void invokeShowResult(
            ShaftFeaturePanel panel, String toolName, ShaftMcpToolResult result, Throwable error) throws Exception {
        Method showResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showResult", String.class, ShaftMcpToolResult.class, Throwable.class,
                com.intellij.openapi.project.Project.class);
        showResult.setAccessible(true); // NOPMD - reflective test invocation of a private completion handler
        showResult.invoke(panel, toolName, result, error, (com.intellij.openapi.project.Project) null);
    }

    private static void invokeShowCatalogResult(
            ShaftFeaturePanel panel, ShaftMcpToolResult result, Throwable error) throws Exception {
        Method showCatalogResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showCatalogResult", ShaftMcpToolResult.class, Throwable.class,
                com.intellij.openapi.project.Project.class);
        showCatalogResult.setAccessible(true); // NOPMD - reflective test invocation of a private completion handler
        showCatalogResult.invoke(panel, result, error, (com.intellij.openapi.project.Project) null);
    }

    private static JButton recoveryButton(ShaftFeaturePanel panel) throws Exception {
        Field field = ShaftFeaturePanel.class.getDeclaredField("recoveryButton");
        field.setAccessible(true); // NOPMD - test-only field inspection, matching the established getField pattern
        return (JButton) field.get(panel);
    }
}
