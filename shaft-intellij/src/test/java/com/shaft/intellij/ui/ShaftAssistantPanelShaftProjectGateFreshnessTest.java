package com.shaft.intellij.ui;

import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Reproduces the nightly Live Tools E2E failure (issue #3988, run 30146701546:
 * {@code ShaftAssistantPanelLiveAuthoringToolE2ETest#shaftProjectServiceCreatesAndInitializesAgentsThroughTheRealChatPanel}).
 *
 * <p>{@link ShaftAssistantPanel#send} evaluates {@code
 * !ShaftProjectDetector.isShaftProject(project) && AssistantCommand.requiresShaftProject(invocation)}
 * left-to-right, so {@code isShaftProject} runs on EVERY dispatch -- including ones (like {@code
 * shaft_project_create} or a plain {@code /help}) that never needed a SHAFT-project answer at all.
 * {@link com.shaft.intellij.project.ShaftProjectDetector} caches its answer per root for 30 seconds
 * (deliberately -- see its class doc). Dispatching any ungated command against a not-yet-a-SHAFT-project
 * directory therefore caches a {@code false} that outlives the directory becoming a real SHAFT project a
 * moment later (e.g. right after {@code shaft_project_create} runs), wrongly blocking the very next
 * SHAFT-project-only tool call with the "doesn't look like a SHAFT project yet" nudge instead of
 * dispatching it -- exactly the transcript captured in the nightly run.</p>
 *
 * <p>No fake {@code Application} is installed here (unlike {@code LiveChatToolE2ESupport}):
 * {@code showLocalResponse} runs synchronously with no EDT hop (confirmed against {@code
 * ShaftPanelSetupTest}'s {@code assistantLocalPromptCompletionOmitsSyntheticMilestoneBubbles} and
 * sibling tests, which assert on the transcript immediately after a local {@code /help} send with a
 * {@code null} project and no installed {@code Application}), so a synchronous check right after
 * {@code send()} is exactly how production code behaves.</p>
 */
class ShaftAssistantPanelShaftProjectGateFreshnessTest {

    @Test
    void gatedToolCallSeesAProjectCreatedAfterAnEarlierUngatedSend(@TempDir Path projectRoot) throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        Project project = fakeProject(projectRoot);
        ShaftAssistantPanel panel = new ShaftAssistantPanel(project, settings);

        // An ungated command (never needs the SHAFT-project answer) dispatched while projectRoot is
        // still empty -- this alone must not poison ShaftProjectDetector's cache for projectRoot.
        send(panel, project, "/help");

        // The directory becomes a genuine SHAFT project (mirrors shaft_project_create's real output).
        Files.writeString(projectRoot.resolve("pom.xml"), """
                <project>
                    <dependencies>
                        <dependency>
                            <groupId>io.github.shafthq</groupId>
                            <artifactId>shaft-engine</artifactId>
                        </dependency>
                    </dependencies>
                </project>
                """);

        // A SHAFT-project-only tool call, moments later (well inside the 30s cache TTL), must see the
        // now-real project instead of a stale cached "false" from the earlier ungated /help dispatch.
        send(panel, project, "/mcp shaft_project_upgrade");

        String transcript = panel.transcriptMarkdown();
        assertFalse(transcript.contains("doesn't look like a SHAFT project yet"),
                "shaft_project_upgrade was wrongly blocked by a stale SHAFT-project-detection cache: "
                        + transcript);
        assertTrue(transcript.contains("Configure SHAFT MCP"),
                "Expected the gate to pass and fall through to the (unconfigured) MCP setup check: "
                        + transcript);
    }

    private static void send(ShaftAssistantPanel panel, Project project, String prompt) throws Exception {
        panel.prefillPrompt(prompt);
        Method send = ShaftAssistantPanel.class.getDeclaredMethod("send", Project.class);
        send.setAccessible(true);
        try {
            send.invoke(panel, project);
        } catch (InvocationTargetException exception) {
            if (exception.getCause() instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            throw exception;
        }
    }

    private static Project fakeProject(Path basePath) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return basePath.toString();
                        case "getName":
                            return "shaft-assistant-panel-shaft-project-gate-freshness-test-project";
                        case "isDisposed":
                            return false;
                        case "getService":
                            Class<?> serviceClass = (Class<?>) arguments[0];
                            // send()'s openFileContext(project) unconditionally dereferences this once
                            // project != null (ShaftAssistantPanel.java:4023-4028); a no-open-files fake is
                            // the honest state for a headless test with no real editor.
                            return serviceClass == FileEditorManager.class ? NoOpFileEditorManager.INSTANCE : null;
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
        return returnType == void.class ? null : 0;
    }
}
