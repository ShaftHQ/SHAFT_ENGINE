package com.shaft.intellij.ui;

import com.intellij.openapi.util.Disposer;
import com.shaft.intellij.mcp.ShaftMcpConnectionState;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Regression coverage for issue #3621: {@link ShaftAssistantPanel#addNotify()} and {@link
 * ShaftAssistantPanel#removeNotify()} each used to write {@code this::onConnectionStateChanged} as
 * a separate method-reference expression. Re-evaluating a method reference is not guaranteed to
 * produce an identity-equal object, so {@link ShaftMcpConnectionState#removeStateChangeListener}
 * (a plain {@code List.remove(Object)}) could silently fail to find and remove the listener added
 * by {@code addNotify()}. Swing calls addNotify()/removeNotify() every time a panel is added to or
 * removed from a visible container (tool window tab switches, re-docking), so this leaked one
 * stale listener per cycle.
 */
class ShaftAssistantPanelTest {
    @Test
    void listenerCountStaysFlatAcrossRepeatedAddRemoveNotifyCycles() throws ReflectiveOperationException {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, ShaftAssistantChatState.getInstance(null));

        // project == null so the constructor leaves connectionState null (no live IntelliJ Project
        // service available in this headless Gradle unit test JVM); inject a real
        // ShaftMcpConnectionState via reflection so addNotify()/removeNotify() exercise their real
        // listener add/remove calls, the exact path this regression is about.
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        Field connectionStateField = ShaftAssistantPanel.class.getDeclaredField("connectionState");
        connectionStateField.setAccessible(true);
        connectionStateField.set(panel, connectionState);

        // ShaftAssistantPanel.addNotify() calls super.addNotify(), which cascades into the child
        // AssistantTranscriptView's own addNotify() override. That override unconditionally reaches
        // ApplicationManager.getApplication().getMessageBus() the first time its
        // lafConnectionDisposable field is null -- unrelated to the #3621 connection-listener bug
        // this test targets, but fatal (NPE) in this lightweight, no-platform-fixture Gradle unit
        // test JVM where no live Application exists. Pre-seed that guard field with a disposable
        // placeholder before every addNotify() call so the transcript view's own addNotify() short-
        // circuits past the ApplicationManager touch, letting the panel's *real* addNotify()/
        // removeNotify() run end to end.
        Field transcriptField = ShaftAssistantPanel.class.getDeclaredField("transcript");
        transcriptField.setAccessible(true);
        AssistantTranscriptView transcript = (AssistantTranscriptView) transcriptField.get(panel);
        Field lafConnectionDisposableField =
                AssistantTranscriptView.class.getDeclaredField("lafConnectionDisposable");
        lafConnectionDisposableField.setAccessible(true);

        for (int cycle = 0; cycle < 5; cycle++) {
            lafConnectionDisposableField.set(transcript, Disposer.newDisposable());
            panel.addNotify();
            panel.removeNotify();
        }

        assertEquals(0, connectionState.listenerCount(),
                "issue #3621: removeNotify() must remove the exact same listener reference "
                        + "addNotify() registered. If addNotify()/removeNotify() each re-evaluate "
                        + "`this::onConnectionStateChanged` as a fresh method reference, "
                        + "removeStateChangeListener's List.remove(Object) silently no-ops instead of "
                        + "removing anything, so the listener list grows by one on every tab switch or "
                        + "re-dock and every leaked listener keeps firing onConnectionStateChanged() on a "
                        + "panel instance that is no longer attached to any UI.");
    }
}
