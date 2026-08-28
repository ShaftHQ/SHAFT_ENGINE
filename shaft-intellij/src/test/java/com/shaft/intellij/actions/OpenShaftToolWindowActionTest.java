package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.ex.AnActionListener;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.ActionCallback;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Proxy;
import java.awt.Component;
import java.awt.event.InputEvent;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenShaftToolWindowActionTest {
    @Test
    void filesystemBackedProjectGateRunsOnBackgroundUpdateThread() {
        assertEquals(ActionUpdateThread.BGT, new OpenShaftToolWindowAction().getActionUpdateThread());
    }

    @Test
    void updateBindsVisibilityToShaftProjectDetection(@TempDir Path root) throws Exception {
        OpenShaftToolWindowAction action = new OpenShaftToolWindowAction();
        AnActionEvent event = event(projectAt(root));
        action.update(event);
        assertFalse(event.getPresentation().isEnabledAndVisible());

        Path shaftRoot = Files.createDirectory(root.resolve("shaft"));
        Files.writeString(shaftRoot.resolve("pom.xml"), "<dependency>io.github.shafthq:shaft-engine</dependency>");
        event = event(projectAt(shaftRoot));
        action.update(event);
        assertTrue(event.getPresentation().isEnabledAndVisible());
    }

    @SuppressWarnings("removal")
    private static AnActionEvent event(Project project) {
        DataContext context = dataId -> CommonDataKeys.PROJECT.is(dataId) ? project : null;
        return new AnActionEvent(null, context, "test", new Presentation(), new StubActionManager(), 0);
    }

    @SuppressWarnings({"deprecation", "removal"})
    private static final class StubActionManager extends ActionManager {
        @Override public ActionPopupMenu createActionPopupMenu(String place, ActionGroup group) { return null; }
        @Override public ActionToolbar createActionToolbar(String place, ActionGroup group, boolean horizontal) { return null; }
        @Override public AnAction getAction(String id) { return null; }
        @Override public String getId(AnAction action) { return null; }
        @Override public void registerAction(String id, AnAction action) { /* no-op test fixture */ }
        @Override public void registerAction(String id, AnAction action, PluginId pluginId) { /* no-op test fixture */ }
        @Override public void unregisterAction(String id) { /* no-op test fixture */ }
        @Override public void replaceAction(String id, AnAction action) { /* no-op test fixture */ }
        @Override public String[] getActionIds(String prefix) { return new String[0]; }
        @Override public List<String> getActionIdList(String prefix) { return List.of(); }
        @Override public boolean isGroup(String id) { return false; }
        @Override public AnAction getActionOrStub(String id) { return null; }
        @Override public void addTimerListener(TimerListener listener) { /* no-op test fixture */ }
        @Override public void removeTimerListener(TimerListener listener) { /* no-op test fixture */ }
        @Override public ActionCallback tryToExecute(AnAction action, InputEvent inputEvent,
                                                     Component contextComponent, String place,
                                                     boolean now) { return ActionCallback.DONE; }
        @Override public void addAnActionListener(AnActionListener listener) { /* no-op test fixture */ }
        @Override public KeyboardShortcut getKeyboardShortcut(String actionId) { return null; }
    }

    private static Project projectAt(Path root) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, args) -> "getBasePath".equals(method.getName()) ? root.toString() : null);
    }
}
