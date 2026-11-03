package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.TestActionEvent;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

/** Platform fixture coverage for W1.7 Open tool-window visibility and show path. */
public class OpenShaftToolWindowActionPlatformTest extends BasePlatformTestCase {
    public void testUpdateHidesActionWithoutShaftMarkers() {
        OpenShaftToolWindowAction action = new OpenShaftToolWindowAction();
        AnActionEvent event = TestActionEvent.createTestEvent(
                action,
                SimpleDataContext.builder().add(CommonDataKeys.PROJECT, getProject()).build());
        action.update(event);
        assertFalse(event.getPresentation().isEnabledAndVisible());
        action.actionPerformed(event);
    }

    public void testShowToolWindowIsSafeWhenStripeMissing() {
        OpenShaftToolWindowAction.showToolWindow(getProject());
        PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    }
}
