package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class OpenShaftToolWindowActionTest {
    @Test
    void usesBackgroundUpdateThreadAndSetsIcon() {
        OpenShaftToolWindowAction action = new OpenShaftToolWindowAction();
        assertEquals(ActionUpdateThread.BGT, action.getActionUpdateThread());
        assertNotNull(action.getTemplatePresentation().getIcon());
    }
}
