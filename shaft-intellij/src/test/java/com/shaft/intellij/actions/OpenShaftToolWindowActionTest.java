package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.project.Project;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;

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
        assertFalse(OpenShaftToolWindowAction.isAvailable(projectAt(root)));

        Path shaftRoot = Files.createDirectory(root.resolve("shaft"));
        Files.writeString(shaftRoot.resolve("pom.xml"), "<dependency>io.github.shafthq:shaft-engine</dependency>");
        assertTrue(OpenShaftToolWindowAction.isAvailable(projectAt(shaftRoot)));
    }

    private static Project projectAt(Path root) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, args) -> "getBasePath".equals(method.getName()) ? root.toString() : null);
    }
}
