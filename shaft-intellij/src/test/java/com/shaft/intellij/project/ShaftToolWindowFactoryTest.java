package com.shaft.intellij.project;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.ShaftToolWindowFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftToolWindowFactoryTest {
    @Test
    void factoryAvailabilityUsesShaftProjectDetection(@TempDir Path root) throws Exception {
        Project project = projectAt(root);
        assertFalse(new ShaftToolWindowFactory().shouldBeAvailable(project));

        Files.writeString(root.resolve("pom.xml"), "<dependency>io.github.shafthq:shaft-engine</dependency>");
        ShaftProjectDetector.clearCacheForTests();

        assertTrue(new ShaftToolWindowFactory().shouldBeAvailable(project));
    }

    private static Project projectAt(Path root) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, args) -> "getBasePath".equals(method.getName()) ? root.toString() : null);
    }
}
