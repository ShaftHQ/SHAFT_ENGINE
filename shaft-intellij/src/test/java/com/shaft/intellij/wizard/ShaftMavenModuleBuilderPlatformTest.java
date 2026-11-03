package com.shaft.intellij.wizard;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.nio.file.Files;
import java.nio.file.Path;

/** Platform coverage for ModuleBuilder IDE glue (setupRootModel / show tool window). */
public class ShaftMavenModuleBuilderPlatformTest extends BasePlatformTestCase {
    public void testShowShaftToolWindowIsSafeWhenStripeMissing() {
        ShaftMavenModuleBuilder.showShaftToolWindow(getProject());
        PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    }

    public void testSetupRootModelWritesScaffoldIntoContentPath() throws Exception {
        Path root = Path.of(getProject().getBasePath());
        ShaftMavenModuleBuilder builder = new ShaftMavenModuleBuilder();
        builder.setName("shaft-demo");
        builder.setModuleFilePath(root.resolve("shaft-demo.iml").toString());
        builder.setContentEntryPath(root.toString());

        Module module = getModule();
        ModifiableRootModel model = ModuleRootManager.getInstance(module).getModifiableModel();
        try {
            builder.setupRootModel(model);
        } finally {
            model.dispose();
        }
        assertTrue(Files.isRegularFile(root.resolve("pom.xml")));
    }

    public void testShouldOpenAssistantWithRealModule() {
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        settings.setOpenAssistantAfterCreate(true);
        assertTrue(ShaftMavenModuleBuilder.shouldOpenAssistant(getModule(), settings));
        settings.setOpenAssistantAfterCreate(false);
        assertFalse(ShaftMavenModuleBuilder.shouldOpenAssistant(getModule(), settings));
    }

    public void testAfterCommitOpensOrSkipsToolWindow() {
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        settings.setOpenAssistantAfterCreate(false);
        assertSame(getModule(), ShaftMavenModuleBuilder.afterCommit(getProject(), getModule(), settings));

        settings.setOpenAssistantAfterCreate(true);
        assertSame(getModule(), ShaftMavenModuleBuilder.afterCommit(getProject(), getModule(), settings));
        PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();

        assertNull(ShaftMavenModuleBuilder.afterCommit(getProject(), null, settings));
    }
}
