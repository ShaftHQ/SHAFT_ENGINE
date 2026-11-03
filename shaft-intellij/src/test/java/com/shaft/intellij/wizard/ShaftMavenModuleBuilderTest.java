package com.shaft.intellij.wizard;

import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.openapi.util.Disposer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.swing.JComponent;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import com.intellij.openapi.options.ConfigurationException;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMavenModuleBuilderTest {
    @Test
    void exposesWizardMetadataAndCustomStep() {
        ShaftMavenModuleBuilder builder = new ShaftMavenModuleBuilder();
        assertEquals("SHAFT_MAVEN_TEST_PROJECT", builder.getBuilderId());
        assertEquals("SHAFT", builder.getPresentableName());
        assertEquals("SHAFT", builder.getGroupName());
        assertTrue(builder.getDescription().contains("Maven"));
        assertNotNull(builder.getNodeIcon());
        assertNotNull(builder.getModuleType());

        var disposable = Disposer.newDisposable();
        try {
            ModuleWizardStep step = builder.getCustomOptionsStep(null, disposable);
            assertNotNull(step);
            JComponent component = step.getComponent();
            assertNotNull(component);
            assertTrue(component.getComponentCount() > 0);
            step.updateDataModel();
            step.disposeUIResources();
        } finally {
            Disposer.dispose(disposable);
        }
    }

    @Test
    void setupRootModelWritesScaffold(@TempDir Path root) throws Exception {
        ShaftMavenModuleBuilder builder = new ShaftMavenModuleBuilder();
        // content entry path is package-private via setter on ModuleBuilder
        builder.setName("demo");
        builder.setModuleFilePath(root.resolve("demo.iml").toString());
        builder.setContentEntryPath(root.toString());

        // Exercise scaffold path used by setupRootModel without a real ModifiableRootModel.
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        settings.setGroupId("org.example");
        settings.setArtifactId("demo");
        settings.setVersion("1.2.3");
        settings.setOpenAssistantAfterCreate(false);
        ShaftMavenProjectScaffold.write(root, settings);
        assertTrue(Files.isRegularFile(root.resolve("pom.xml")));
        assertEquals("org.example", settings.getGroupId());
        assertEquals("demo", settings.getArtifactId());
        assertEquals("1.2.3", settings.getVersion());
        assertTrue(!settings.isOpenAssistantAfterCreate());
    }

    @Test
    void writeScaffoldForContentPathCreatesFiles(@TempDir Path root) throws Exception {
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        settings.setGroupId("com.acme");
        settings.setArtifactId("suite");
        ShaftMavenModuleBuilder.writeScaffoldForContentPath(root.toString(), settings);
        assertTrue(Files.isRegularFile(root.resolve("pom.xml")));
    }

    @Test
    void writeScaffoldForContentPathRejectsNull() {
        assertThrows(ConfigurationException.class,
                () -> ShaftMavenModuleBuilder.writeScaffoldForContentPath(null, new ShaftMavenProjectSettings()));
    }


    @Test
    void shouldOpenAssistantRequiresModuleAndFlag() {
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        settings.setOpenAssistantAfterCreate(true);
        assertFalse(ShaftMavenModuleBuilder.shouldOpenAssistant(null, settings));
        settings.setOpenAssistantAfterCreate(false);
        assertFalse(ShaftMavenModuleBuilder.shouldOpenAssistant(null, settings));
    }

    @Test
    void writeScaffoldForContentPathWrapsIoErrors(@TempDir Path root) throws Exception {
        Path blocked = root.resolve("not-a-directory");
        Files.writeString(blocked, "file");
        ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();
        assertThrows(ConfigurationException.class,
                () -> ShaftMavenModuleBuilder.writeScaffoldForContentPath(blocked.toString(), settings));
    }

    @Test
    void showExistingToolWindowNoopsForNull() {
        assertFalse(ShaftMavenModuleBuilder.showExistingToolWindow(null));
    }
}
