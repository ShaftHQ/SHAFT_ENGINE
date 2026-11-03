package com.shaft.intellij.wizard;

import com.intellij.ide.util.projectWizard.ModuleBuilder;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.module.ModifiableModuleModel;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.shaft.intellij.ui.ShaftIcons;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.IOException;
import java.nio.file.Path;

/**
 * New Project / Module wizard builder for SHAFT Maven TestNG projects (W2.1).
 */
public final class ShaftMavenModuleBuilder extends ModuleBuilder {
    private final ShaftMavenProjectSettings settings = new ShaftMavenProjectSettings();

    @Override
    public @NotNull String getBuilderId() {
        return "SHAFT_MAVEN_TEST_PROJECT";
    }

    @Override
    public @NotNull String getPresentableName() {
        return "SHAFT";
    }

    @Override
    public @NotNull String getDescription() {
        return "Maven TestNG project with SHAFT Engine, custom.properties, and secrets.properties.example.";
    }

    @Override
    public @NotNull String getGroupName() {
        return "SHAFT";
    }

    @Override
    public @Nullable Icon getNodeIcon() {
        return ShaftIcons.TOOL_WINDOW;
    }

    @Override
    public ModuleType<?> getModuleType() {
        return ModuleType.EMPTY;
    }

    @Override
    public void setupRootModel(@NotNull ModifiableRootModel modifiableRootModel) throws ConfigurationException {
        String contentPath = getContentEntryPath();
        if (contentPath == null) {
            throw new ConfigurationException("Project path is missing");
        }
        try {
            ShaftMavenProjectScaffold.write(Path.of(contentPath), settings);
        } catch (IOException error) {
            throw new ConfigurationException("Could not create SHAFT project files: " + error.getMessage());
        }
        doAddContentEntry(modifiableRootModel);
    }

    @Override
    public @Nullable ModuleWizardStep getCustomOptionsStep(WizardContext context, Disposable parentDisposable) {
        return new ShaftMavenWizardStep(settings);
    }


    @Override
    public @Nullable Module commitModule(@NotNull Project project, @Nullable ModifiableModuleModel model) {
        Module module = super.commitModule(project, model);
        if (module != null && settings.isOpenAssistantAfterCreate()) {
            ToolWindowManager.getInstance(project).invokeLater(() -> {
                ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
                if (toolWindow != null) {
                    toolWindow.show();
                }
            });
        }
        return module;
    }

    ShaftMavenProjectSettings settings() {
        return settings;
    }

    private static final class ShaftMavenWizardStep extends ModuleWizardStep {
        private final ShaftMavenProjectSettings settings;
        private final JTextField groupId = new JTextField();
        private final JTextField artifactId = new JTextField();
        private final JTextField version = new JTextField();
        private final JCheckBox openAssistant = new JCheckBox("Open SHAFT Assistant after create");
        private final JPanel panel = new JPanel(new GridBagLayout());

        private ShaftMavenWizardStep(ShaftMavenProjectSettings settings) {
            this.settings = settings;
            groupId.setText(settings.getGroupId());
            artifactId.setText(settings.getArtifactId());
            version.setText(settings.getVersion());
            openAssistant.setSelected(settings.isOpenAssistantAfterCreate());
            GridBagConstraints c = new GridBagConstraints();
            c.insets = new Insets(4, 4, 4, 4);
            c.anchor = GridBagConstraints.WEST;
            c.gridx = 0;
            c.gridy = 0;
            panel.add(new JLabel("GroupId"), c);
            c.gridx = 1;
            c.fill = GridBagConstraints.HORIZONTAL;
            c.weightx = 1;
            panel.add(groupId, c);
            c.gridx = 0;
            c.gridy = 1;
            c.weightx = 0;
            c.fill = GridBagConstraints.NONE;
            panel.add(new JLabel("ArtifactId"), c);
            c.gridx = 1;
            c.fill = GridBagConstraints.HORIZONTAL;
            c.weightx = 1;
            panel.add(artifactId, c);
            c.gridx = 0;
            c.gridy = 2;
            c.weightx = 0;
            c.fill = GridBagConstraints.NONE;
            panel.add(new JLabel("Version"), c);
            c.gridx = 1;
            c.fill = GridBagConstraints.HORIZONTAL;
            c.weightx = 1;
            panel.add(version, c);
            c.gridx = 0;
            c.gridy = 3;
            c.gridwidth = 2;
            panel.add(openAssistant, c);
        }

        @Override
        public JComponent getComponent() {
            return panel;
        }

        @Override
        public void updateDataModel() {
            settings.setGroupId(groupId.getText());
            settings.setArtifactId(artifactId.getText());
            settings.setVersion(version.getText());
            settings.setOpenAssistantAfterCreate(openAssistant.isSelected());
        }

        @Override
        public void disposeUIResources() {
            // no-op: fields are GC'd with the wizard step
        }
    }
}
