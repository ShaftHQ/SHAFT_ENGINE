package com.shaft.intellij.wizard;

import com.intellij.ide.util.projectWizard.ModuleBuilder;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.module.ModifiableModuleModel;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
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
    private static final String TOOL_WINDOW_ID = "SHAFT";

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
        writeScaffoldForContentPath(getContentEntryPath(), settings);
        doAddContentEntry(modifiableRootModel);
    }

    static void writeScaffoldForContentPath(@Nullable String contentPath,
                                            @NotNull ShaftMavenProjectSettings projectSettings)
            throws ConfigurationException {
        if (contentPath == null) {
            throw new ConfigurationException("Project path is missing");
        }
        try {
            ShaftMavenProjectScaffold.write(Path.of(contentPath), projectSettings);
        } catch (IOException error) {
            throw new ConfigurationException("Could not create SHAFT project files: " + error.getMessage());
        }
    }

    @Override
    public @Nullable ModuleWizardStep getCustomOptionsStep(WizardContext context, Disposable parentDisposable) {
        return new ShaftMavenWizardStep(settings);
    }

    @Override
    public @Nullable Module commitModule(@NotNull Project project, @Nullable ModifiableModuleModel model) {
        Module module = super.commitModule(project, model);
        if (shouldOpenAssistant(module, settings)) {
            showShaftToolWindow(project);
        }
        return module;
    }

    static boolean shouldOpenAssistant(@Nullable Module module, @NotNull ShaftMavenProjectSettings projectSettings) {
        return module != null && projectSettings.isOpenAssistantAfterCreate();
    }

    static void showShaftToolWindow(@NotNull Project project) {
        ToolWindowManager manager = ToolWindowManager.getInstance(project);
        manager.invokeLater(() -> {
            ToolWindow toolWindow = manager.getToolWindow(TOOL_WINDOW_ID);
            if (toolWindow != null) {
                toolWindow.show();
            }
        });
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
            buildForm();
        }

        private void buildForm() {
            addLabeledField(0, "GroupId", groupId);
            addLabeledField(1, "ArtifactId", artifactId);
            addLabeledField(2, "Version", version);
            GridBagConstraints checkboxConstraints = baseConstraints();
            checkboxConstraints.gridx = 0;
            checkboxConstraints.gridy = 3;
            checkboxConstraints.gridwidth = 2;
            panel.add(openAssistant, checkboxConstraints);
        }

        private void addLabeledField(int row, @NotNull String label, @NotNull JComponent field) {
            GridBagConstraints labelConstraints = baseConstraints();
            labelConstraints.gridx = 0;
            labelConstraints.gridy = row;
            panel.add(new JLabel(label), labelConstraints);
            GridBagConstraints fieldConstraints = baseConstraints();
            fieldConstraints.gridx = 1;
            fieldConstraints.gridy = row;
            fieldConstraints.fill = GridBagConstraints.HORIZONTAL;
            fieldConstraints.weightx = 1;
            panel.add(field, fieldConstraints);
        }

        private static @NotNull GridBagConstraints baseConstraints() {
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.insets = new Insets(4, 4, 4, 4);
            constraints.anchor = GridBagConstraints.WEST;
            return constraints;
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
