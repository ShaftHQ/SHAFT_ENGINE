package com.shaft.intellij.testrunner;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.RunnerSettings;
import com.intellij.execution.configurations.SimpleJavaParameters;
import com.intellij.openapi.externalSystem.service.execution.ExternalSystemRunConfiguration;
import com.intellij.openapi.externalSystem.service.execution.configuration.ExternalSystemRunConfigurationExtension;
import com.intellij.openapi.options.SettingsEditor;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.plugins.gradle.service.execution.GradleRunConfiguration;

/**
 * SHAFT run overrides for Gradle run configurations (#6428). The platform forwards the VM parameters added in
 * {@link #updateVMParameters} to Gradle test JVMs as {@code -D} system properties, matching the JUnit and TestNG
 * extensions. Registered only when the bundled Gradle plugin is enabled ({@code io.github.shafthq.shaft-withGradle.xml}).
 */
public final class ShaftGradleRunConfigurationExtension extends ExternalSystemRunConfigurationExtension {
    @Override
    public boolean isApplicableFor(@NotNull ExternalSystemRunConfiguration configuration) {
        return configuration instanceof GradleRunConfiguration
                && ShaftRunConfigurationExtensionSupport.isApplicableFor(configuration);
    }

    @Override
    public boolean isEnabledFor(@NotNull ExternalSystemRunConfiguration configuration, RunnerSettings runnerSettings) {
        return true;
    }

    @Override
    protected String getEditorTitle() {
        return ShaftRunConfigurationExtensionSupport.EDITOR_TITLE;
    }

    @Override
    protected @NotNull String getSerializationId() {
        return ShaftRunConfigurationExtensionSupport.SERIALIZATION_ID;
    }

    @Override
    @SuppressWarnings("unchecked")
    protected <P extends ExternalSystemRunConfiguration> SettingsEditor<P> createEditor(@NotNull P configuration) {
        return ShaftRunConfigurationExtensionSupport.createEditor();
    }

    @Override
    protected void readExternal(@NotNull ExternalSystemRunConfiguration configuration, @NotNull Element element) {
        ShaftRunConfigurationExtensionSupport.readExternal(configuration, element);
    }

    @Override
    protected void writeExternal(@NotNull ExternalSystemRunConfiguration configuration, @NotNull Element element) {
        ShaftRunConfigurationExtensionSupport.writeExternal(configuration, element);
    }

    @Override
    public void updateVMParameters(@NotNull ExternalSystemRunConfiguration configuration,
                                   @NotNull SimpleJavaParameters javaParameters,
                                   RunnerSettings settings, @NotNull Executor executor) {
        ShaftRunConfigurationExtensionSupport.applyOverrides(configuration, javaParameters);
    }

    @Override
    protected void patchCommandLine(@NotNull ExternalSystemRunConfiguration configuration, RunnerSettings runnerSettings,
                                    @NotNull GeneralCommandLine cmdLine, @NotNull String runnerId) {
        // Gradle runs are launched by the external-system task manager, not this command line.
    }
}
