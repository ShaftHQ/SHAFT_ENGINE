package com.shaft.intellij.testrunner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.RunConfigurationExtension;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.JavaParameters;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.RunnerSettings;
import com.intellij.openapi.options.SettingsEditor;
import com.theoryinpractice.testng.configuration.TestNGConfiguration;
import org.jdom.Element;

/**
 * SHAFT's "Override SHAFT execution properties for this run" tab (issue #3659) on IntelliJ's
 * native {@link TestNGConfiguration} editor: a per-run-configuration browser/headless/extra-VM-arg
 * override, applied only when explicitly opted in (default: untouched, inherits {@code
 * custom.properties}).
 * <p>
 * Registered only when the bundled TestNG-J plugin is enabled (see {@code
 * io.github.shafthq.shaft-withTestNG.xml}), mirroring {@link ShaftTestNgRunConfigurationProducer}:
 * {@link TestNGConfiguration} lives in that optional plugin. All behavior is shared with {@link
 * ShaftJUnitRunConfigurationExtension} through {@link ShaftRunConfigurationExtensionSupport}; this
 * class only narrows {@code isApplicableFor} to {@link TestNGConfiguration} instances.
 */
public final class ShaftTestNgRunConfigurationExtension extends RunConfigurationExtension {
    @Override
    public boolean isApplicableFor(RunConfigurationBase configuration) {
        return configuration instanceof TestNGConfiguration
                && ShaftRunConfigurationExtensionSupport.isApplicableFor(configuration);
    }

    @Override
    public boolean isEnabledFor(RunConfigurationBase configuration, RunnerSettings runnerSettings) {
        return true;
    }

    @Override
    public String getEditorTitle() {
        return ShaftRunConfigurationExtensionSupport.EDITOR_TITLE;
    }

    @Override
    public String getSerializationId() {
        return ShaftRunConfigurationExtensionSupport.SERIALIZATION_ID;
    }

    @Override
    public SettingsEditor createEditor(RunConfigurationBase configuration) {
        return ShaftRunConfigurationExtensionSupport.createEditor();
    }

    @Override
    public void readExternal(RunConfigurationBase configuration, Element element) {
        ShaftRunConfigurationExtensionSupport.readExternal(configuration, element);
    }

    @Override
    public void writeExternal(RunConfigurationBase configuration, Element element) {
        ShaftRunConfigurationExtensionSupport.writeExternal(configuration, element);
    }

    // Compiler-mandated (RunConfigurationExtension#updateJavaParameters remains abstract).
    @Override
    public void updateJavaParameters(RunConfigurationBase configuration, JavaParameters javaParameters,
                                      RunnerSettings runnerSettings) throws ExecutionException {
        ShaftRunConfigurationExtensionSupport.applyOverrides(configuration, javaParameters);
    }

    // Not compiler-mandated here (RunConfigurationExtension already gives patchCommandLine a
    // concrete no-op default), but applied redundantly alongside updateJavaParameters since we
    // don't know which launch path a given runner actually exercises; idempotent -D overrides are
    // harmless applied twice.
    @Override
    protected void patchCommandLine(RunConfigurationBase configuration, RunnerSettings runnerSettings,
                                     GeneralCommandLine commandLine, String runnerId) throws ExecutionException {
        ShaftRunConfigurationExtensionSupport.applyOverrides(configuration, commandLine);
    }
}
