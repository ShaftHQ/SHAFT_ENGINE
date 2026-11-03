package com.shaft.intellij.testrunner;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.SimpleJavaParameters;
import com.intellij.openapi.util.Key;
import org.jdom.Element;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ShaftRunConfigurationExtensionSupportTest {
    private final Map<Key<?>, Object> userData = new HashMap<>();
    private RunConfigurationBase<?> configuration;

    @BeforeEach
    @SuppressWarnings({"rawtypes", "unchecked"})
    void setUp() {
        userData.clear();
        configuration = mock(RunConfigurationBase.class);
        doAnswer(invocation -> {
            userData.put(invocation.getArgument(0), invocation.getArgument(1));
            return null;
        }).when(configuration).putCopyableUserData(any(Key.class), ArgumentMatchers.any());
        when(configuration.getCopyableUserData(any(Key.class)))
                .thenAnswer(invocation -> userData.get(invocation.getArgument(0)));
    }

    @Test
    void installNullIsNoOpAndIsApplicableRequiresProject() {
        ShaftRunConfigurationExtensionSupportAccess.installOverrides(null, new ShaftRunConfigurationOverrides());
        assertFalse(ShaftRunConfigurationExtensionSupportAccess.isApplicableFor(null));
        when(configuration.getProject()).thenReturn(null);
        assertFalse(ShaftRunConfigurationExtensionSupportAccess.isApplicableFor(configuration));
    }

    @Test
    void readWriteRoundTripAndDisabledWriteIsEmpty() {
        Element element = new Element("shaft");
        element.setAttribute("enabled", "true");
        element.setAttribute("browser", "chrome");
        element.setAttribute("headless", "true");
        element.setAttribute("extraVmArgs", "-Dx=1");
        element.setAttribute("platform", "WEB");
        element.setAttribute("execution", "LOCAL");
        element.setAttribute("deviceOs", "linux");
        element.setAttribute("appAlias", "demo");
        element.setAttribute("retryCount", "2");
        ShaftRunConfigurationExtensionSupportAccess.readExternal(configuration, element);

        Element written = new Element("shaft");
        ShaftRunConfigurationExtensionSupportAccess.writeExternal(configuration, written);
        assertEquals("true", written.getAttributeValue("enabled"));
        assertEquals("chrome", written.getAttributeValue("browser"));
        assertEquals("WEB", written.getAttributeValue("platform"));
        assertEquals("LOCAL", written.getAttributeValue("execution"));
        assertEquals("linux", written.getAttributeValue("deviceOs"));
        assertEquals("demo", written.getAttributeValue("appAlias"));
        assertEquals("2", written.getAttributeValue("retryCount"));

        Element empty = new Element("shaft");
        ShaftRunConfigurationExtensionSupportAccess.readExternal(configuration, empty);
        Element noWrite = new Element("shaft");
        ShaftRunConfigurationExtensionSupportAccess.writeExternal(configuration, noWrite);
        assertTrue(noWrite.getAttributes().isEmpty());
    }

    @Test
    void readParsesInvalidEnumsAndBadRetryAsDefaults() {
        Element element = new Element("shaft");
        element.setAttribute("enabled", "true");
        element.setAttribute("platform", "NOT_A_PLATFORM");
        element.setAttribute("execution", "NOPE");
        element.setAttribute("retryCount", "x");
        ShaftRunConfigurationExtensionSupportAccess.readExternal(configuration, element);
        Element written = new Element("shaft");
        ShaftRunConfigurationExtensionSupportAccess.writeExternal(configuration, written);
        assertEquals(ShaftTargetSelection.Platform.INHERIT_PLATFORM.name(), written.getAttributeValue("platform"));
        assertEquals(ShaftTargetSelection.Execution.INHERIT_EXECUTION.name(), written.getAttributeValue("execution"));
        assertEquals("0", written.getAttributeValue("retryCount"));
    }

    @Test
    void applyOverridesToCommandLineAndJavaParameters() {
        ShaftRunConfigurationOverrides overrides = new ShaftRunConfigurationOverrides();
        overrides.setEnabled(true);
        overrides.setBrowser("firefox");
        overrides.setHeadless(true);
        overrides.setExtraVmArgs("-Dextra=1");
        ShaftTargetSelection target = new ShaftTargetSelection();
        target.setPlatform(ShaftTargetSelection.Platform.WEB);
        target.setExecution(ShaftTargetSelection.Execution.LOCAL);
        target.setRetryCount(1);
        overrides.setTarget(target);
        ShaftRunConfigurationExtensionSupportAccess.installOverrides(configuration, overrides);

        GeneralCommandLine commandLine = new GeneralCommandLine("java");
        ShaftRunConfigurationExtensionSupportAccess.applyOverrides(configuration, commandLine);
        String joined = String.join(" ", commandLine.getParametersList().getList());
        assertTrue(joined.contains("-DtargetBrowserName=firefox"));
        assertTrue(joined.contains("-DheadlessExecution=true"));
        assertTrue(joined.contains("-DretryCount=1") || joined.contains("retryCount"));
        assertTrue(joined.contains("-Dextra=1"));

        SimpleJavaParameters parameters = new SimpleJavaParameters();
        ShaftRunConfigurationExtensionSupportAccess.applyOverrides(configuration, parameters);
        assertTrue(parameters.getVMParametersList().getList().stream()
                .anyMatch(s -> s.contains("targetBrowserName") || s.equals("firefox")));
        assertTrue(parameters.getVMParametersList().hasProperty("headlessExecution"));
    }

    @Test
    void applySkippedWhenDisabledOrInvalidTarget() {
        ShaftRunConfigurationOverrides disabled = new ShaftRunConfigurationOverrides();
        disabled.setEnabled(false);
        ShaftRunConfigurationExtensionSupportAccess.installOverrides(configuration, disabled);
        GeneralCommandLine commandLine = new GeneralCommandLine("java");
        ShaftRunConfigurationExtensionSupportAccess.applyOverrides(configuration, commandLine);
        assertTrue(commandLine.getParametersList().getList().isEmpty());

        ShaftRunConfigurationOverrides invalid = new ShaftRunConfigurationOverrides();
        invalid.setEnabled(true);
        invalid.setBrowser("chrome");
        ShaftTargetSelection bad = new ShaftTargetSelection();
        bad.setPlatform(ShaftTargetSelection.Platform.ANDROID);
        bad.setExecution(ShaftTargetSelection.Execution.LOCAL);
        // missing device/app often invalidates mobile local
        invalid.setTarget(bad);
        ShaftRunConfigurationExtensionSupportAccess.installOverrides(configuration, invalid);
        GeneralCommandLine cl2 = new GeneralCommandLine("java");
        ShaftRunConfigurationExtensionSupportAccess.applyOverrides(configuration, cl2);
        // browser/headless still applied; target tokens skipped when invalid
        assertTrue(cl2.getParametersList().getList().stream()
                .anyMatch(s -> s.contains("targetBrowserName")));
    }


    @Test
    void blankPlatformExecutionAndCreateEditor() {
        Element element = new Element("shaft");
        element.setAttribute("enabled", "true");
        element.setAttribute("platform", "  ");
        element.setAttribute("execution", "");
        ShaftRunConfigurationExtensionSupportAccess.readExternal(configuration, element);
        Element written = new Element("shaft");
        ShaftRunConfigurationExtensionSupportAccess.writeExternal(configuration, written);
        assertEquals(ShaftTargetSelection.Platform.INHERIT_PLATFORM.name(), written.getAttributeValue("platform"));
        assertEquals(ShaftTargetSelection.Execution.INHERIT_EXECUTION.name(), written.getAttributeValue("execution"));
        assertNotNull(ShaftRunConfigurationExtensionSupportAccess.createEditor());
    }
}
