package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SetupCatalogTest {
    @Test
    void builtInCatalogPinsEveryTargetCapabilityAndOrderedProfileMembership() {
        SetupCatalog catalog = SetupCatalog.builtIn();
        Map<SetupTarget, Set<SetupCapability>> actualCapabilities = new EnumMap<>(SetupTarget.class);
        catalog.targets().forEach(target -> actualCapabilities.put(target.target(), target.capabilities()));

        assertEquals(1, catalog.schemaVersion());
        assertEquals(expectedCapabilities(), actualCapabilities);

        Map<SetupProfile, List<SetupTarget>> actualProfiles = new EnumMap<>(SetupProfile.class);
        catalog.profiles().forEach(profile -> actualProfiles.put(profile.profile(), profile.targets()));
        assertEquals(expectedProfiles(), actualProfiles);
    }

    @Test
    void localAiCatalogAdvertisesShaftOwnedInstallableRuntimeAndModel() {
        SetupCatalog catalog = SetupCatalog.builtIn();
        SetupProfileDefinition localAi = catalog.profiles().stream()
                .filter(profile -> profile.profile() == SetupProfile.LOCAL_AI)
                .findFirst().orElseThrow();

        assertEquals(List.of("MANAGED_LOCAL_AI_RUNTIME", "MANAGED_LOCAL_AI_MODEL"),
                localAi.targets().stream().map(Enum::name).toList());
        assertEquals(List.of(Set.of(SetupCapability.INSTALLABLE), Set.of(SetupCapability.INSTALLABLE)),
                localAi.targets().stream().map(target -> catalog.targets().stream()
                        .filter(definition -> definition.target() == target)
                        .findFirst().orElseThrow().capabilities()).toList());
    }

    @Test
    void valueObjectsAreImmutableAndRejectAmbiguousInput() {
        var mutableTargets = new java.util.ArrayList<>(List.of(SetupTarget.JAVA));
        var profile = new SetupProfileDefinition(SetupProfile.AGENT_TOOLS, "Agent tools", mutableTargets);
        mutableTargets.add(SetupTarget.MAVEN);
        assertEquals(List.of(SetupTarget.JAVA), profile.targets());
        assertThrows(UnsupportedOperationException.class, () -> profile.targets().add(SetupTarget.MAVEN));
        assertThrows(IllegalArgumentException.class, () -> new SetupProfileDefinition(
                SetupProfile.AGENT_TOOLS, "Agent tools", List.of(SetupTarget.JAVA, SetupTarget.JAVA)));
        assertThrows(IllegalArgumentException.class, () -> new SetupProfileDefinition(
                SetupProfile.AGENT_TOOLS, " ", List.of(SetupTarget.JAVA)));
        assertThrows(IllegalArgumentException.class, () -> new SetupProfileDefinition(
                SetupProfile.AGENT_TOOLS, "Agent tools", List.of()));

        var mutableCapabilities = EnumSet.of(SetupCapability.INSTALLABLE);
        var target = new SetupTargetDefinition(SetupTarget.ALLURE, "Allure", mutableCapabilities, "Reports");
        mutableCapabilities.add(SetupCapability.STARTABLE);
        assertEquals(Set.of(SetupCapability.INSTALLABLE), target.capabilities());
        assertThrows(UnsupportedOperationException.class,
                () -> target.capabilities().add(SetupCapability.STARTABLE));
        assertThrows(IllegalArgumentException.class, () -> new SetupTargetDefinition(
                SetupTarget.ALLURE, " ", Set.of(SetupCapability.INSTALLABLE), "Reports"));
        assertThrows(IllegalArgumentException.class, () -> new SetupTargetDefinition(
                SetupTarget.ALLURE, "Allure", Set.of(), "Reports"));
        assertThrows(IllegalArgumentException.class, () -> new SetupTargetDefinition(
                SetupTarget.ALLURE, "Allure", Set.of(SetupCapability.INSTALLABLE), " "));
    }

    @Test
    void catalogRejectsInvalidSchemaDuplicatesAndUndeclaredReferences() {
        var java = definition(SetupTarget.JAVA);
        var maven = definition(SetupTarget.MAVEN);
        var agents = new SetupProfileDefinition(SetupProfile.AGENT_TOOLS, "Agents", List.of(SetupTarget.JAVA));
        assertThrows(IllegalArgumentException.class, () -> new SetupCatalog(0, List.of(java), List.of(agents)));
        assertThrows(IllegalArgumentException.class, () -> new SetupCatalog(1, List.of(java, java), List.of(agents)));
        assertThrows(IllegalArgumentException.class, () -> new SetupCatalog(1, List.of(java), List.of(agents, agents)));
        assertThrows(IllegalArgumentException.class, () -> new SetupCatalog(1, List.of(maven), List.of(agents)));
    }

    private static SetupTargetDefinition definition(SetupTarget target) {
        return new SetupTargetDefinition(target, target.name(), Set.of(SetupCapability.HOST_PREREQUISITE), "Test");
    }

    private static Map<SetupTarget, Set<SetupCapability>> expectedCapabilities() {
        Map<SetupTarget, Set<SetupCapability>> expected = new EnumMap<>(SetupTarget.class);
        put(expected, SetupCapability.INSTALLABLE, SetupTarget.NODE, SetupTarget.ALLURE,
                SetupTarget.PLAYWRIGHT_CHROMIUM, SetupTarget.PLAYWRIGHT_FIREFOX, SetupTarget.PLAYWRIGHT_WEBKIT,
                SetupTarget.LIGHTHOUSE, SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_UIAUTOMATOR2_DRIVER,
                SetupTarget.APPIUM_XCUITEST_DRIVER, SetupTarget.APPIUM_WINDOWS_DRIVER, SetupTarget.APPIUM_FLUTTER_DRIVER,
                SetupTarget.ANDROID_SDK, SetupTarget.OCR_TESSDATA, SetupTarget.AGENT_CLI,
                SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupTarget.MANAGED_LOCAL_AI_MODEL);
        put(expected, Set.of(SetupCapability.INSTALLABLE, SetupCapability.STARTABLE), SetupTarget.APPIUM_SERVER,
                SetupTarget.ANDROID_EMULATOR, SetupTarget.SELENIUM_GRID, SetupTarget.HEALENIUM,
                SetupTarget.REPORT_PORTAL, SetupTarget.BROWSERSTACK_LOCAL);
        expected.put(SetupTarget.WINAPPDRIVER, Set.of(SetupCapability.INSTALLABLE, SetupCapability.STARTABLE,
                SetupCapability.PRIVILEGED));
        expected.put(SetupTarget.IOS_SIMULATOR, Set.of(SetupCapability.STARTABLE));
        expected.put(SetupTarget.OLLAMA, Set.of(SetupCapability.HOST_PREREQUISITE,
                SetupCapability.PREWARMABLE, SetupCapability.STARTABLE));
        put(expected, SetupCapability.HOST_PREREQUISITE, SetupTarget.JAVA, SetupTarget.MAVEN,
                SetupTarget.PYTHON, SetupTarget.DOCKER, SetupTarget.LM_STUDIO);
        expected.put(SetupTarget.SELENIUM_BROWSER,
                Set.of(SetupCapability.HOST_PREREQUISITE, SetupCapability.PREWARMABLE));
        expected.put(SetupTarget.XCODE,
                Set.of(SetupCapability.HOST_PREREQUISITE, SetupCapability.PRIVILEGED));
        expected.put(SetupTarget.FFMPEG, Set.of(SetupCapability.INSTALLABLE, SetupCapability.PROVIDED));
        put(expected, SetupCapability.PROVIDED, SetupTarget.OPENCV);
        return expected;
    }

    private static Map<SetupProfile, List<SetupTarget>> expectedProfiles() {
        Map<SetupProfile, List<SetupTarget>> expected = new EnumMap<>(SetupProfile.class);
        expected.put(SetupProfile.WEB_LOCAL, List.of(SetupTarget.SELENIUM_BROWSER));
        expected.put(SetupProfile.PLAYWRIGHT, List.of(SetupTarget.NODE, SetupTarget.PLAYWRIGHT_CHROMIUM,
                SetupTarget.PLAYWRIGHT_FIREFOX, SetupTarget.PLAYWRIGHT_WEBKIT, SetupTarget.FFMPEG));
        expected.put(SetupProfile.LIGHTHOUSE, List.of(SetupTarget.NODE, SetupTarget.LIGHTHOUSE));
        expected.put(SetupProfile.MOBILE_ANDROID, List.of(SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_UIAUTOMATOR2_DRIVER,
                SetupTarget.ANDROID_SDK, SetupTarget.ANDROID_EMULATOR));
        expected.put(SetupProfile.MOBILE_IOS, List.of(SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_XCUITEST_DRIVER,
                SetupTarget.XCODE, SetupTarget.IOS_SIMULATOR));
        expected.put(SetupProfile.MOBILE_WINDOWS, List.of(SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_WINDOWS_DRIVER, SetupTarget.WINAPPDRIVER));
        expected.put(SetupProfile.SELENIUM_GRID, List.of(SetupTarget.DOCKER, SetupTarget.SELENIUM_GRID));
        expected.put(SetupProfile.REPORTING, List.of(SetupTarget.NODE, SetupTarget.ALLURE));
        expected.put(SetupProfile.OCR, List.of(SetupTarget.OCR_TESSDATA));
        expected.put(SetupProfile.HEALENIUM, List.of(SetupTarget.DOCKER, SetupTarget.HEALENIUM));
        expected.put(SetupProfile.REPORT_PORTAL, List.of(SetupTarget.DOCKER, SetupTarget.REPORT_PORTAL));
        expected.put(SetupProfile.BROWSERSTACK_LOCAL, List.of(SetupTarget.BROWSERSTACK_LOCAL));
        expected.put(SetupProfile.AGENT_TOOLS, List.of(SetupTarget.JAVA, SetupTarget.MAVEN, SetupTarget.PYTHON,
                SetupTarget.NODE, SetupTarget.AGENT_CLI));
        expected.put(SetupProfile.LOCAL_AI, List.of(SetupTarget.MANAGED_LOCAL_AI_RUNTIME,
                SetupTarget.MANAGED_LOCAL_AI_MODEL));
        return expected;
    }

    private static void put(Map<SetupTarget, Set<SetupCapability>> target, SetupCapability capability,
                            SetupTarget... keys) {
        put(target, Set.of(capability), keys);
    }

    private static void put(Map<SetupTarget, Set<SetupCapability>> target, Set<SetupCapability> capabilities,
                            SetupTarget... keys) {
        for (SetupTarget key : keys) {
            target.put(key, capabilities);
        }
    }
}
