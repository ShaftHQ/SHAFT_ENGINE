package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SetupCatalogProviderCoverageTest {
    private static final Set<SetupProfile> HOST_ONLY_PROFILES = Set.of(SetupProfile.WEB_LOCAL);
    private static final Set<SetupProfile> SERVICE_LOADER_PROFILES = Set.of(SetupProfile.LOCAL_AI);
    private static final Set<SetupTarget> REMAINING_INSTALLABLE_GAPS = Set.of(
            SetupTarget.APPIUM_FLUTTER_DRIVER, SetupTarget.OPENCV);

    @Test
    void everyCatalogProfileIsOwnedOrExplicitlyAllowlisted() {
        InfrastructureSetupService linux = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        InfrastructureSetupService macos = InfrastructureSetupService.builtIn(
                SetupPlatform.MACOS, SetupArchitecture.X64);
        InfrastructureSetupService windows = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);

        Set<SetupProfile> catalogProfiles = catalogProfiles();
        assertEquals(EnumSet.allOf(SetupProfile.class), catalogProfiles);

        for (SetupProfile profile : catalogProfiles) {
            if (HOST_ONLY_PROFILES.contains(profile) || SERVICE_LOADER_PROFILES.contains(profile)) {
                assertFalse(linux.supports(profile),
                        profile + " is allowlisted as host/ServiceLoader-owned and must not be hard-wired");
                continue;
            }
            if (profile == SetupProfile.MOBILE_IOS) {
                assertTrue(macos.supports(profile), "MOBILE_IOS must be owned on macOS");
                assertFalse(linux.supports(profile));
                continue;
            }
            if (profile == SetupProfile.MOBILE_WINDOWS) {
                assertTrue(windows.supports(profile), "MOBILE_WINDOWS must be owned on Windows");
                assertFalse(linux.supports(profile));
                continue;
            }
            assertTrue(linux.supports(profile),
                    profile + " is advertised in the catalog but has no executable provider");
        }
    }

    @Test
    void everyInstallableCatalogTargetIsPlannedOrExplicitlyAllowlisted(@TempDir Path temp) {
        Set<SetupTarget> plannedInstall = plannedInstallTargets(temp);
        Set<SetupTarget> serviceLoaderTargets = EnumSet.noneOf(SetupTarget.class);
        SetupCatalog.builtIn().profiles().stream()
                .filter(profile -> SERVICE_LOADER_PROFILES.contains(profile.profile()))
                .forEach(profile -> serviceLoaderTargets.addAll(profile.targets()));
        Set<SetupTarget> installable = EnumSet.noneOf(SetupTarget.class);
        SetupCatalog.builtIn().targets().forEach(definition -> {
            if (definition.capabilities().contains(SetupCapability.INSTALLABLE)
                    && !definition.capabilities().contains(SetupCapability.PRIVILEGED)) {
                installable.add(definition.target());
            }
        });

        for (SetupTarget target : installable) {
            if (REMAINING_INSTALLABLE_GAPS.contains(target)) {
                assertFalse(plannedInstall.contains(target),
                        target + " is allowlisted as a remaining gap and must not already be planned");
                continue;
            }
            if (serviceLoaderTargets.contains(target)) {
                assertFalse(plannedInstall.contains(target),
                        target + " is ServiceLoader-owned and must not be hard-wired in builtIn()");
                continue;
            }
            assertTrue(plannedInstall.contains(target),
                    target + " is INSTALLABLE in the catalog but no provider plans an INSTALL action");
        }
    }

    private static Set<SetupProfile> catalogProfiles() {
        Set<SetupProfile> profiles = EnumSet.noneOf(SetupProfile.class);
        SetupCatalog.builtIn().profiles().forEach(profile -> profiles.add(profile.profile()));
        return profiles;
    }

    private static Set<SetupTarget> plannedInstallTargets(Path temp) {
        Map<SetupProfile, InfrastructureSetupService> owners = new EnumMap<>(SetupProfile.class);
        owners.put(SetupProfile.MOBILE_IOS, InfrastructureSetupService.builtIn(
                SetupPlatform.MACOS, SetupArchitecture.X64));
        owners.put(SetupProfile.MOBILE_WINDOWS, InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64));
        InfrastructureSetupService host = InfrastructureSetupService.builtIn();
        Set<SetupTarget> planned = EnumSet.noneOf(SetupTarget.class);
        for (SetupProfile profile : catalogProfiles()) {
            if (HOST_ONLY_PROFILES.contains(profile) || SERVICE_LOADER_PROFILES.contains(profile)) {
                continue;
            }
            InfrastructureSetupService service = owners.getOrDefault(profile, host);
            SetupPlan plan = service.plan(managed(temp, profile));
            plan.actions().stream()
                    .filter(action -> action.kind() == SetupActionKind.INSTALL)
                    .forEach(action -> planned.add(action.target()));
        }
        return planned;
    }

    private static SetupOptions managed(Path temp, SetupProfile profile) {
        Path root = temp.resolve(profile.name());
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return SetupOptions.defaults(profile, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")))
                .withMode(SetupMode.MANAGED);
    }
}
