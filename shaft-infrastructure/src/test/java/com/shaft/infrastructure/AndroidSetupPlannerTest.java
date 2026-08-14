package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AndroidSetupPlannerTest {
    @Test
    void defaultRequestPinsEveryHostArtifactAndAndroidPackage() {
        assertSdkArchive(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                "commandlinetools-win-15859902_latest.zip",
                "90ae805d20434428bffcb699c290860f19bb5f66a67e6b330067e3de801fb04a");
        assertSdkArchive(SetupPlatform.LINUX, SetupArchitecture.X64,
                "commandlinetools-linux-15859902_latest.zip",
                "4e4c464f145a7512b57d088ac6c278c03c9eea610886b35a5e0804e74eedf583");
        assertSdkArchive(SetupPlatform.LINUX, SetupArchitecture.ARM64,
                "commandlinetools-linux-15859902_latest.zip",
                "4e4c464f145a7512b57d088ac6c278c03c9eea610886b35a5e0804e74eedf583");
        assertSdkArchive(SetupPlatform.MACOS, SetupArchitecture.X64,
                "commandlinetools-mac_x86_64-15859902_latest.zip",
                "c5a6378ab5cf7e0d5701921405115befff13e9ff7417fb588389338f8bd050f3");
        assertSdkArchive(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                "commandlinetools-mac_arm64-15859902_latest.zip",
                "835b62a26162b229b441d1f6d4680383815a270809eb33522c0d480fa5002c4e");
        assertThrows(IllegalArgumentException.class, () -> AndroidSetupPlanner.plan(SetupPlatform.WINDOWS,
                SetupArchitecture.ARM64, SetupMode.MANAGED, AndroidSetupRequest.defaults()));
    }

    @Test
    void requestIsNormalizedAndBoundIntoTheReviewedPlan() {
        AndroidSetupRequest defaults = AndroidSetupRequest.defaults();
        AndroidSetupRequest custom = new AndroidSetupRequest(36, "pixel_8", "google_apis", "x86_64",
                "custom_avd", 6144, 4, 4823);

        SetupPlan defaultPlan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, defaults);
        SetupPlan customPlan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, custom);

        assertNotEquals(defaultPlan.digest(), customPlan.digest());
        assertTrue(customPlan.actions().getLast().version().contains("avd=custom_avd"));
        assertTrue(customPlan.actions().getLast().version().contains("ramMb=6144"));
        assertTrue(customPlan.actions().getLast().version().contains("cores=4"));
        assertTrue(customPlan.actions().getLast().version().contains("port=4823"));
        assertEquals(custom, AndroidSetupRequest.fromPlan(customPlan));
        assertEquals(custom, AndroidSetupRequest.fromSelection(custom.toSelection()));
        assertThrows(IllegalArgumentException.class, () -> new AndroidSetupRequest(
                36, "pixel_8", "google_apis", "x86_64", "../escape", 4096, 2, 4723));
        assertThrows(IllegalArgumentException.class, () -> new AndroidSetupRequest(
                35, "pixel_8", "google_apis", "x86_64", "safe", 4096, 2, 4723));
        SetupAction emulator = customPlan.actions().getLast();
        SetupAction injected = new SetupAction(emulator.target(), emulator.kind(),
                emulator.version() + ",extra=unapproved", emulator.source(), emulator.checksum(),
                emulator.dependencyLockChecksum(), emulator.privileged(), emulator.requiredLicenses());
        SetupPlan injectedPlan = SetupPlan.create(customPlan.profile(), customPlan.platform(),
                customPlan.architecture(), customPlan.mode(), List.of(customPlan.actions().get(0),
                customPlan.actions().get(1), customPlan.actions().get(2), customPlan.actions().get(3),
                customPlan.actions().get(4), injected));
        assertThrows(IllegalArgumentException.class, () -> AndroidSetupRequest.fromPlan(injectedPlan));
    }

    @Test
    void coordinatorAcceptsTheTypedAndroidRequestWithoutWeakeningGenericProviders(
            @org.junit.jupiter.api.io.TempDir java.nio.file.Path temp) {
        ShaftCachePaths paths = new ShaftCachePaths(temp.resolve("cache").toAbsolutePath(),
                temp.resolve("data").toAbsolutePath(), temp.resolve("cache/downloads").toAbsolutePath(),
                temp.resolve("data/tools").toAbsolutePath(), temp.resolve("data/state").toAbsolutePath(),
                temp.resolve("data/receipts").toAbsolutePath());
        AndroidSetupRequest request = new AndroidSetupRequest(36, "pixel_8", "google_apis", "x86_64",
                "typed_avd", 8192, 6, 4923);
        SetupOptions options = SetupOptions.defaults(SetupProfile.MOBILE_ANDROID, paths)
                .withMode(SetupMode.MANAGED);

        SetupPlan plan = InfrastructureSetupService.builtIn(SetupPlatform.LINUX, SetupArchitecture.X64)
                .plan(options, request);

        assertEquals(request, AndroidSetupRequest.fromPlan(plan));
        assertThrows(IllegalArgumentException.class, () -> InfrastructureSetupService
                .builtIn(SetupPlatform.LINUX, SetupArchitecture.X64)
                .plan(SetupOptions.defaults(SetupProfile.REPORTING, paths), request));
    }

    @Test
    void bundledAppiumLockMatchesEveryPlannedPackage() throws Exception {
        byte[] packageJson;
        byte[] lock;
        try (var packageInput = getClass().getResourceAsStream("/com/shaft/infrastructure/appium/package.json");
             var lockInput = getClass().getResourceAsStream("/com/shaft/infrastructure/appium/package-lock.json")) {
            packageJson = java.util.Objects.requireNonNull(packageInput).readAllBytes();
            lock = java.util.Objects.requireNonNull(lockInput).readAllBytes();
        }
        String manifest = new String(packageJson, StandardCharsets.UTF_8);
        String canonicalLock = new String(lock, StandardCharsets.UTF_8)
                .replace("\r\n", "\n").replace('\r', '\n');
        assertTrue(manifest.contains("\"appium\": \"3.6.0\""));
        assertTrue(manifest.contains("\"appium-inspector-plugin\": \"2026.7.1\""));
        assertTrue(manifest.contains("\"appium-uiautomator2-driver\": \"8.2.2\""));
        assertTrue(canonicalLock.contains("\"node_modules/appium\""));
        assertTrue(canonicalLock.contains("\"node_modules/appium-inspector-plugin\""));
        assertTrue(canonicalLock.contains("\"node_modules/appium-uiautomator2-driver\""));
        String digest = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
                .digest(canonicalLock.getBytes(StandardCharsets.UTF_8)));
        assertEquals(AndroidSetupPlanner.APPIUM_LOCK_SHA256, digest);
    }

    private static void assertSdkArchive(SetupPlatform platform, SetupArchitecture architecture,
                                         String fileName, String checksum) {
        SetupPlan plan = AndroidSetupPlanner.plan(platform, architecture, SetupMode.MANAGED,
                AndroidSetupRequest.defaults());
        SetupAction sdk = plan.actions().stream().filter(action -> action.target() == SetupTarget.ANDROID_SDK)
                .findFirst().orElseThrow();
        assertTrue(sdk.source().toString().endsWith(fileName));
        assertEquals("sha256:" + checksum, sdk.checksum());
        assertTrue(sdk.version().contains("platform-tools@37.0.1"));
        assertTrue(sdk.version().contains("emulator@37.1.11"));
        assertTrue(sdk.version().contains("platforms;android-36@2"));
        assertTrue(sdk.version().contains("build-tools;36.0.0@36.0.0"));
        assertTrue(sdk.version().contains("system-images;android-36;google_apis;"));
        assertTrue(sdk.version().contains("@7"));
        assertTrue(sdk.requiredLicenses().contains(AndroidSetupPlanner.ANDROID_SDK_LICENSE));
        List<SetupAction> appiumActions = plan.actions().subList(1, 4);
        assertTrue(appiumActions.stream().allMatch(action -> action.dependencyLockChecksum()
                .equals("sha256:" + AndroidSetupPlanner.APPIUM_LOCK_SHA256)));
    }
}
