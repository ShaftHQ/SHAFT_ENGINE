package com.shaft.infrastructure;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;

/** Release-coupled planner for the managed Appium and Android emulator toolchain. */
public final class AndroidSetupPlanner {
    public static final String APPIUM_VERSION = "3.6.0";
    public static final String INSPECTOR_PLUGIN_VERSION = "2026.7.1";
    public static final String UIAUTOMATOR2_VERSION = "8.2.2";
    public static final String APPIUM_LOCK_SHA256 =
            "4da109a812861e2a1fc26792e04c3128242e5b17e4c2d97e26a20e91ef8fbe98";
    public static final String COMMAND_LINE_TOOLS_VERSION = "15859902";
    public static final String PLATFORM_TOOLS_VERSION = "37.0.1";
    public static final String EMULATOR_VERSION = "37.1.11";
    public static final String ANDROID_PLATFORM_REVISION = "2";
    public static final String BUILD_TOOLS_VERSION = "36.0.0";
    public static final String SYSTEM_IMAGE_REVISION = "7";
    public static final int API_LEVEL = 36;
    public static final String DEVICE_PROFILE = "pixel_8";
    public static final String IMAGE_TAG = "google_apis";
    public static final int RAM_MB = 4096;
    public static final int CORES = 2;
    public static final int APPIUM_PORT = 4723;
    public static final String ANDROID_SDK_LICENSE = "android-sdk-license";

    private static final String APPIUM_SHA256 =
            "ea722c272d117ffac7e265e6565651f3835efbcea670f82a16f4e75de120b76e";
    private static final String INSPECTOR_SHA256 =
            "fcaf8d9434a9809fc0c5df16902b87b6c5920bb8f446f05e85ab77301ed9e99d";
    private static final String UIAUTOMATOR2_SHA256 =
            "a53c05850eaf08372672dc425298e77a2094f6334bb2b1ac220b705255483a22";

    private AndroidSetupPlanner() { }

    /** Creates the exact default Android plan shipped with this release. */
    public static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode) {
        return plan(platform, architecture, mode, AndroidSetupRequest.defaults());
    }

    /** Creates an exact Android plan whose AVD/runtime request is digest-bound. */
    public static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode,
                                 AndroidSetupRequest request) {
        requireSupportedHost(platform, architecture);
        AndroidSetupRequest resolved = request.resolve(architecture);
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        String lock = "sha256:" + APPIUM_LOCK_SHA256;
        SetupAction node = ReportingSetupPlanner.plan(platform, architecture, mode).actions().getFirst();
        SetupAction appium = new SetupAction(SetupTarget.APPIUM_SERVER, kind, APPIUM_VERSION,
                URI.create("https://registry.npmjs.org/appium/-/appium-" + APPIUM_VERSION + ".tgz"),
                "sha256:" + APPIUM_SHA256, lock, false, Set.of());
        SetupAction inspector = new SetupAction(SetupTarget.APPIUM_INSPECTOR_PLUGIN, kind,
                INSPECTOR_PLUGIN_VERSION, URI.create("https://registry.npmjs.org/appium-inspector-plugin/-/"
                + "appium-inspector-plugin-" + INSPECTOR_PLUGIN_VERSION + ".tgz"),
                "sha256:" + INSPECTOR_SHA256, lock, false, Set.of());
        SetupAction driver = new SetupAction(SetupTarget.APPIUM_UIAUTOMATOR2_DRIVER, kind,
                UIAUTOMATOR2_VERSION, URI.create("https://registry.npmjs.org/appium-uiautomator2-driver/-/"
                + "appium-uiautomator2-driver-" + UIAUTOMATOR2_VERSION + ".tgz"),
                "sha256:" + UIAUTOMATOR2_SHA256, lock, false, Set.of());
        String abi = resolved.abi();
        String image = "system-images;android-" + API_LEVEL + ';' + IMAGE_TAG + ';' + abi;
        String packages = String.join(",", "cmdline-tools:" + COMMAND_LINE_TOOLS_VERSION,
                "platform-tools@" + PLATFORM_TOOLS_VERSION, "emulator@" + EMULATOR_VERSION,
                "platforms;android-" + API_LEVEL + '@' + ANDROID_PLATFORM_REVISION,
                "build-tools;" + BUILD_TOOLS_VERSION + '@' + BUILD_TOOLS_VERSION,
                image + '@' + SYSTEM_IMAGE_REVISION);
        AndroidArchive archive = archive(platform, architecture);
        SetupAction sdk = new SetupAction(SetupTarget.ANDROID_SDK, kind, packages, archive.source(),
                "sha256:" + archive.sha256(), false, Set.of(ANDROID_SDK_LICENSE));
        String avd = resolved.avdName();
        String avdSpec = String.join(",", "avd=" + avd, "device=" + resolved.deviceProfile(),
                "api=" + resolved.apiLevel(), "tag=" + resolved.imageTag(), "abi=" + abi,
                "ramMb=" + resolved.ramMb(), "cores=" + resolved.cores(),
                "port=" + resolved.appiumPort());
        SetupAction emulator = new SetupAction(SetupTarget.ANDROID_EMULATOR, kind, avdSpec,
                URI.create("urn:shaft:android-avd:" + avd), sha256(avdSpec), false,
                Set.of(ANDROID_SDK_LICENSE));
        return SetupPlan.create(SetupProfile.MOBILE_ANDROID, platform, architecture, mode,
                List.of(node, appium, inspector, driver, sdk, emulator));
    }

    private static AndroidArchive archive(SetupPlatform platform, SetupArchitecture architecture) {
        String file;
        String checksum;
        switch (platform) {
            case WINDOWS -> {
                file = "commandlinetools-win-15859902_latest.zip";
                checksum = "90ae805d20434428bffcb699c290860f19bb5f66a67e6b330067e3de801fb04a";
            }
            case LINUX -> {
                file = "commandlinetools-linux-15859902_latest.zip";
                checksum = "4e4c464f145a7512b57d088ac6c278c03c9eea610886b35a5e0804e74eedf583";
            }
            case MACOS -> {
                if (architecture == SetupArchitecture.ARM64) {
                    file = "commandlinetools-mac_arm64-15859902_latest.zip";
                    checksum = "835b62a26162b229b441d1f6d4680383815a270809eb33522c0d480fa5002c4e";
                } else {
                    file = "commandlinetools-mac_x86_64-15859902_latest.zip";
                    checksum = "c5a6378ab5cf7e0d5701921405115befff13e9ff7417fb588389338f8bd050f3";
                }
            }
            default -> throw new IllegalArgumentException("Unsupported Android host platform: " + platform);
        }
        return new AndroidArchive(URI.create("https://dl.google.com/android/repository/" + file), checksum);
    }

    private static void requireSupportedHost(SetupPlatform platform, SetupArchitecture architecture) {
        if (platform == SetupPlatform.WINDOWS && architecture == SetupArchitecture.ARM64) {
            throw new IllegalArgumentException("Android Emulator is not supported on Windows ARM64.");
        }
    }

    private static String sha256(String value) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256")
                    .digest(value.getBytes(StandardCharsets.UTF_8));
            return "sha256:" + HexFormat.of().formatHex(digest);
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    private record AndroidArchive(URI source, String sha256) { }
}
