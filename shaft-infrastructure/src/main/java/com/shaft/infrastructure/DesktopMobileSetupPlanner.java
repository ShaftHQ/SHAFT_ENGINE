package com.shaft.infrastructure;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Release-coupled plans for existing iOS Simulator and Windows desktop hosts. */
final class DesktopMobileSetupPlanner {
    static final String XCUITEST_VERSION = "12.1.1";
    static final String WINDOWS_DRIVER_VERSION = "6.1.0";
    static final String WINAPPDRIVER_VERSION = "1.2.1";
    private static final Pattern IOS_SPEC = Pattern.compile(
            "udid=(existing|[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}),appiumPort=([0-9]+)");
    private static final Pattern WINDOWS_SPEC = Pattern.compile(
            "wad=" + Pattern.quote(WINAPPDRIVER_VERSION) + ",appiumPort=([0-9]+)");
    static final String IOS_LOCK_SHA256 =
            "sha256:27167be51ad4a85d8310ff30eab59eabb922cae796dd623af9514dc2a27b595e";
    static final String WINDOWS_LOCK_SHA256 =
            "sha256:3d49b003cebb6eda9a947b91070b86eba64642d27c6c64c961f6a5e10fc68050";

    private static final String APPIUM_SHA256 =
            "ea722c272d117ffac7e265e6565651f3835efbcea670f82a16f4e75de120b76e";
    private static final String INSPECTOR_SHA256 =
            "fcaf8d9434a9809fc0c5df16902b87b6c5920bb8f446f05e85ab77301ed9e99d";
    private static final String XCUITEST_SHA256 =
            "cd1e3c0acf6f799c5458ecae028994f29504dffd325dbef5331afd40c1b68741";
    private static final String WINDOWS_DRIVER_SHA256 =
            "f16ed78b40425fb2f6cbbea0693d416733c5c5d9a4f4f46bc9b60447ca36458c";
    private static final String WINAPPDRIVER_SHA256 =
            "a76a8f4e44b29bad331acf6b6c248fcc65324f502f28826ad2acd5f3c80857fe";

    private DesktopMobileSetupPlanner() {
        throw new IllegalStateException("Utility class");
    }

    static SetupPlan ios(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode,
                         SetupSelection selection) {
        if (platform != SetupPlatform.MACOS) {
            throw new IllegalArgumentException("iOS Simulator setup requires macOS.");
        }
        String simulator = selectedSimulator(selection);
        int port = selectedPort(selection);
        String simulatorSpec = "udid=" + simulator + ",appiumPort=" + port;
        SetupActionKind kind = packageKind(mode);
        return SetupPlan.create(SetupProfile.MOBILE_IOS, platform, architecture, mode, List.of(
                ReportingSetupPlanner.plan(platform, architecture, mode).actions().getFirst(),
                appium(kind, IOS_LOCK_SHA256),
                inspector(kind, IOS_LOCK_SHA256),
                new SetupAction(SetupTarget.APPIUM_XCUITEST_DRIVER, kind, XCUITEST_VERSION,
                        URI.create("https://registry.npmjs.org/appium-xcuitest-driver/-/"
                                + "appium-xcuitest-driver-" + XCUITEST_VERSION + ".tgz"),
                        "sha256:" + XCUITEST_SHA256, 715_958, IOS_LOCK_SHA256, false, Set.of()),
                diagnostic(SetupTarget.XCODE, "14.3+", URI.create("urn:shaft:host:xcode")),
                diagnostic(SetupTarget.IOS_SIMULATOR, simulatorSpec,
                        URI.create("urn:shaft:ios-simulator:" + simulator + ":port:" + port))));
    }

    static SetupPlan windows(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode,
                             SetupSelection selection) {
        if (platform != SetupPlatform.WINDOWS) {
            throw new IllegalArgumentException("Windows desktop setup requires Windows.");
        }
        requireOnly(selection, "port_");
        int port = selectedPort(selection);
        SetupActionKind kind = packageKind(mode);
        return SetupPlan.create(SetupProfile.MOBILE_WINDOWS, platform, architecture, mode, List.of(
                ReportingSetupPlanner.plan(platform, architecture, mode).actions().getFirst(),
                appium(kind, WINDOWS_LOCK_SHA256),
                inspector(kind, WINDOWS_LOCK_SHA256),
                new SetupAction(SetupTarget.APPIUM_WINDOWS_DRIVER, kind, WINDOWS_DRIVER_VERSION,
                        URI.create("https://registry.npmjs.org/appium-windows-driver/-/"
                                + "appium-windows-driver-" + WINDOWS_DRIVER_VERSION + ".tgz"),
                        "sha256:" + WINDOWS_DRIVER_SHA256, 125_443, WINDOWS_LOCK_SHA256, false, Set.of()),
                new SetupAction(SetupTarget.WINAPPDRIVER, SetupActionKind.DIAGNOSE,
                        "wad=" + WINAPPDRIVER_VERSION + ",appiumPort=" + port,
                        URI.create("https://github.com/microsoft/WinAppDriver/releases/download/v1.2.1/"
                                + "WindowsApplicationDriver_1.2.1.msi"),
                "sha256:" + WINAPPDRIVER_SHA256, 3_932_160, false, Set.of())));
    }

    static String requestedSimulator(SetupPlan plan) {
        if (plan.profile() != SetupProfile.MOBILE_IOS) {
            throw new IllegalArgumentException("Simulator identity is only defined for iOS plans.");
        }
        return selectedSimulator(selectionFromPlan(plan));
    }

    static int requestedAppiumPort(SetupPlan plan) {
        return selectedPort(selectionFromPlan(plan));
    }

    static SetupSelection selectionFromPlan(SetupPlan plan) {
        java.util.Objects.requireNonNull(plan, "plan");
        return switch (plan.profile()) {
            case MOBILE_IOS -> iosSelection(requireSingleAction(plan, SetupTarget.IOS_SIMULATOR).version());
            case MOBILE_WINDOWS -> windowsSelection(requireSingleAction(plan, SetupTarget.WINAPPDRIVER).version());
            default -> throw new IllegalArgumentException("Desktop-mobile plan requires an iOS or Windows profile.");
        };
    }

    private static SetupSelection iosSelection(String version) {
        Matcher match = IOS_SPEC.matcher(version);
        if (!match.matches()) throw new IllegalArgumentException("iOS simulator plan metadata is invalid.");
        java.util.ArrayList<String> components = new java.util.ArrayList<>();
        if (!match.group(1).equals("existing")) {
            components.add("simulator_" + match.group(1).replace('-', '_'));
        }
        addSelectedPort(components, match.group(2));
        return new SetupSelection(components);
    }

    private static SetupSelection windowsSelection(String version) {
        Matcher match = WINDOWS_SPEC.matcher(version);
        if (!match.matches()) throw new IllegalArgumentException("Windows desktop plan metadata is invalid.");
        java.util.ArrayList<String> components = new java.util.ArrayList<>();
        addSelectedPort(components, match.group(1));
        return new SetupSelection(components);
    }

    private static void addSelectedPort(List<String> components, String value) {
        int port;
        try {
            port = Integer.parseInt(value);
        } catch (NumberFormatException invalid) {
            throw new IllegalArgumentException("Appium port must be a decimal integer.", invalid);
        }
        if (port != AndroidSetupPlanner.APPIUM_PORT) components.add("port_" + port);
    }

    private static SetupAction requireSingleAction(SetupPlan plan, SetupTarget target) {
        List<SetupAction> actions = plan.actions().stream().filter(action -> action.target() == target).toList();
        if (actions.size() != 1) {
            throw new IllegalArgumentException("Desktop-mobile plan must contain exactly one " + target + " action.");
        }
        return actions.getFirst();
    }

    private static SetupAction appium(SetupActionKind kind, String lock) {
        return new SetupAction(SetupTarget.APPIUM_SERVER, kind, AndroidSetupPlanner.APPIUM_VERSION,
                URI.create("https://registry.npmjs.org/appium/-/appium-"
                        + AndroidSetupPlanner.APPIUM_VERSION + ".tgz"),
                "sha256:" + APPIUM_SHA256, lock, false, Set.of());
    }

    private static SetupAction inspector(SetupActionKind kind, String lock) {
        return new SetupAction(SetupTarget.APPIUM_INSPECTOR_PLUGIN, kind,
                AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION,
                URI.create("https://registry.npmjs.org/appium-inspector-plugin/-/appium-inspector-plugin-"
                        + AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION + ".tgz"),
                "sha256:" + INSPECTOR_SHA256, lock, false, Set.of());
    }

    private static SetupAction diagnostic(SetupTarget target, String version, URI source) {
        return new SetupAction(target, SetupActionKind.DIAGNOSE, version, source,
                sha256(source + "\0" + version), false, Set.of());
    }

    private static SetupActionKind packageKind(SetupMode mode) {
        return mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
    }

    private static String selectedSimulator(SetupSelection selection) {
        List<String> simulators = selection.components().stream()
                .filter(component -> component.startsWith("simulator_"))
                .toList();
        if (simulators.size() > 1) {
            throw new IllegalArgumentException("Select at most one iOS simulator UDID.");
        }
        requireOnly(selection, "simulator_", "port_");
        if (simulators.isEmpty()) return "existing";
        String udid = simulators.getFirst().substring("simulator_".length()).replace('_', '-');
        if (!udid.matches("[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}")) {
            throw new IllegalArgumentException("iOS simulator UDID has an invalid format.");
        }
        return udid;
    }

    private static int selectedPort(SetupSelection selection) {
        List<String> ports = selection.components().stream()
                .filter(component -> component.startsWith("port_"))
                .toList();
        if (ports.size() > 1) {
            throw new IllegalArgumentException("Select at most one Appium port.");
        }
        int port = AndroidSetupPlanner.APPIUM_PORT;
        if (!ports.isEmpty()) {
            try {
                port = Integer.parseInt(ports.getFirst().substring("port_".length()));
            } catch (NumberFormatException invalid) {
                throw new IllegalArgumentException("Appium port must be a decimal integer.", invalid);
            }
        }
        if (port < 1024 || port > 65535) {
            throw new IllegalArgumentException("Appium port must be between 1024 and 65535.");
        }
        return port;
    }

    private static void requireOnly(SetupSelection selection, String... prefixes) {
        for (String component : selection.components()) {
            if (java.util.Arrays.stream(prefixes).noneMatch(component::startsWith)) {
                throw new IllegalArgumentException("Unsupported desktop-mobile component: " + component);
            }
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
}
