package com.shaft.mcp;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Static, platform-specific pairing instructions and known-limitation warnings for mobile API
 * capture. These are informational text only -- none of it requires (or can rely on) a live
 * Android SDK/adb or Xcode/simctl toolchain, since a caller may be pairing a real device or an
 * emulator/simulator this MCP server host has no visibility into.
 */
final class MobilePairingGuidance {
    private MobilePairingGuidance() {
    }

    /**
     * Builds pairing guidance for one capture session.
     *
     * @param platform "Android", "iOS", or an unrecognized caller-supplied label
     * @param proxyPort loopback port the MITM proxy is listening on
     * @return ordered, human-readable guidance and limitation warnings
     */
    static List<String> forPlatform(String platform, int proxyPort) {
        String normalized = platform == null ? "" : platform.trim().toLowerCase(Locale.ROOT);
        List<String> guidance = new ArrayList<>();
        if (normalized.equals("android")) {
            guidance.add("Android emulator: run `adb reverse tcp:" + proxyPort + " tcp:" + proxyPort
                    + "` then set the device's HTTP proxy to 127.0.0.1:" + proxyPort
                    + " (for example `adb shell settings put global http_proxy 127.0.0.1:" + proxyPort + "`).");
            guidance.add("Android 7+ (API 24+) ignores user-installed CAs for apps that do not explicitly opt in "
                    + "via a network security config; only such apps, or a rooted device with the CA installed "
                    + "into the system trust store, can be intercepted.");
        } else if (normalized.equals("ios")) {
            guidance.add("iOS Simulator: point the app under test's HTTP(S) proxy at 127.0.0.1:" + proxyPort
                    + ", then trust the CA certificate under Settings > General > About > Certificate Trust "
                    + "Settings.");
        } else {
            guidance.add("Unrecognized platform \"" + platform + "\"; pass \"Android\" or \"iOS\" for "
                    + "platform-specific pairing guidance.");
        }
        guidance.add("Apps using certificate pinning will reject this proxy's impersonated certificate "
                + "regardless of CA trust; those hosts must be added to an internal pass-through list so their "
                + "traffic tunnels uncaptured instead of breaking the app (code-level API for now, not yet "
                + "exposed as an MCP tool).");
        guidance.add("This proxy binds to 127.0.0.1 only. Real (non-emulator/simulator) devices need a USB "
                + "tunnel such as `adb reverse` (Android) or `iproxy` (iOS) to reach it; direct LAN pairing is "
                + "not supported in this phase.");
        guidance.add("Hybrid/webview traffic still goes through this MITM proxy today; the existing "
                + "browser capture path (capture_start, driven by Selenium DevTools) does not attach to "
                + "Appium's WEBVIEW_* contexts (mobile_get_contexts / mobile_switch_context), since Appium's "
                + "mobile drivers do not implement Selenium's HasDevTools. A CA-free CDP bridge for webview "
                + "contexts is a possible future improvement, not available in this phase.");
        return List.copyOf(guidance);
    }
}
