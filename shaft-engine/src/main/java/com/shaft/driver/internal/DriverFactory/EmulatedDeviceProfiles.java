package com.shaft.driver.internal.DriverFactory;

import java.util.Locale;
import java.util.Map;
import java.util.Optional;

/**
 * Shared table of emulated-device profiles that SHAFT resolves to explicit device metrics instead
 * of passing a {@code deviceName} to the browser.
 *
 * <p>Chromium's built-in DevTools emulated-device list drifts across releases (Chrome 143+ removed
 * "Pixel 5", SHAFT's long-standing default), and an unknown {@code deviceName} fails session
 * creation with the opaque "entry 0 of 'firstMatch' is invalid". Devices in this table always use
 * explicit {@code deviceMetrics} + user agent, so existing configurations keep working regardless
 * of the browser's own device list. {@code OptionsManager} and {@code MobileTraceMetadata} both
 * read this single table so the emulation options and the recorded trace metadata can never
 * disagree.</p>
 */
public final class EmulatedDeviceProfiles {
    /**
     * One resolved emulated-device profile.
     *
     * @param deviceName canonical device name
     * @param width viewport width in CSS pixels
     * @param height viewport height in CSS pixels
     * @param pixelRatio device scale factor
     * @param userAgent emulated user-agent string
     */
    public record Profile(String deviceName, int width, int height, double pixelRatio, String userAgent) {
        /**
         * Returns the viewport as {@code widthxheight}.
         *
         * @return viewport string
         */
        public String viewport() {
            return width + "x" + height;
        }
    }

    private static final Map<String, Profile> PROFILES = Map.of(
            "pixel 5", new Profile("Pixel 5", 393, 851, 2.75,
                    "Mozilla/5.0 (Linux; Android 11; Pixel 5) AppleWebKit/537.36 (KHTML, like Gecko) "
                            + "Chrome/91.0.4472.77 Mobile Safari/537.36"));

    private EmulatedDeviceProfiles() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Looks up a known emulated-device profile by (case-insensitive) device name.
     *
     * @param deviceName configured mobileEmulationDeviceName
     * @return the pinned profile, or empty when the device is not in the table
     */
    public static Optional<Profile> of(String deviceName) {
        if (deviceName == null || deviceName.isBlank()) {
            return Optional.empty();
        }
        return Optional.ofNullable(PROFILES.get(deviceName.trim().toLowerCase(Locale.ROOT)));
    }

    /**
     * Builds the up-front warning logged when an unpinned device name is passed through to the
     * browser, replacing chromedriver's opaque session-creation failure with an actionable hint.
     *
     * @param deviceName configured mobileEmulationDeviceName
     * @return human-readable warning text
     */
    public static String driftWarning(String deviceName) {
        return "Mobile emulation device \"" + deviceName + "\" is not pinned by SHAFT and will be "
                + "resolved by the browser's own DevTools device list. If session creation fails with "
                + "\"entry 0 of 'firstMatch' is invalid\", the browser no longer ships this device; "
                + "switch to custom metrics (mobileEmulation.isCustomDevice=true with width/height/"
                + "pixelRatio/userAgent) or a pinned device such as Pixel 5.";
    }
}
