package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;

/**
 * Configuration properties interface for internal engine metadata in the SHAFT framework.
 * Exposes internal defaults such as the framework version, build timestamp, and the
 * version coordinates used to bootstrap the Allure 3 CLI at report-generation time.
 * These values are typically populated from build-time defaults, but they may also be
 * supplied or overridden through the configured OWNER property sources, such as system
 * properties or internal properties files.
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/internal.properties",
        "file:src/main/resources/properties/default/internal.properties",
        "classpath:internal.properties",
})
public interface Internal extends EngineProperties<Internal> {
    @Key("shaftEngineVersion")
    @DefaultValue("10.3.20260911")
    String shaftEngineVersion();

    @Key("watermarkImagePath")
    @DefaultValue("https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/shaft-engine/src/main/resources/images/shaft_white_bg.png")
    String watermarkImagePath();

    /**
     * Version of the Allure 3 npm CLI package used by managed report generation (#5801/#5815).
     * Keep this aligned with the Allure 3 CLI you expect in CI (independent of
     * {@code allure-bom} Java adapter coordinates), root POM {@code allure.cli.version}, and
     * {@code allure-cli/pom.xml}. SHAFT prefers a Maven-provisioned copy under
     * {@code ~/.m2/repository/allure/allure-cli/&lt;version&gt;/} (optionally unpacked from
     * {@code io.github.shafthq:allure-cli:&lt;version&gt;:zip}); otherwise it falls back to
     * {@code npx --yes allure@&lt;version&gt;} (or portable Node under {@code ~/.m2/repository/nodejs/}).
     * User {@code PATH} {@code allure} is never used. Update this value to upgrade the pinned CLI
     * across the engine, the {@code provision-allure-cli} / {@code provision-allure-cli-maven}
     * profiles, and the zip module. Latest releases:
     * <a href="https://github.com/allure-framework/allure3/releases">Allure3Releases</a>
     */
    @Key("allure3Version")
    @DefaultValue("3.17.0")
    String allure3Version();

    /**
     * Version of the portable Node.js LTS distribution that SHAFT downloads when neither
     * {@code node}/{@code npx} is available on {@code PATH}, or when provisioning the Maven-cached
     * Allure 3 CLI (#5801). The archive is cached in {@code ~/.m2/repository/nodejs/} so it is
     * only downloaded once per machine. Update this value here to upgrade the bundled Node.js
     * runtime. Latest LTS:
     * <a href="https://nodejs.org/en/about/previous-releases#looking-for-the-latest-release-of-a-version-branch">NodeJsLTS</a>
     */
    @Key("nodeLtsVersion")
    @DefaultValue("24.21.0")
    String nodeLtsVersion();

    /**
     * Appium server npm package version used by SHAFT MCP when it needs to bootstrap
     * a local Appium server without requiring administrator-installed tools.
     */
    @Key("appiumServerVersion")
    @DefaultValue("3.7.0")
    String appiumServerVersion();

    /**
     * Appium Inspector plugin npm package version used by SHAFT MCP for wrapped
     * mobile recording sessions.
     */
    @Key("appiumInspectorPluginVersion")
    @DefaultValue("2026.7.1")
    String appiumInspectorPluginVersion();

    /**
     * Appium UiAutomator2 driver npm package version used by SHAFT MCP for Android.
     */
    @Key("appiumUiAutomator2DriverVersion")
    @DefaultValue("8.6.3")
    String appiumUiAutomator2DriverVersion();

    /**
     * Appium XCUITest driver npm package version used by SHAFT MCP for iOS attach
     * and recording flows on macOS.
     */
    @Key("appiumXcuitestDriverVersion")
    @DefaultValue("12.12.0")
    String appiumXcuitestDriverVersion();

    /**
     * Android command-line tools build number used by SHAFT MCP when no Android SDK
     * tools are already available on the machine.
     */
    @Key("androidCommandLineToolsVersion")
    @DefaultValue("16111833")
    String androidCommandLineToolsVersion();

    /**
     * Default Android API level proposed for SHAFT-managed emulator creation.
     */
    @Key("androidEmulatorApiLevel")
    @DefaultValue("36")
    int androidEmulatorApiLevel();

    /**
     * Default Android virtual device profile proposed for SHAFT-managed emulators.
     */
    @Key("androidEmulatorDeviceProfile")
    @DefaultValue("pixel_8")
    String androidEmulatorDeviceProfile();

    /**
     * Default Android system image tag proposed for SHAFT-managed emulators.
     */
    @Key("androidEmulatorImageTag")
    @DefaultValue("google_apis")
    String androidEmulatorImageTag();

    /**
     * Default Android emulator memory size in MB.
     */
    @Key("androidEmulatorRamMb")
    @DefaultValue("4096")
    int androidEmulatorRamMb();

    /**
     * Default Android emulator CPU core count.
     */
    @Key("androidEmulatorCores")
    @DefaultValue("2")
    int androidEmulatorCores();

    /**
     * Google Analytics 4 Measurement ID used for anonymous telemetry.
     * Found in the GA4 console under Admin &gt; Data Streams &gt; choose your stream &gt; Measurement ID.
     * Override in {@code internal.properties} to route telemetry to a different GA4 property.
     */
    @Key("ga4MeasurementId")
    @DefaultValue("G-4L9L79WZBV")
    String ga4MeasurementId();

    /**
     * Google Analytics 4 API Secret used for the Measurement Protocol.
     * Found in the GA4 console under Admin &gt; Data Streams &gt; choose your stream &gt;
     * Measurement Protocol &gt; Create.
     * Override in {@code internal.properties} to supply a fresh secret after rotation.
     * <p>
     * <strong>Note</strong>: if you override this with your own GA4 property's secret,
     * rotate the secret regularly via the GA4 console to prevent abuse; the new value
     * can then be supplied via {@code internal.properties} without a SHAFT release.
     */
    @Key("ga4ApiSecret")
    @DefaultValue("nzK22pHiTZWu8FGgvDVtnA")
    String ga4ApiSecret();
}
