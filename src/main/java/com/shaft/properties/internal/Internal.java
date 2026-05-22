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
    @DefaultValue("10.2.20260522")
    String shaftEngineVersion();

    @Key("watermarkImagePath")
    @DefaultValue("https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/src/main/resources/images/shaft_white_bg.png")
    String watermarkImagePath();

    /**
     * Version of the Allure 3 npm package used when the CLI is not already on {@code PATH}.
     * SHAFT invokes {@code npx --yes allure@<version>} to download and cache the package
     * automatically.  Update this value here to upgrade the bundled CLI across the engine
     * without changing {@code AllureManager} or any CI script. Use this url for the latest version <a href="https://github.com/allure-framework/allure3/releases">Allure3Releases</a>
     */
    @Key("allure3Version")
    @DefaultValue("3.8.2")
    String allure3Version();

    /**
     * Version of the portable Node.js LTS distribution that SHAFT downloads when neither
     * {@code allure} nor {@code npx} is available on {@code PATH}.  The archive is cached
     * in {@code ~/.m2/repository/nodejs/} so it is only downloaded once per machine.
     * Update this value here to upgrade the bundled Node.js runtime. Use this url for the latest version <a href="https://nodejs.org/en/about/previous-releases#looking-for-the-latest-release-of-a-version-branch">NodeJsLTS</a>
     *
     */
    @Key("nodeLtsVersion")
    @DefaultValue("24.15.0")
    String nodeLtsVersion();

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
