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
    @DefaultValue("10.2.20260410")
    String shaftEngineVersion();

    @Key("watermarkImagePath")
    @DefaultValue("https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/src/main/resources/images/shaft_white_bg.png")
    String watermarkImagePath();

    /**
     * Returns the configured Allure version string.
     *
     * @return the configured Allure version value
     * @deprecated This property is no longer used by the SHAFT framework internally.
     *             It has been superseded by {@link #allure3Version()} and {@link #nodeLtsVersion()}.
     *             Retained for one release cycle to preserve backward compatibility with downstream
     *             code that still references {@code SHAFT.Properties.internal.allureVersion()}.
     *             The default value is kept at the last Allure 2 release used by SHAFT so that
     *             existing downstream consumers that read this property are not silently broken.
     */
    @Deprecated
    @Key("allureVersion")
    @DefaultValue("2.29.0")
    String allureVersion();

    /**
     * Version of the Allure 3 npm package used when the CLI is not already on {@code PATH}.
     * SHAFT invokes {@code npx --yes allure@<version>} to download and cache the package
     * automatically.  Update this value here to upgrade the bundled CLI across the engine
     * without changing {@code AllureManager} or any CI script.
     */
    @Key("allure3Version")
    @DefaultValue("3.3.1")
    String allure3Version();

    /**
     * Version of the portable Node.js LTS distribution that SHAFT downloads when neither
     * {@code allure} nor {@code npx} is available on {@code PATH}.  The archive is cached
     * in {@code ~/.m2/repository/nodejs/} so it is only downloaded once per machine.
     * Update this value here to upgrade the bundled Node.js runtime.
     */
    @Key("nodeLtsVersion")
    @DefaultValue("20.19.1")
    String nodeLtsVersion();
}
