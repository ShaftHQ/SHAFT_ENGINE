package com.shaft.mcp;

/**
 * Optional nested mobile-session request object accepted by {@code driver_initialize} when
 * {@code engine} selects {@link ActiveEngine#MOBILE_NATIVE} or {@link ActiveEngine#MOBILE_WEB}
 * (design doc amendment A9): carries the union of the former {@code mobile_initialize_native} and
 * {@code mobile_initialize_web_emulation} tools' parameters. Every field is optional; unset fields
 * resolve from {@code SHAFT.Properties} exactly as the current mobile init tools do (blank/zero
 * values are passed straight through to {@link MobileService#initializeNative}/
 * {@link MobileService#initializeWebEmulation}, which already default them internally).
 *
 * @param targetUrl       web-emulation only: initial URL; blank leaves the browser on its startup page
 * @param browser         web-emulation only: browser name, defaults to CHROME
 * @param deviceName      web-emulation: Chrome/Edge emulated device name, defaults to Pixel 5;
 *                        native: emulator, simulator, or real device name
 * @param width           web-emulation only: custom device width; requires height
 * @param height          web-emulation only: custom device height; requires width
 * @param pixelRatio      web-emulation only: custom device pixel ratio
 * @param userAgent       web-emulation only: optional custom mobile user agent
 * @param headless        web-emulation only: whether the emulated browser should run headlessly
 * @param platformName    native only: Android or iOS
 * @param appiumServerUrl native only: Appium server URL; blank defaults to http://127.0.0.1:4723
 * @param automationName  native only: Appium automation name; blank selects UiAutomator2/XCUITest
 * @param platformVersion native only: optional platform version
 * @param udid            native only: optional device UDID
 * @param app             native only: optional app path or remote app URL
 * @param appPackage      native only: optional Android app package
 * @param appActivity     native only: optional Android app activity
 * @param bundleId        native only: optional iOS bundle identifier for installed apps
 */
public record McpMobileInitOptions(
        String targetUrl,
        String browser,
        String deviceName,
        Integer width,
        Integer height,
        Double pixelRatio,
        String userAgent,
        Boolean headless,
        String platformName,
        String appiumServerUrl,
        String automationName,
        String platformVersion,
        String udid,
        String app,
        String appPackage,
        String appActivity,
        String bundleId) {

    /** Every field unset; equivalent to omitting {@code mobileOptions} entirely. */
    static final McpMobileInitOptions EMPTY = new McpMobileInitOptions(
            null, null, null, null, null, null, null, null,
            null, null, null, null, null, null, null, null, null);

    private static String text(String value) {
        return value == null ? "" : value;
    }

    private static int intOrZero(Integer value) {
        return value == null ? 0 : value;
    }

    private static double doubleOrZero(Double value) {
        return value == null ? 0 : value;
    }

    private static boolean boolOrFalse(Boolean value) {
        return Boolean.TRUE.equals(value);
    }

    /**
     * Applies this options object to a {@link MobileService}'s web-emulation initializer.
     *
     * @param mobileService target mobile service
     * @return the session result
     */
    McpMobileSessionResult initializeWebEmulation(MobileService mobileService) {
        return mobileService.initializeWebEmulation(
                text(targetUrl), text(browser), text(deviceName),
                intOrZero(width), intOrZero(height), doubleOrZero(pixelRatio),
                text(userAgent), boolOrFalse(headless));
    }

    /**
     * Applies this options object to a {@link MobileService}'s native Appium initializer.
     *
     * @param mobileService target mobile service
     * @return the session result
     */
    McpMobileSessionResult initializeNative(MobileService mobileService) {
        return mobileService.initializeNative(
                text(platformName), text(deviceName), text(appiumServerUrl), text(automationName),
                text(platformVersion), text(udid), text(app), text(appPackage), text(appActivity), text(bundleId));
    }
}
